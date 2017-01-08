{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils.Types where

import Utils.Abstract
import Utils.Position
import Utils.Show
import Utils.Verbose
import Utils.Eval

import System.Console.ANSI

import qualified Data.Map as M
import Data.Maybe
import Data.List

import Control.Monad.Except
import Control.Monad.State

import Control.Lens hiding (Empty)

type SymbolsSt = M.Map Ident SymbolInfo
type CheckSt = ExceptT String (StateT InfoSt CompilerOptsM)

data DefPlace = Global | InBlock Block

instance Show DefPlace where
    show Global = "Global"
    show (InBlock block) = "Block at " ++ show (block ^. pos)

instance Eq DefPlace where
    Global == Global = True
    InBlock b1 == InBlock b2 = b1 `sameBlock` b2
    _ == _ = False

data InfoSt = InfoSt
    { _symbols :: SymbolsSt
    , _wholeProgram :: Program
    }

data SymbolInfo = SymbolInfo
    { _typ :: Type
    , _defIdent :: Ident
    , _defInBlock :: DefPlace
    , _prevDef :: Maybe SymbolInfo
    }

instance Show SymbolInfo where
    show (SymbolInfo t i bl prev) = absShow i ++ " : " ++ absShow t ++ " (defined at " ++ show bl ++ ")" ++ case prev of
        Nothing -> "."
        Just si -> "\n\t prev:\n" ++ indentStr (show si)

makeLenses ''InfoSt
makeLenses ''SymbolInfo


insertTopTypes :: CheckSt ()
insertTopTypes = do
    tds <- use (wholeProgram . aa . programTopDefs)
    let funcType t args = makeAbs $ Fun t (args ^.. traverse . aa . argType)
    let aux td = putItemIntoState (funcType (td ^. aa . topDefType) (td ^. aa . topDefArgs)) (td ^. aa . topDefIdent) td Global
    mapM_ aux tds

buildInfoSt :: Program -> InfoSt
buildInfoSt = InfoSt $ M.fromList
    [ runtimeFun "printInt"     Void    [Int]
    , runtimeFun "printString"  Void    [Str]
    , runtimeFun "error"        Void    []
    , runtimeFun "readInt"      Int     []
    , runtimeFun "readString"   Str     []
    ]
  where
    runtimeFun name retType argTypes = let ident = makeAbs (Ident name)
        in (ident, SymbolInfo (makeAbs $ Fun (makeAbs retType) (map makeAbs argTypes)) ident Global Nothing)


programValid :: Program -> CompilerOptsM (Either String ())
programValid program = evalStateT (runExceptT checkProgram) (buildInfoSt program)


checkProgram :: CheckSt ()
checkProgram = do
    insertTopTypes
    checkMain
    topDefs <- use $ wholeProgram . aa . programTopDefs
    mapM_ checkFunction topDefs
    mapM_ checkFunReturn topDefs


checkMain :: CheckSt ()
checkMain = use (symbols . at (makeAbs $ Ident "main")) >>= \case
        Just main -> do
            let expectedType = makeAbs (Fun (makeAbs Int) [])
            unless (main ^. typ == expectedType) $ contextError (main ^. defIdent)
                ("The `main` function has wrong signature: it should be of type " ++ absShow expectedType
                    ++ ", but it is of type " ++ absShow (main ^. typ))  >>= throwError
        Nothing -> throwError $ red "Error: " ++ "File doesn't contain the `main` function!"

checkFunReturn :: TopDef -> CheckSt ()
checkFunReturn topDef = unless (topDef ^. aa . topDefType == makeAbs Void) $ case hasReturn (simplifyTopDef topDef) of
    Right () -> return ()
    Left err -> throwError err

checkFunction :: TopDef -> CheckSt ()
checkFunction topDef = do
    let defPlace = InBlock $ topDef ^. aa . topDefBlock
    mapM_ (\arg -> putItemIntoState (arg ^. aa . argType) (arg ^. aa . argIdent) arg defPlace) (topDef ^. aa . topDefArgs)
    checkBlock (topDef ^. aa . topDefType) (topDef ^. aa . topDefBlock)

checkBlock :: Type -> Block -> CheckSt ()
checkBlock returnType block = do
    ss <- use symbols
--     lift $ lift $ verbosePrint $ "Checking block:\n" ++ show block ++ "it has defined:\n" ++ unlines (map show (M.elems ss))
    mapM_ checkStmt $ block ^. aa . blockStmts
    modify $ over symbols $ M.mapMaybe $ \symbolInfo -> if symbolInfo ^. defInBlock == InBlock block
        then symbolInfo ^. prevDef
        else Just symbolInfo
  where
    checkStmt stmt = case stmt ^. aa of
        Empty -> return ()
        BStmt bl -> checkBlock returnType bl
        Decl t items -> forM_ items $ ignorePos $ \case
            NoInit ident    -> putItemIntoState t ident stmt (InBlock block)
            Init ident expr -> putItemIntoState t ident stmt (InBlock block) >> constrainExprType t expr
        Ass ident expr -> do
            t <- getIdentType ident
            constrainExprType t expr
        Incr ident -> do
            t <- getIdentType ident
            unless (t == makeAbs Int) $ contextError stmt ("Increment operator requires type " ++ show Int ++ ", but `"
                ++ absShow ident ++ "` is of type " ++ absShow t) >>= throwError
        Decr ident -> do
            t <- getIdentType ident
            unless (t == makeAbs Int) $ contextError stmt ("Decrement operator requires type " ++ show Int ++ ", but `"
                ++ absShow ident ++ "` is of type " ++ absShow t) >>= throwError
        Ret expr -> constrainExprType returnType expr
        VRet -> unless (returnType == makeAbs Void) $ contextError stmt
            ("Void return in function returning type " ++ absShow returnType ++ "!") >>= throwError
        Cond expr s -> do
            constrainExprType (makeAbs Bool) expr
            checkStmt s
        CondElse expr s1 s2 -> do
            constrainExprType (makeAbs Bool) expr
            checkStmt s1
            checkStmt s2
        While expr s -> do
            constrainExprType (makeAbs Bool) expr
            checkStmt s
        SExp expr -> void $ getExprType expr

getExprType :: Expr -> CheckSt Type
getExprType expr = case expr ^. aa of
    EVar ident -> getIdentType ident
    ELitInt integer -> return $ makeAbs Int
    ELitBool bool -> return $ makeAbs Bool
    EApp ident exprs -> do
        types <- mapM getExprType exprs
        funcType <- getIdentType ident
        funcApplication funcType types >>= \case
            Just t -> return t
            Nothing -> contextError expr ("Application function of type " ++ absShow funcType
                ++ " to arguments of types: " ++ intercalate ", " (map absShow types) ++ " failed!") >>= throwError
    EString string -> return (makeAbs Str)
    Neg e -> constrainExprType (makeAbs Int) e >> return (makeAbs Int)
    Not e -> constrainExprType (makeAbs Bool) e >> return (makeAbs Bool)
    EMul e1 _ e2 -> binOpType Int Int e1 e2
    EAdd e1 op e2 -> case op ^. aa of
        Plus  -> binOpType Int Int e1 e2 `catchError` \_ -> binOpType Str Str e1 e2
        Minus -> binOpType Int Int e1 e2
    ERel e1 op e2 -> case op ^. aa of
        EQU -> binOpType Bool Int e1 e2 `catchError` \_ -> binOpType Bool Bool e1 e2
        NEQ -> binOpType Bool Int e1 e2 `catchError` \_ -> binOpType Bool Bool e1 e2
        _   -> binOpType Bool Int e1 e2
    EAnd e1 e2 -> binOpType Bool Bool e1 e2
    EOr e1 e2  -> binOpType Bool Bool e1 e2
  where
    binOpType :: AType -> AType -> Expr -> Expr -> CheckSt Type
    binOpType retType argsType e1 e2 = do
        constrainExprType (makeAbs argsType) e1
        constrainExprType (makeAbs argsType) e2
        return (makeAbs retType)


getIdentType :: Ident -> CheckSt Type
getIdentType ident = do
    xx <- get
    let mType = M.lookup ident (xx ^. symbols)
    case mType of
        Just info -> return $ info ^. typ
        Nothing -> identUnknown ident >>= throwError

constrainExprType :: Type ->  Expr -> CheckSt ()
constrainExprType expectedType expr = do
    actualType <- getExprType expr
    unless (expectedType == actualType) $ contextError expr ("Expression `" ++ absShow expr ++ "` was expected to be of type "
        ++ absShow expectedType ++ " but it is of type " ++ absShow actualType ++ "!") >>= throwError

funcApplication :: Type -> [Type] -> CheckSt (Maybe Type)
funcApplication funType types = do
    let Fun retType _ = funType ^. aa
    return $ if funType == makeAbs (Fun retType types)
        then Just retType
        else Nothing

putItemIntoState :: Type -> Ident -> AbsPos a -> DefPlace -> CheckSt ()
putItemIntoState t ident context defPlace = do
    mSymbolInfo <- use $ symbols . at ident
    let mod = modify $ over symbols $ M.insert ident (SymbolInfo t ident defPlace mSymbolInfo)
    case mSymbolInfo of
        Nothing -> mod
        Just symbolInfo -> if symbolInfo ^. defInBlock /= defPlace
            then mod
            else let prevPos = (show . fromJust . view (defIdent . pos)) symbolInfo in contextError context
                ("Identifier `" ++ absShow ident ++ "` is already declared (at " ++ prevPos ++ ")!")
                    >>= throwError

-- error handling
getProgramLine :: Int -> CheckSt CStr
getProgramLine n = do
    program <- use wholeProgram
    let relLine = n - fst (begin $ fromJust $ program ^. pos)
    return $ fromJust (program ^. cStrRep) !! relLine

identUnknown :: Ident -> CheckSt String
identUnknown ident = contextError ident ("Identifier `" ++ absShow ident ++ "` is unknown!")

contextError :: AbsPos a -> String -> CheckSt String
contextError element errorMsg = do
    let p@(Position b e) = fromJust (element ^. pos)
    line <- getProgramLine (fst b)
    let start = snd b
    let end = if fst e == fst b then snd e - 1 else csLength line - start + 1
    return $ unlines
        [ red ("Error in " ++ show p ++ ": ") ++ errorMsg
        , csShow line
        , ([1 .. start - 1] >> " ") ++ red ([start .. end] >> "^")
        ]
