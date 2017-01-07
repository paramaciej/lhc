{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils.Types where

import Utils.Abstract
import Utils.Position
import Utils.Show
import Utils.Verbose

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
        Just si -> "\n\tprev:\n" ++ indentStr (show si)

makeLenses ''InfoSt
makeLenses ''SymbolInfo


topTypes ::Program -> SymbolsSt
topTypes program = M.fromList $ map (aux . (^. aa)) (programTopDefs $ program ^. aa)
  where
    funcType t args = makeAbs $ Fun t (map (argType . (^. aa)) args)
    aux (FnDef t ident args _) = (ident, SymbolInfo (funcType t args) ident Global Nothing)

buildInfoSt :: Program -> InfoSt
buildInfoSt program = InfoSt (topTypes program) program


programValid :: Program -> CompilerOptsM (Either String ())
programValid program = evalStateT (runExceptT $ checkProgram program) (buildInfoSt program)


checkProgram :: Program -> CheckSt ()
checkProgram = ignorePos $ \(Program topDefs) -> mapM_ checkFunction topDefs

checkFunction :: TopDef -> CheckSt ()
checkFunction topDef = do
    let block = topDefBlock $ topDef ^. aa
    mapM_ (\arg -> putItemIntoState (argType $ arg ^. aa) (argIdent $ arg ^. aa) arg block) (topDefArgs $ topDef ^. aa)
    let t = topDefType (topDef ^. aa)
    checkBlock t (topDefBlock $ topDef ^. aa)

checkBlock :: Type -> Block -> CheckSt ()
checkBlock returnType block = do
    ss <- use symbols
    lift $ lift $ verbosePrint $ "Checking block:\n" ++ show block ++ "it has defined:\n" ++ unlines (map show (M.elems ss))
    mapM_ checkStmt $ blockStmts $ block ^. aa
    modify $ over symbols $ M.mapMaybe $ \symbolInfo -> if symbolInfo ^. defInBlock == InBlock block
        then symbolInfo ^. prevDef
        else Just symbolInfo
  where
    checkStmt stmt = case stmt ^. aa of
        Empty -> return ()
        BStmt bl -> checkBlock returnType bl
        Decl t items -> forM_ items $ ignorePos $ \case
            NoInit ident    -> putItemIntoState t ident stmt block
            Init ident expr -> putItemIntoState t ident stmt block >> constrainExprType t expr
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
        VRet -> unless (returnType == makeAbs Void) $ throwError "Return void! TODO"
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
    ELitInt integer -> return intType
    ELitBool bool -> return boolType
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
    EMul e1 _ e2 -> intIntToInt e1 e2
    EAdd e1 op e2 -> case op ^. aa of
        Plus -> do
            let strAlternative = let strType = makeAbs Str in do
                    constrainExprType strType e1
                    constrainExprType strType e2
                    return strType
            -- if typing `+` as int fails, try with string:
            intIntToInt e1 e2 `catchError` const strAlternative
        Minus -> intIntToInt e1 e2
    ERel e1 _ e2 -> do
        constrainExprType intType e1
        constrainExprType intType e2
        return boolType
    EAnd e1 e2 -> boolBoolToBool e1 e2
    EOr e1 e2 -> boolBoolToBool e1 e2
  where
    intType = makeAbs Int
    boolType = makeAbs Bool
    intIntToInt e1 e2 = do
        constrainExprType intType e1
        constrainExprType intType e2
        return intType
    boolBoolToBool e1 e2 = do
        constrainExprType boolType e1
        constrainExprType boolType e2
        return boolType


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

putItemIntoState :: Type -> Ident -> AbsPos a -> Block -> CheckSt ()
putItemIntoState t ident context block = do
    mSymbolInfo <- use $ symbols . at ident
    let mod = modify $ over symbols $ M.insert ident (SymbolInfo t ident (InBlock block) mSymbolInfo)
    case mSymbolInfo of
        Nothing -> mod
        Just symbolInfo -> if symbolInfo ^. defInBlock /= InBlock block
            then mod
            else let prevPos = (show . fromJust . view (defIdent . pos)) symbolInfo in contextError context
                ("Identifier `" ++ absShow ident ++ "` is already declared (at " ++ prevPos ++ ")!")
                    >>= throwError

-- error handling
getProgramLine :: Int -> CheckSt String
getProgramLine n = do
    program <- use wholeProgram
    let relLine = n - fst (begin $ fromJust $ program ^. pos)
    return $ lines (show program) !! relLine

identUnknown :: Ident -> CheckSt String
identUnknown ident = contextError ident ("Identifier `" ++ absShow ident ++ "` is unknown!")

contextError :: AbsPos a -> String -> CheckSt String
contextError element errorMsg = do
    let p@(Position b e) = fromJust (element ^. pos)
    line <- getProgramLine (fst b)
    return $ unlines
        [ red ("Error in " ++ show p ++ ": ") ++ errorMsg
        , line
        , ([1 .. snd b - 1] >> " ") ++ red ([(snd b) .. snd e - 1] >> "^")
        ]

red :: String -> String
red = colorize [SetColor Foreground Vivid Red]