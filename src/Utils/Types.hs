{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils.Types where

import Utils.Abstract
import Utils.Eval
import Utils.Position
import Utils.Show
import Utils.Verbose

import Control.Lens hiding (Empty)
import Control.Monad.Except
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Foldable


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
    , _classes :: ClassSt
    , _wholeProgram :: Program
    } deriving Show

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

type ClassSt = M.Map Ident ClassInfo

data ClassInfo = ClassInfo
    { _attributes :: M.Map Ident Type
    , _methods :: M.Map Ident Type
    , _superClass :: Maybe Ident
    } deriving Show

makeLenses ''InfoSt
makeLenses ''SymbolInfo
makeLenses ''ClassInfo


insertTopTypes :: CheckSt ()
insertTopTypes = do
    tds <- use (wholeProgram . aa . programFunctions)
    mapM_ aux tds
  where
    aux td = putItemIntoState (makeFunType (td ^. aa . fnDefType) (td ^. aa . fnDefArgs)) (td ^. aa . fnDefIdent) td Global

insertClassInfos :: CheckSt ()
insertClassInfos = do
    classes <- use (wholeProgram . aa . programClasses)
    let names = map (^. aa . clsDefIdent) classes
    _ <- foldlM checkNames M.empty names
    mapM_ insertClass classes
  where
    checkNames acc ident = case ident `M.lookup` acc of
        Just prevIdent -> reDefinition prevIdent ident "Class"
        Nothing -> if (ident ^. aa . identString) `S.member` S.fromList ["int", "boolean", "string", "void"]
            then contextError ident "Class name must be different than primitive type names!" >>= throwError
            else return $ M.insert ident ident acc
    insertClass cls = do
        let stmts = cls ^. aa . clsDefBody . aa . classBodyStmts
        (attrs, methods) <- foldlM aux (M.empty, M.empty) stmts
        classes . at (cls ^. aa . clsDefIdent) .= Just (ClassInfo (M.map snd attrs) (M.map snd methods) (cls ^. aa . clsDefExtend))
    aux (amp, mmp) clsStmt = case clsStmt ^. aa of
        Attr t is -> foldlM (attrAux t) (amp, mmp) is
        Method retType name args _ -> case name `M.lookup` mmp of
            Just (prev, _) -> reDefinition prev name "Method in this class"
            Nothing -> return (amp, M.insert name (name, makeFunType retType args) mmp)
    attrAux t (amp, mmp) attrItem = let AttrItem i = attrItem ^. aa in case i `M.lookup` amp of
        Just (prev, _) -> reDefinition prev i "Attribute"
        Nothing -> return (M.insert i (i, t) amp, mmp)
    reDefinition prev new display = do
        let posInfo = maybe "" (\s -> " (at " ++ show s ++ ")") (prev ^. pos)
        contextError new (display ++ " `" ++ absShow new ++ "` is already declared" ++ posInfo ++ "!") >>= throwError

makeFunType :: Type -> [Arg] -> Type
makeFunType t args = makeAbs $ Fun t (args ^.. traverse . aa . argType)

buildInfoSt :: Program -> InfoSt
buildInfoSt = InfoSt buildIns M.empty
  where
    buildIns = M.fromList
        [ runtimeFun "printInt"     Void    [Int]
        , runtimeFun "printString"  Void    [Str]
        , runtimeFun "error"        Void    []
        , runtimeFun "readInt"      Int     []
        , runtimeFun "readString"   Str     []
        ]
    runtimeFun name retType argTypes = let ident = makeAbs (Ident name)
        in (ident, SymbolInfo (makeAbs $ Fun (makeAbs retType) (map makeAbs argTypes)) ident Global Nothing)


programValid :: Program -> CompilerOptsM (Either String ())
programValid program = evalStateT (runExceptT checkProgram) (buildInfoSt program)


checkProgram :: CheckSt ()
checkProgram = do
    insertTopTypes
    insertClassInfos
    checkMain
    fnDefs <- use $ wholeProgram . aa . programFunctions
    mapM_ checkFunction fnDefs
    mapM_ checkFunReturn fnDefs


checkMain :: CheckSt ()
checkMain = use (symbols . at (makeAbs $ Ident "main")) >>= \case
        Just main -> do
            let expectedType = makeAbs (Fun (makeAbs Int) [])
            unless (main ^. typ == expectedType) $ contextError (main ^. defIdent)
                ("The `main` function has wrong signature: it should be of type " ++ absShow expectedType
                    ++ ", but it is of type " ++ absShow (main ^. typ))  >>= throwError
        Nothing -> throwError $ red "Error: " ++ "File doesn't contain the `main` function!"

checkFunReturn :: FnDef -> CheckSt ()
checkFunReturn fnDef = unless (fnDef ^. aa . fnDefType == makeAbs Void) $ case hasReturn (simplifyFnDef fnDef) of
    Right () -> return ()
    Left err -> throwError err

checkFunction :: FnDef -> CheckSt ()
checkFunction fnDef = do
    let defPlace = InBlock $ fnDef ^. aa . fnDefBlock
    mapM_ (\arg -> putItemIntoState (arg ^. aa . argType) (arg ^. aa . argIdent) arg defPlace) (fnDef ^. aa . fnDefArgs)
    checkBlock (fnDef ^. aa . fnDefType) (fnDef ^. aa . fnDefBlock)

checkBlock :: Type -> Block -> CheckSt ()
checkBlock returnType block = do
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
        Ass lval expr -> do
            t <- getLValueType lval
            constrainExprType t expr
        Incr lval -> do
            t <- getLValueType lval
            unless (t == makeAbs Int) $ contextError stmt ("Increment operator requires type " ++ show Int ++ ", but `"
                ++ absShow lval ++ "` is of type " ++ absShow t) >>= throwError
        Decr lval -> do
            t <- getLValueType lval
            unless (t == makeAbs Int) $ contextError stmt ("Decrement operator requires type " ++ show Int ++ ", but `"
                ++ absShow lval ++ "` is of type " ++ absShow t) >>= throwError
        Ret expr -> constrainExprType returnType expr
        VRet -> unless (returnType == makeAbs Void) $ contextError stmt
            ("Void return in function returning type " ++ absShow returnType ++ "!") >>= throwError
        Cond expr b -> do
            constrainExprType (makeAbs Bool) expr
            checkBlock returnType b
        CondElse expr b1 b2 -> do
            constrainExprType (makeAbs Bool) expr
            checkBlock returnType b1
            checkBlock returnType b2
        While expr b -> do
            constrainExprType (makeAbs Bool) expr
            checkBlock returnType b
        SExp expr -> void $ getExprType expr

getExprType :: Expr -> CheckSt Type
getExprType expr = case expr ^. aa of
    EVar ident -> getIdentType ident
    ELitInt _ -> return $ makeAbs Int
    ELitBool _ -> return $ makeAbs Bool
    ENull t -> return t
    EApp ident exprs -> do
        types <- mapM getExprType exprs
        funcType <- getIdentType ident
        funcApplication funcType types >>= \case
            Just t -> return t
            Nothing -> contextError expr ("Application function of type " ++ absShow funcType
                ++ " to arguments of types: " ++ intercalate ", " (map absShow types) ++ " failed!") >>= throwError
    EMember _ -> undefined -- TODO
    ENew t -> return t
    EString _ -> return (makeAbs Str)
    Neg e -> constrainExprType (makeAbs Int) e >> return (makeAbs Int)
    Not e -> constrainExprType (makeAbs Bool) e >> return (makeAbs Bool)
    EMul e1 _ e2 -> binOpType Int Int e1 e2
    EAdd e1 op e2 -> case op ^. aa of
        Plus  -> binOpType Int Int e1 e2 `catchError` \_ -> binOpType Str Str e1 e2
        Minus -> binOpType Int Int e1 e2
    ERel e1 op e2 -> case op ^. aa of
        EQU -> binOpType Bool Int e1 e2 `catchError` \_ -> binOpType Bool Bool e1 e2 `catchError` \_ -> binOpType Bool Str e1 e2
        NEQ -> binOpType Bool Int e1 e2 `catchError` \_ -> binOpType Bool Bool e1 e2 `catchError` \_ -> binOpType Bool Str e1 e2
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

getLValueType :: LValue -> CheckSt Type
getLValueType = undefined -- TODO

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
