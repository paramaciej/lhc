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
    validClsNames <- S.fromList . M.keys <$> foldlM checkNames M.empty names
    mapM_ (insertClass validClsNames) classes
  where
    checkNames acc ident = case ident `M.lookup` acc of
        Just prevIdent -> reDefinition prevIdent ident "Class"
        Nothing -> if (ident ^. aa . identString) `S.member` S.fromList ["int", "boolean", "string", "void"]
            then contextError ident "Class name must be different than primitive type names!" >>= throwError
            else return $ M.insert ident ident acc
    insertClass validNames cls = do
        let stmts = cls ^. aa . clsDefBody . aa . classBodyStmts
        (attrs, methods) <- foldlM aux (M.empty, M.empty) stmts
        case  cls ^. aa . clsDefExtend of
            Nothing -> return ()
            Just super -> unless (super `S.member` validNames) $ contextError super ("Class `" ++ absShow (cls ^. aa . clsDefIdent) ++ "` is extending non-existent class `" ++ absShow super ++"`!") >>= throwError
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
    clsDefs <- use $ wholeProgram . aa . programClasses
    mapM_ checkClass clsDefs
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

checkClass :: ClsDef -> CheckSt ()
checkClass cls = mapM_ (checkClassStmt cls) (cls ^. aa . clsDefBody . aa . classBodyStmts)

checkClassStmt :: ClsDef -> ClassStmt -> CheckSt ()
checkClassStmt cls stmt = case stmt ^. aa of
    Attr _ _ -> return () -- TODO chyba mogę, bo robię checki na to już w insertClassInfos
    Method t name args block -> do
        case cls ^. aa . clsDefExtend of
            Just super -> use (classes . at super . singular _Just . methods . at name) >>= \case
                Just superType -> do
                    let thisType = makeFunType t args
                    typeMatch <- thisType `isSubTypeOf` superType
                    unless typeMatch $ contextError name ("Method must be of the same type as method from superclass!\n"
                        ++ "\texprected: " ++ show superType ++ "\n\tactual: " ++ show thisType ++ "") >>= throwError
                Nothing -> return ()
            Nothing -> return ()
        putClassAttrsIntoState cls block
        putArgsIntoState block args
        checkBlock t block
      where
        putClassAttrsIntoState cls block = do
            attrs <- clsAllAttrs (cls ^. aa . clsDefIdent)
            mapM_ aux (M.toAscList attrs)
          where
            aux (i, t) = putItemIntoState t i cls (InBlock block)
        clsAllAttrs :: Ident -> CheckSt (M.Map Ident Type)
        clsAllAttrs clsIdent = do
            defInThis <- use $ classes . at clsIdent . singular _Just . attributes
            defInSuper <- use (classes . at clsIdent . singular _Just . superClass) >>= \case
                Just super -> clsAllAttrs super
                Nothing -> return M.empty
            return $ defInThis `M.union` defInSuper


checkMethodReturn :: ClassStmt -> CheckSt ()
checkMethodReturn clsStmt = unless methodIsVoid $ case methodHasReturn (simplifyClsStmt clsStmt) of
    Right () -> return ()
    Left err -> throwError err
  where
    methodIsVoid = clsStmt ^. aa . singular methodRetType == makeAbs Void

checkFunReturn :: FnDef -> CheckSt ()
checkFunReturn fnDef = unless (fnDef ^. aa . fnDefType == makeAbs Void) $ case fnHasReturn (simplifyFnDef fnDef) of
    Right () -> return ()
    Left err -> throwError err

checkFunction :: FnDef -> CheckSt ()
checkFunction fnDef = do
    putArgsIntoState (fnDef ^. aa . fnDefBlock) (fnDef ^. aa . fnDefArgs)
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
    EMember member -> case member ^. aa of
        MemberAttr obj attr -> getMemberType AttrKind obj attr
        MemberMethod obj method exprs -> do
            argTypes <- mapM getExprType exprs
            funcType <- getMemberType MethodKind obj method
            funcApplication funcType argTypes >>= \case
                Just t -> return t
                Nothing -> contextError expr ("Application method of type " ++ absShow funcType
                    ++ " to arguments of types: " ++ intercalate ", " (map absShow argTypes) ++ " failed!") >>= throwError
    ENew t -> return t
    EString _ -> return (makeAbs Str)
    Neg e -> constrainExprType (makeAbs Int) e >> return (makeAbs Int)
    Not e -> constrainExprType (makeAbs Bool) e >> return (makeAbs Bool)
    EMul e1 _ e2 -> binOpType Int Int e1 e2
    EAdd e1 op e2 -> case op ^. aa of
        Plus  -> binOpType Int Int e1 e2 `catchError` \_ -> binOpType Str Str e1 e2
        Minus -> binOpType Int Int e1 e2
    ERel e1 op e2 -> case op ^. aa of
        EQU -> eqRelOp e1 e2
        NEQ -> eqRelOp e1 e2
        _   -> binOpType Bool Int e1 e2
    EAnd e1 e2 -> binOpType Bool Bool e1 e2
    EOr e1 e2  -> binOpType Bool Bool e1 e2
  where
    binOpType :: AType -> AType -> Expr -> Expr -> CheckSt Type
    binOpType retType argsType e1 e2 = do
        constrainExprType (makeAbs argsType) e1
        constrainExprType (makeAbs argsType) e2
        return (makeAbs retType)
    clsRelOpType :: Expr -> Expr -> CheckSt Type
    clsRelOpType e1 e2 = do
        t <- getExprType e1
        case t ^. aa of
            ClsType _ -> constrainExprType t e2 >> return (makeAbs Bool)
            _ -> contextError e1 "Expression in ==/!== must be of int, boolean, string or class type!" >>= throwError
    eqRelOp :: Expr -> Expr -> CheckSt Type
    eqRelOp e1 e2 = binOpType Bool Int e1 e2
        `catchError` const (binOpType Bool Bool e1 e2)
        `catchError` const (binOpType Bool Str e1 e2)
        `catchError` const (clsRelOpType e1 e2)


getIdentType :: Ident -> CheckSt Type
getIdentType ident = use (symbols . at ident) >>= \case
    Just info -> return $ info ^. typ
    Nothing -> identUnknown ident >>= throwError

getLValueType :: LValue -> CheckSt Type
getLValueType lval = case lval ^. aa of
    LVar ident -> getIdentType ident
    LMember obj attr -> getMemberType AttrKind obj attr

getMemberType :: ClassMemberKind -> Ident -> Ident -> CheckSt Type
getMemberType kind obj member = getIdentType obj >>= \typ -> case typ ^. aa of
    ClsType cls -> use (classes . at cls) >>= \case
        Just clsInfo -> clsInfoMemberType kind clsInfo member
        Nothing -> contextError obj ("Class `" ++ absShow cls ++ "` not found!") >>= throwError
    _ -> contextError obj ("Identifier `" ++ absShow obj ++ "` must be a class instance!`") >>= throwError

clsInfoMemberType :: ClassMemberKind -> ClassInfo -> Ident -> CheckSt Type
clsInfoMemberType kind clsInfo attr = case attr `M.lookup` (clsInfo ^. getter) of
    Just t -> return t
    Nothing -> case clsInfo ^. superClass of
        Just super -> use (classes . at super) >>= \case
            Just superInfo -> clsInfoMemberType kind superInfo attr
            Nothing -> contextError super ("Class `" ++ absShow super ++ "` is not definied!") >>= throwError
        Nothing -> contextError attr ("Class hasn't got `" ++ absShow attr ++ "` " ++ show kind ++ "!") >>= throwError
  where
    getter = case kind of
        AttrKind -> attributes
        MethodKind -> methods

constrainExprType :: Type -> Expr -> CheckSt ()
constrainExprType expectedType expr = do
    actualType <- getExprType expr
    accepted <- actualType `isSubTypeOf` expectedType
    unless accepted $ contextError expr ("Expression `" ++ absShow expr ++ "` was expected to be a subtype of "
        ++ absShow expectedType ++ " but it is of type " ++ absShow actualType ++ "!") >>= throwError

isSubTypeOf :: Type -> Type -> CheckSt Bool
sub `isSubTypeOf` sup = case (sub ^. aa, sup ^. aa) of
    (ClsType subIdent, ClsType supIdent) -> if subIdent == supIdent
        then return True
        else use (classes . at subIdent) >>= \case
            Just info -> case info ^. superClass of
                Just subSuper -> makeAbs (ClsType subSuper) `isSubTypeOf` sup
                Nothing -> return False
            Nothing -> return False
    (subType, supType) -> return $ subType == supType

funcApplication :: Type -> [Type] -> CheckSt (Maybe Type)
funcApplication funType types = do
    let Fun retType _ = funType ^. aa
    return $ if funType == makeAbs (Fun retType types)
        then Just retType
        else Nothing

putItemIntoState :: Type -> Ident -> AbsPos a -> DefPlace -> CheckSt ()
putItemIntoState t ident context defPlace = case t ^. aa of
    Void -> contextError context "Cannot declare variables of type void!" >>= throwError
    _ -> do
        mSymbolInfo <- use $ symbols . at ident
        let mod = modify $ over symbols $ M.insert ident (SymbolInfo t ident defPlace mSymbolInfo)
        case mSymbolInfo of
            Nothing -> mod
            Just symbolInfo -> if symbolInfo ^. defInBlock /= defPlace
                then mod
                else let prevPos = (show . fromJust . view (defIdent . pos)) symbolInfo in contextError context
                    ("Identifier `" ++ absShow ident ++ "` is already declared (at " ++ prevPos ++ ")!")
                        >>= throwError

putArgsIntoState :: Block -> [Arg] -> CheckSt ()
putArgsIntoState block = mapM_ aux
  where
    aux arg = putItemIntoState (arg ^. aa . argType) (arg ^. aa . argIdent) arg (InBlock block)

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
