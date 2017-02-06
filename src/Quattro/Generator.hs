{-# LANGUAGE LambdaCase #-}
module Quattro.Generator where

import Quattro.Types
import qualified Utils.Abstract as A
import Utils.Types
import Utils.Verbose

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import qualified Data.Map as M

genProgram :: A.Program -> GenM ()
genProgram program = do
    mapM_ genFnDef  (program ^. A.aa . A.programFunctions)
    mapM_ genClsDef (program ^. A.aa . A.programClasses)

genFnDef :: A.FnDef -> GenM ()
genFnDef fnDef = do
    let block = fnDef ^. A.aa . A.fnDefBlock
    let ident = fnDef ^. A.aa . A.fnDefIdent

    codeForFun ident
    mapM_ (uncurry $ genFunArg block) (zip [0..] $ fnDef ^. A.aa . A.fnDefArgs ^.. traverse . A.aa)

    genBlock block


genClsDef :: A.ClsDef -> GenM ()
genClsDef clsDef = do
    let methods = filter isMethod $ clsDef ^. A.aa . A.clsDefBody . A.aa . A.classBodyStmts ^.. traverse . A.aa
    mapM_ genMethod methods
  where
    isMethod A.Attr{} = False
    isMethod A.Method{} = True
    genMethod method = do
        let fName = let cName = (clsDef ^. A.aa . A.clsDefIdent . A.aa . A.identString);
                            mName = (method ^. A.methodName . A.aa . A.identString)
                    in "_class_" ++ cName ++ "_" ++ show (length cName) ++ "_" ++ mName ++ "_" ++ show (length mName)
        let block = method ^. singular A.methodBlock
        let args = method ^. singular A.methodArgs ^.. traverse . A.aa
        codeForFun (A.makeAbs $ A.Ident fName)

        vTables<- M.mapMaybe (getMethodNumberFromClsInfo (method ^. singular A.methodName)) <$> use classInfo
        emitExpr $ IsMethod fName $ M.mapKeys getVTableName vTables
        mapM_ (uncurry $ genFunArg block) (zip [0..] $ (A.selfArg clsDef ^. A.aa) : args)
        genBlock (method ^. singular A.methodBlock)


    getMethodNumberFromClsInfo :: A.Ident -> ClsInfo -> Maybe Integer
    getMethodNumberFromClsInfo mIdent clsInfo = aux (mIdent `M.lookup` (clsInfo ^. qMethods))
      where
        aux (Just (_, nr, cls)) = if cls == clsDef ^. A.aa . A.clsDefIdent then Just nr else Nothing
        aux Nothing = Nothing
    getVTableName ident = let cName = (ident ^. A.aa . A.identString) in "_vtable_" ++ cName ++ "_" ++ show (length cName)



codeForFun :: A.Ident -> GenM ()
codeForFun ident = do
    funCode . at ident .= Just (QCode 0 M.empty)
    currentFun .= Just ident
    startBlock <- freshBlock
    funCode . at ident . singular _Just . entryCodeBlock .= startBlock
    setActiveBlock startBlock

genBlock :: A.Block -> GenM ()
genBlock block = do
    mapM_ (genStmt block) (block ^. A.aa . A.blockStmts)
    modify $ over locals $ M.mapMaybe $ \info -> if info ^. defIn == InBlock block
        then info ^. prev
        else Just info

genStmt :: A.Block -> A.Stmt -> GenM ()
genStmt block = A.ignorePos $ \case
    A.Empty -> return ()
    A.BStmt block -> genBlock block
    A.Decl typ items -> forM_ items $ A.ignorePos $ \case
        A.NoInit ident -> do
            val <- genExpr (defaultValForType typ)
            declare block ident typ
            setLocal ident val
        A.Init ident expr -> do
            val <- genExpr expr
            declare block ident typ
            setLocal ident val
    A.Ass lval expr -> genExpr expr >>= setLocalLValue lval
    A.Incr lval -> do
        loc <- genLValueExpr lval
        temp <- freshLocForLValue lval
        emitExpr $ BinStmt temp Add (Location loc) (Literal 1)
        setLocalLValue lval $ Location temp
    A.Decr lval -> do
        loc <- genLValueExpr lval
        temp <- freshLocForLValue lval
        emitExpr $ BinStmt temp Sub (Location loc) (Literal 1)
        setLocalLValue lval $ Location temp
    A.Ret expr -> do
        temp <- genExpr expr
        quitBlock $ Ret temp
    A.VRet -> quitBlock VRet
    A.Cond expr ifTrue -> do
        condBlock <- freshBlock
        trueBlock <- freshBlock
        nextBlock <- freshBlock
        quitBlock $ Goto condBlock
        performOnBlock condBlock $ genExpr expr >>= quitBlock . Branch trueBlock nextBlock
        performOnBlock trueBlock $ genBlock ifTrue >> quitBlock (Goto nextBlock)
        setActiveBlock nextBlock

    A.CondElse expr ifTrue ifFalse -> do
        condBlock  <- freshBlock
        trueBlock  <- freshBlock
        falseBlock <- freshBlock
        nextBlock  <- freshBlock
        quitBlock $ Goto condBlock
        performOnBlock condBlock  $ genExpr expr >>= quitBlock . Branch trueBlock falseBlock
        performOnBlock trueBlock  $ genBlock ifTrue >> quitBlock (Goto nextBlock)
        performOnBlock falseBlock $ genBlock ifFalse >> quitBlock (Goto nextBlock)
        setActiveBlock nextBlock

    A.While expr whileBlock -> do
        condBlock <- freshBlock
        bodyBlock <- freshBlock
        nextBlock <- freshBlock
        quitBlock $ Goto condBlock
        performOnBlock condBlock $ genExpr expr >>= quitBlock . Branch bodyBlock nextBlock
        performOnBlock bodyBlock $ genBlock whileBlock >> quitBlock (Goto condBlock)
        setActiveBlock nextBlock

    A.SExp expr -> void $ genExpr expr

genExpr :: A.Expr -> GenM Value
genExpr = A.ignorePos $ \case
    A.ERVal rvalue -> case rvalue ^. A.aa of
        A.RLValue lvalue -> Location <$> genLValueExpr lvalue
        A.RApp lvalue args -> do
            values <- mapM genExpr args
            case lvalue ^. A.aa of
                A.LVar function -> use (funRetTypes . at function) >>= \case
                    Just typ -> do
                        ret <- freshLoc typ
                        emitExpr $ Call ret (function ^. A.aa . A.identString) values
                        return $ Location ret
                    Nothing -> error $ "RETURN TYPE FOR FUNCTION " ++ A.absShow function ++ " NOT FOUND"
                A.LMember obj method -> do
                    objAddr <- genLValueExpr obj
                    info <- getClassInfo objAddr
                    case info ^. qMethods . at method of
                        Just (typ, number, _) -> do
                            let A.Fun retType _ = typ ^. A.aa
                            ret <- freshLoc retType
                            emitExpr $ CallVirtual ret objAddr number values
                            return $ Location ret
                        Nothing -> error $ "METHOD " ++ A.absShow method ++ " NOT FOUND"
    A.ELitInt int -> return $ Literal int
    A.ELitBool bool -> return $ Literal $ if bool then 1 else 0
    A.ENull typ -> return $ Null typ
    A.ENew typ -> do
        ret <- freshLoc typ
        info <- getClassInfoFromType typ
        let size = toInteger $ 8 * (1 + info ^. qAttrs . to length)
        let A.ClsType ident = typ ^. A.aa
        emitExpr $ New ret (ident ^. A.aa . A.identString) size (info ^. qMethods . to length > 0)
        Location <$> foldlM aux ret (M.elems $ info ^. qAttrs)
      where
        aux :: Address -> (A.Type, Integer) -> GenM Address
        aux ret (t, nr) = case t ^. A.aa of
            A.Str -> do
                stringLoc <- freshLoc t
                newRet <- freshLoc typ
                emitExpr $ StringLit stringLoc Nothing
                emitExpr $ SetAttr newRet ret nr (Location stringLoc)
                return newRet
            _ -> return ret
    A.EString string -> do
        ret <- freshLoc $ A.makeAbs A.Str
        emitExpr $ StringLit ret string
        return $ Location ret

    A.Neg expr      -> genUniOp Neg expr
    A.Not expr      -> genUniOp Not expr
    A.EMul e1 op e2 -> case op ^. A.aa of
        A.Times -> genBinOp Mul e1 e2
        A.Div   -> genBinOp Div e1 e2
        A.Mod   -> genBinOp Mod e1 e2
    A.EAdd e1 op e2 -> case op ^. A.aa of
        A.Plus  -> genBinOp Add e1 e2
        A.Minus -> genBinOp Sub e1 e2
    A.ERel e1 op e2 -> genRelOp (op ^. A.aa) e1 e2
    A.EAnd e1 e2    -> lazyOp False e1 e2
    A.EOr e1 e2     -> lazyOp True e1 e2

  where
    m = A.makeAbs
    lazyOp or e1 e2 = do
        tempIdent <- getUniqueIdent
        let tempBlock = m (A.Block
                [ m (A.Decl (m A.Bool) [m $ A.Init tempIdent e1])
                , m (A.Cond
                    (if or
                        then m $ A.Not (m $ A.ERVal $ m $ A.RLValue $ m $ A.LVar tempIdent)
                        else m $ A.ERVal $ m $ A.RLValue $ m $ A.LVar tempIdent)
                    (m $ A.Block [m $ A.Ass (m $ A.LVar tempIdent) e2]))
                ])
        mapM_ (genStmt tempBlock) (tempBlock ^. A.aa . A.blockStmts)
        val <- genExpr (m $ A.ERVal $ m $ A.RLValue $ m $ A.LVar tempIdent)
        locals . at tempIdent .= Nothing
        return val

genLValueAddrMod :: (A.Ident -> GenM Address) -> A.LValue -> GenM Address
genLValueAddrMod mod = A.ignorePos $ \case
    A.LVar ident -> mod ident
    A.LMember lval member -> do
        addr <- genLValueExpr lval
        info <- getClassInfo addr
        case info ^. qAttrs . at member of
            Just (typ, number) -> do
                ret <- freshLoc typ
                emitExpr $ GetAttr ret addr number
                return ret
            Nothing -> error $ "ATTRIBUTE " ++ A.absShow member ++ " NOT FOUND"

genLValueExpr :: A.LValue -> GenM Address
genLValueExpr = genLValueAddrMod getLocal


genMov :: Address -> Value -> GenM ()
genMov to from = emitExpr $ Mov to from


genFunArg :: A.Block -> Integer -> A.AArg -> GenM ()
genFunArg block number arg = do
    declare block (arg ^. A.argIdent) (arg ^. A.argType)
    label <- use currentBlock
    addr <- use $ locals . at (arg ^. A.argIdent) . singular _Just . address . at label . singular _Just
    emitExpr $ FunArg addr number


genUniOp :: UniOp -> A.Expr -> GenM Value
genUniOp op e = do
    loc <- genExpr e
    ret <- freshLocForValue loc
    emitExpr $ UniStmt ret op loc
    return $ Location ret


genBinOp :: BinOp -> A.Expr -> A.Expr -> GenM Value
genBinOp op e1 e2 = do
    loc1 <- genExpr e1
    loc2 <- genExpr e2
    ret <- freshLocForValue loc1
    emitExpr $ BinStmt ret op loc1 loc2
    return $ Location ret

genRelOp :: RelOp -> A.Expr -> A.Expr -> GenM Value
genRelOp op e1 e2 = do
    loc1 <- genExpr e1
    loc2 <- genExpr e2
    ret <- freshLoc $ A.makeAbs A.Int
    emitExpr $ CmpStmt ret op loc1 loc2
    return $ Location ret


emitExpr :: Stmt -> GenM ()
emitExpr stmt = do
    lab <- use currentBlock
    emitExprIn lab stmt

emitExprIn :: Label -> Stmt -> GenM ()
emitExprIn label stmt = do
    fun <- getCurrentFun
    qStmtGetter fun label %= aux
  where
    aux :: QBlock -> QBlock
    aux qBlock = case qBlock ^. out of
            Nothing -> qBlock & blockStmts %~ cons stmt
            Just _ -> qBlock

freshLoc :: A.Type -> GenM Address
freshLoc typ = do
    lastLoc <- use addressMax
    addressMax += 1
    return $ Address lastLoc typ


freshLocForIdent :: A.Ident -> GenM Address
freshLocForIdent ident = use (locals . at ident . singular _Just . locType) >>= freshLoc

freshLocForLValue :: A.LValue -> GenM Address
freshLocForLValue = genLValueAddrMod freshLocForIdent

freshLocForValue :: Value -> GenM Address
freshLocForValue (Literal _) = freshLoc $ A.makeAbs A.Int
freshLocForValue (Location (Address _ t)) = freshLoc t
freshLocForValue (Null t) = freshLoc t

freshBlock :: GenM Label
freshBlock = do
    fun <- getCurrentFun
    lastLabel <- use labelMax
    labelMax += 1
    funCode . at fun . _Just . codeBlocks . at lastLabel .= Just (QBlock [] M.empty [] Nothing)
    return lastLabel

setActiveBlock :: Label -> GenM ()
setActiveBlock label = currentBlock .= label

declare :: A.Block -> A.Ident -> A.Type -> GenM ()
declare block ident typ = do
    addr <- freshLoc typ
    label <- use currentBlock
    oldInfo <- use (locals . at ident)
    locals . at ident .= Just (LocalInfo (M.fromList [(label, addr)]) typ (InBlock block) oldInfo)

setLocalLValue :: A.LValue -> Value -> GenM ()
setLocalLValue lval rval = case lval ^. A.aa of
    A.LVar ident -> setLocal ident rval
    A.LMember obj attr -> do
        oldLeft <- genLValueExpr obj
        newLeft <- freshLoc (oldLeft ^. addressType)
        info <- getClassInfo oldLeft
        case info ^. qAttrs . at attr of
            Just (_, number) -> do
                emitExpr $ SetAttr newLeft oldLeft number rval
                setLocalLValue obj (Location newLeft)
            Nothing -> error $ "ATTRIBUTE " ++ A.absShow attr ++ " NOT FOUND"

setLocal :: A.Ident -> Value -> GenM ()
setLocal ident val = do
    bl <- use currentBlock
    case val of
        Location addr -> locals . at ident . _Just . address . at bl .= Just addr
        _ -> do
            -- we don't use getLocal, because previous value of variable doesn't interest us here
            newAddr <- freshLocForValue val
            locals . at ident . _Just . address . at bl .= Just newAddr
            genMov newAddr val

getClassInfo :: Address -> GenM ClsInfo
getClassInfo addr = getClassInfoFromType $ addr ^. addressType

getClassInfoFromType :: A.Type -> GenM ClsInfo
getClassInfoFromType = A.ignorePos $ \case
    A.ClsType ident -> use (classInfo . at ident) >>= \case
        Just info -> return info
        Nothing -> error $ "INFORMATION ABOUT CLASS " ++ A.absShow ident ++ " NOT FOUND"
    other -> error $ "CLASS OBJECT IS NOT OF CLASS TYPE (IS: " ++ show other ++ ")"

getLocal :: A.Ident -> GenM Address
getLocal ident = do
    block <- use currentBlock
    getLocalFrom block ident

getLocalFrom :: Label -> A.Ident -> GenM Address
getLocalFrom label ident = use (locals . at ident) >>= \case
    Just info -> case info ^. address . at label of
        Just addr -> do
            verbosePrint $ "get local for " ++ A.absShow ident ++ " --> " ++ show addr
            return addr
        Nothing -> genPhi label ident
    Nothing -> error $ "LOCAL " ++ A.absShow ident ++ " UNKNOWN IN BLOCK " ++ show label

genPhi :: Label -> A.Ident -> GenM Address
genPhi bl ident = do
    verbosePrint $ "genphi for " ++ A.absShow ident
    fun <- getCurrentFun
    newAddr <- freshLocForIdent ident
    locals . at ident . _Just . address . at bl .= Just newAddr

    phiList <- use (qStmtGetter fun bl . entry) >>= mapM getAddr
    qStmtGetter fun bl . phiQ . at newAddr .= Just (ident, M.fromList phiList)
    return newAddr
  where
    getAddr label = do
        addr <- getLocalFrom label ident
        return (label, addr)

getCurrentFun :: GenM A.Ident
getCurrentFun = use $ currentFun . singular _Just

quitBlock :: OutStmt -> GenM ()
quitBlock oStmt = do
    fun <- getCurrentFun
    label <- use currentBlock
    use (qStmtGetter fun label . out) >>= \case
        Nothing -> do
            qStmtGetter fun label . out .= Just oStmt
            case oStmt of
                Goto destination -> appendEntryPoint label destination
                Branch dest1 dest2 _ -> appendEntryPoint label dest1 >> appendEntryPoint label dest2
                _ -> return ()
        Just currentOut -> qStmtGetter fun label . out .= Just currentOut -- we leave previous escape statetment

appendEntryPoint :: Label -> Label -> GenM ()
appendEntryPoint source destination = do
    fun <- getCurrentFun
    qStmtGetter fun destination . entry %= cons source

    phis <- use $ qStmtGetter fun destination . phiQ
    newPhis <- mapM aux phis
    qStmtGetter fun destination . phiQ .= newPhis
  where
    aux (ident, mp) = do
        addr <- getLocalFrom source ident
        return (ident, M.insert source addr mp)


performOnBlock :: Label -> GenM () -> GenM ()
performOnBlock label ops = do
    prevBlock <- use currentBlock
    setActiveBlock label
    ops
    setActiveBlock prevBlock


getUniqueIdent :: GenM A.Ident
getUniqueIdent = do
    newName <- (("TEMP_" ++) . unwords . map (^. A.aa . A.identString) . M.keys) <$> use locals
    return $ A.makeAbs (A.Ident newName)

defaultValForType :: A.Type -> A.Expr
defaultValForType = A.ignorePos $ \case
    A.Int -> A.makeAbs (A.ELitInt 0)
    A.Bool -> A.makeAbs (A.ELitBool False)
    A.Str -> A.makeAbs (A.EString Nothing)
    A.ClsType cls -> A.makeAbs $ A.ENull (A.makeAbs $ A.ClsType cls)
    _ -> error "void/fun type hasn't got any default value!"

qStmtGetter :: Functor f => A.Ident -> Label -> (QBlock -> f QBlock) -> QuattroSt -> f QuattroSt
qStmtGetter fun label = funCode . at fun . singular _Just . codeBlocks . at label . singular _Just