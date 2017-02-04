{-# LANGUAGE LambdaCase #-}
module Quattro.Generator where

import Quattro.Types
import qualified Utils.Abstract as A
import Utils.Types
import Utils.Verbose

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

genProgram :: A.Program -> GenM ()
genProgram program = mapM_ genFnDef (program ^. A.aa . A.programFunctions)

genFnDef :: A.FnDef -> GenM ()
genFnDef fnDef = do
    let block = fnDef ^. A.aa . A.fnDefBlock
    let ident = fnDef ^. A.aa . A.fnDefIdent

    codeForFun ident
    mapM_ (uncurry $ genFunArg block) (zip [0..] $ fnDef ^. A.aa . A.fnDefArgs ^.. traverse . A.aa)

    genBlock block

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
        loc <- getLocalLValue lval
        temp <- freshLocForLValue lval
        emitExpr $ BinStmt temp Add (Location loc) (Literal 1)
        setLocalLValue lval $ Location temp
    A.Decr lval -> do
        loc <- getLocalLValue lval
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
    A.EVar ident -> Location <$> getLocal ident
    A.ELitInt int -> return $ Literal int
    A.ELitBool bool -> return $ Literal $ if bool then 1 else 0
    A.ENull t -> undefined -- TODO
    A.EApp ident args -> do
        values <- mapM genExpr args
        use (funRetTypes . at ident) >>= \case
            Just typ -> do
                ret <- freshLoc typ
                emitExpr $ Call ret (ident ^. A.aa . A.identString) values
                return $ Location ret
            Nothing -> error $ "TYPE FOR FUNC " ++ show ident ++ " NOT FOUND"
    A.EMember member -> genMemberExpr member
    A.ENew typ -> do
        ret <- freshLoc typ
        emitExpr $ New ret typ
        return $ Location ret
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
                        then m $ A.Not (m $ A.EVar tempIdent)
                        else m $ A.EVar tempIdent)
                    (m $ A.Block [m $ A.Ass (m $ A.LVar tempIdent) e2]))
                ])
        mapM_ (genStmt tempBlock) (tempBlock ^. A.aa . A.blockStmts)
        val <- genExpr (m $ A.EVar tempIdent)
        locals . at tempIdent .= Nothing
        return val
    genMemberExpr :: A.Member -> GenM Value
    genMemberExpr = A.ignorePos $ \case
        A.MemberAttr obj attr -> do
            objAddr <- getLocal obj
            error $ "Hello, " ++ show obj ++ "." ++ show attr ++ " :: " ++ show objAddr
        A.MemberMethod obj method args -> do
            values <- mapM genExpr args
            objAddr <- getLocal obj
            info <- getClassInfo obj
            case info ^. qMethods . at method of
                Just (typ, number) -> do
                    let A.Fun retType _ = typ ^. A.aa
                    ret <- freshLoc retType
                    emitExpr $ CallVirtual ret objAddr number values
                    return $ Location ret
                Nothing -> error $ "METHOD " ++ A.absShow method ++ " NOT FOUND"


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
freshLocForLValue = undefined -- TODO

freshLocForValue :: Value -> GenM Address
freshLocForValue (Literal _) = freshLoc $ A.makeAbs A.Int
freshLocForValue (Location (Address _ t)) = freshLoc t

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
    verbosePrint $ "declare ident " ++ A.absShow ident ++ " : " ++ A.absShow typ
    addr <- freshLoc typ
    verbosePrint $ "--> " ++ show addr ++ " / " ++ show (addr ^. addressType)
    label <- use currentBlock
    oldInfo <- use (locals . at ident)
    locals . at ident .= Just (LocalInfo (M.fromList [(label, addr)]) typ (InBlock block) oldInfo)

setLocalLValue :: A.LValue -> Value -> GenM ()
setLocalLValue lval rval = case lval ^. A.aa of
    A.LVar ident -> setLocal ident rval
    A.LMember obj attr -> do
        oldAddr <- getLocal obj
        newAddr <- freshLocForLValue lval
        info <- getClassInfo obj
        case info ^. qAttrs . at attr of
            Just (_, number) -> emitExpr $ SetAttr newAddr oldAddr number rval
            Nothing -> error $ "ATTRIBUTE " ++ A.absShow attr ++ " NOT FOUND"

setLocal :: A.Ident -> Value -> GenM ()
setLocal ident val = do
    bl <- use currentBlock
    case val of
        Location addr -> locals . at ident . _Just . address . at bl .= Just addr
        Literal _ -> do
            -- we don't use getLocal, because previous value of variable doesn't interest us here
            newAddr <- freshLocForValue val
            locals . at ident . _Just . address . at bl .= Just newAddr
            genMov newAddr val

getClassInfo :: A.Ident -> GenM ClsInfo
getClassInfo ident = do
    addr <- getLocal ident
    case addr ^. addressType . A.aa of
        A.ClsType ident -> use (classInfo . at ident) >>= \case
            Just info -> return info
            Nothing -> error $ "INFORMATION ABOUT CLASS " ++ A.absShow ident ++ " NOT FOUND"
        other -> error $ "CLASS OBJECT IS NOT OF CLASS TYPE (IS: " ++ show other ++ ")"

getLocalLValue :: A.LValue -> GenM Address -- TODO taka sygnatura?
getLocalLValue = undefined -- TODO

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
    Nothing -> error $ "LOCAL " ++ show ident ++ " UNKNOWN IN BLOCK " ++ show label

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
    _ -> error "void/fun type hasn't got any default value!"

qStmtGetter :: Functor f => A.Ident -> Label -> (QBlock -> f QBlock) -> QuattroSt -> f QuattroSt
qStmtGetter fun label = funCode . at fun . singular _Just . codeBlocks . at label . singular _Just