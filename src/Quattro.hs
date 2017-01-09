{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Quattro where

import Utils.Verbose
import Utils.Types
import Utils.Show
import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import qualified Utils.Abstract as A
import Utils.Types

import qualified Data.Map as M

data QuattroCode = QuattroCode
    { _codeBlocks :: M.Map Label QBlock
    }

instance Show QuattroCode where
    show (QuattroCode blocks) = green "\nBLOCKS\n======\n" ++ unlines (map (\(nr, block) -> yellow ("\nLABEL " ++ show nr ++ ":\n") ++ show block) (M.toAscList blocks))

data QBlock = QBlock
    { _blockStmts :: [QStmt]
    }

instance Show QBlock where
    show (QBlock stmts) = unlines $ map show (reverse stmts) -- TODO trzeba będzie kiedyś to zreversować!!!

data BinOp = Add | Sub | Mul | Div | Mod | Or | And
  deriving Show
data UniOp = Neg | Not
  deriving Show

type RelOp = A.ARelOp


data QStmt
    = FuncBegin String
    | Mov Value Value
    | BinStmt Value BinOp Value Value
    | CmpStmt Value RelOp Value Value
    | UniStmt Value UniOp Value
    | Goto Label
    | Branch Label Label Value
    | Param Value
    | Call Value String
    | Ret Value
    | VRet
    | StringLit Value String
  deriving Show

type Address = Integer
type Label = Integer

data Value = Location Address | Literal Integer
  deriving Show

data LocalInfo = LocalInfo
    { _address :: Address
    , _defIn :: DefPlace
    , _prev :: Maybe LocalInfo
    } deriving Show

data QuattroSt = QuattroSt
    { _locals :: M.Map A.Ident LocalInfo
    , _currentBlock :: Label
    , _code :: QuattroCode
    , _addressMax :: Integer
    , _labelMax :: Integer
    } deriving Show

makeLenses ''QuattroCode
makeLenses ''QBlock
makeLenses ''LocalInfo
makeLenses ''QuattroSt

type GenM = StateT QuattroSt CompilerOptsM

genProgram :: A.Program -> GenM ()
genProgram program = mapM_ genTopDef (program ^. A.aa . A.programTopDefs)

genTopDef :: A.TopDef -> GenM ()
genTopDef topDef = do
    let block = topDef ^. A.aa . A.topDefBlock
    mapM_ (declare block) (topDef ^. A.aa . A.topDefArgs ^.. traverse . A.aa . A.argIdent) -- TODO
    freshBlock >>= setActiveBlock
    emitExpr $ FuncBegin $ topDef ^. A.aa . A.topDefIdent . A.aa . A.identString
    genBlock block

genBlock :: A.Block -> GenM ()
genBlock block = do
    mapM_ (genStmt block) (block ^. A.aa . A.blockStmts)
    modify $ over locals $ M.mapMaybe $ \info -> if info ^. defIn== InBlock block
        then info ^. prev
        else Just info

genStmt :: A.Block -> A.Stmt -> GenM ()
genStmt block = A.ignorePos $ \case
    A.Empty -> return ()
    A.BStmt block -> genBlock block
    A.Decl t items -> forM_ items $ A.ignorePos $ \case
        A.NoInit ident -> declare block ident
        A.Init ident expr -> do
            declare block ident
            genExpr expr >>= setLocal ident
    A.Ass ident expr -> genExpr expr >>= setLocal ident
    A.Incr ident -> do
        loc <- getLocal ident
        temp <- freshLoc
        emitExpr $ BinStmt temp Add loc (Literal 1)
        setLocal ident temp
    A.Decr ident -> do
        loc <- getLocal ident
        temp <- freshLoc
        emitExpr $ BinStmt temp Sub loc (Literal 1)
        setLocal ident temp
    A.Ret expr -> do
        temp <- genExpr expr
        emitExpr $ Ret temp
    A.VRet -> emitExpr VRet
    A.Cond expr ifTrue -> do
        trueBlock <- freshBlock
        condBlock <- freshBlock
        nextBlock <- freshBlock

        emitExpr $ Goto condBlock

        setActiveBlock condBlock
        jumpingCond trueBlock nextBlock expr

        setActiveBlock trueBlock
        genBlock ifTrue
        emitExpr $ Goto nextBlock

        setActiveBlock nextBlock

    A.CondElse expr ifTrue ifFalse -> do
        condBlock <- freshBlock
        trueBlock <- freshBlock
        falseBlock <- freshBlock
        nextBlock <- freshBlock

        emitExpr $ Goto condBlock

        setActiveBlock condBlock
        jumpingCond trueBlock falseBlock expr

        setActiveBlock trueBlock
        genBlock ifTrue
        emitExpr $ Goto nextBlock

        setActiveBlock falseBlock
        genBlock ifFalse
        emitExpr $ Goto nextBlock

    A.While expr whileBlock -> do
        bodyBlock <- freshBlock
        condBlock <- freshBlock
        nextBlock <- freshBlock

        emitExpr $ Goto condBlock

        setActiveBlock condBlock
        jumpingCond bodyBlock nextBlock expr

        setActiveBlock bodyBlock
        genBlock whileBlock
        emitExpr $ Goto condBlock

        setActiveBlock nextBlock

    A.SExp expr -> void $ genExpr expr

jumpingCond :: Label -> Label -> A.Expr -> GenM ()
jumpingCond ifTrue ifFalse = A.ignorePos $ \case
    A.EAnd e1 e2 -> do
        middleBlock <- freshBlock
        jumpingCond middleBlock ifFalse e1

        setActiveBlock middleBlock
        jumpingCond ifTrue ifFalse e2
    A.EOr e1 e2 -> do
        middleBlock <- freshBlock
        jumpingCond ifTrue middleBlock e1

        setActiveBlock middleBlock
        jumpingCond ifTrue ifFalse e2
    expr -> genExpr (A.makeAbs expr) >>= emitExpr . Branch ifTrue ifFalse

genExpr :: A.Expr -> GenM Value
genExpr = A.ignorePos $ \case
    A.EVar ident -> use (locals . at ident) >>= \case
        Nothing -> error "IDENT NOT FOUND"
        Just x -> return $ Location $ x ^. address
    A.ELitInt int -> return $ Literal int
    A.ELitBool bool -> return $ Literal $ if bool then 1 else 0
    A.EApp ident args -> do
        mapM_ (genExpr >=> emitExpr . Param) (reverse args)
        ret <- freshLoc
        emitExpr $ Call ret (ident ^. A.aa . A.identString)
        return ret
    A.EString string -> do
        ret <- freshLoc
        emitExpr $ StringLit ret string
        return ret
    A.Neg expr -> genUniOp Neg expr
    A.Not expr -> genUniOp Not expr
    A.EMul e1 op e2 -> case op ^. A.aa of
        A.Times -> genBinOp Mul e1 e2
        A.Div   -> genBinOp Div e1 e2
        A.Mod   -> genBinOp Mod e1 e2
    A.EAdd e1 op e2 -> case op ^. A.aa of
        A.Plus  -> genBinOp Add e1 e2
        A.Minus -> genBinOp Sub e1 e2
    A.ERel e1 op e2 -> genRelOp (op ^. A.aa) e1 e2
    A.EAnd e1 e2 -> genBinOp And e1 e2
    A.EOr e1 e2 -> genBinOp Or e1 e2


genMov :: Value -> Value -> GenM ()
genMov to from = emitExpr $ Mov to from


genUniOp :: UniOp -> A.Expr -> GenM Value
genUniOp op e = do
    loc <- genExpr e
    ret <- freshLoc
    emitExpr $ UniStmt ret op loc
    return ret


genBinOp :: BinOp -> A.Expr -> A.Expr -> GenM Value
genBinOp op e1 e2 = do
    loc1 <- genExpr e1
    loc2 <- genExpr e2
    ret <- freshLoc
    emitExpr $ BinStmt ret op loc1 loc2
    return ret

genRelOp :: RelOp -> A.Expr -> A.Expr -> GenM Value
genRelOp op e1 e2 = do
    loc1 <- genExpr e1
    loc2 <- genExpr e2
    ret <- freshLoc
    emitExpr $ CmpStmt ret op loc1 loc2
    return ret


emitExpr :: QStmt -> GenM ()
emitExpr stmt = do
    lab <- use currentBlock
    emitExprIn lab stmt

emitExprIn :: Label -> QStmt -> GenM ()
emitExprIn label stmt = code . codeBlocks . at label . singular _Just . blockStmts %= cons stmt


freshLoc :: GenM Value
freshLoc = do
    lastLoc <- use addressMax
    addressMax += 1
    return $ Location lastLoc


freshBlock :: GenM Label
freshBlock = do
    lastLabel <- use labelMax
    labelMax += 1
    code . codeBlocks . at lastLabel .= Just (QBlock [])
    return lastLabel

setActiveBlock :: Label -> GenM ()
setActiveBlock label = currentBlock .= label

declare :: A.Block -> A.Ident -> GenM ()
declare block ident = do
    Location addr <- freshLoc
    oldInfo <- use (locals . at ident)
    locals . at ident .= Just (LocalInfo addr (InBlock block) oldInfo)

setLocal :: A.Ident -> Value -> GenM ()
setLocal ident val = case val of
    Location addr -> locals . at ident . _Just . address .= addr
    Literal _ -> use (locals .at ident) >>= \case
        Just info -> emitExpr $ Mov (Location (info ^. address)) val
        Nothing -> error "LOCAL UNKNOWN"

getLocal :: A.Ident -> GenM Value
getLocal ident = use (locals . at ident) >>= \case
    Just info -> return $ Location (info ^. address)
    Nothing -> error "LOCAL UNKNOWN"