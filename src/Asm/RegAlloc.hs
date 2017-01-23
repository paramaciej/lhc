{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Asm.RegAlloc where

import Quattro.Alive
import qualified Quattro.Types as Q
import Utils.Abstract (ARelOp(LTH, LEQ, GTH, GEQ, EQU, NEQ))
import Utils.Verbose

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S


data Register = Register
    { _registerType :: RegType
    , _registerReg :: Reg
    } deriving (Eq, Ord)

data RegType = Ptr | Int
  deriving (Eq, Ord)

data Reg = RAX | RDX | RBX | RCX | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 -- TODO czy na pewno nie chcę RSP i RBP?
  deriving (Eq, Ord, Enum, Bounded, Show)

argRegs :: [Reg]
argRegs = [RDI, RSI, RDX, RCX, R8, R9]

data RealLoc = RegisterLoc Register | Stack Integer
  deriving (Eq, Ord)

data Value = Location RealLoc | Literal Integer deriving Eq

newtype ALabel = ALabel Q.Label

data LabelWithAlive = LabelWithAlive
    { _aliveLabel :: ALabel
    , _afterLabel :: Q.AliveSet
    } deriving Show

data Out
    = Goto LabelWithAlive
    | Branch LabelWithAlive LabelWithAlive Q.Value
    | Ret Q.Value
    | VRet

data OrdinaryOp = Add | Sub
  deriving Eq

data AsmStmt
    = Mov Value RealLoc
    | Cmp Value RealLoc
    | BinStmt OrdinaryOp Value RealLoc
    | IMul Value Register
    | IDiv RealLoc
    | CDQ
    | Xor Value RealLoc
    | Not RealLoc
    | CondMov Q.RelOp RealLoc RealLoc
    | Jmp String
    | Jz String
    | Push Value
    | Call String
    | LeaveRet
    | Label String
    | Globl String
    | Custom String
  deriving Eq

data AllocSt = AllocSt
    { _stack :: M.Map Q.Address (S.Set RealLoc)
    , _registers :: M.Map Reg (Maybe Q.Address)
    , _asmStmts :: [AsmStmt]
    } deriving Show

data StmtWithAlive = StmtWithAlive
    { _stmt :: Q.Stmt
    , _after :: Q.AliveSet
    }

type AllocM = StateT AllocSt CompilerOptsM

type StackFixStmt = (Q.Address, Integer, Integer)

instance Show ALabel where
    show (ALabel int) = "_" ++ show int

instance Show Value where
    show (Literal int) = "$" ++ show int
    show (Location realLoc) = show realLoc

instance Show Register where
    show (Register typ reg) = "%" ++ case typ of
        Ptr -> case reg of
            RAX -> "rax"
            RDX -> "rdx"
            RBX -> "rbx"
            RCX -> "rcx"
            RSI -> "rsi"
            RDI -> "rdi"
            R8  -> "r8"
            R9  -> "r9"
            R10 -> "r10"
            R11 -> "r11"
            R12 -> "r12"
            R13 -> "r13"
            R14 -> "r14"
            R15 -> "r15"
        Int -> case reg of
            RAX -> "eax"
            RDX -> "edx"
            RBX -> "ebx"
            RCX -> "ecx"
            RSI -> "esi"
            RDI -> "edi"
            R8  -> "r8d"
            R9  -> "r9d"
            R10 -> "r10d"
            R11 -> "r11d"
            R12 -> "r12d"
            R13 -> "r13d"
            R14 -> "r14d"
            R15 -> "r15d"

instance Show RealLoc where
    show (RegisterLoc register) = show register
    show (Stack pos)            = show ((-8) * (pos + 1)) ++ "(%rbp)"

instance Show AsmStmt where
    show (Globl str)  = ".globl " ++ str
    show (Label str)  = str ++ ":"
    show (Mov v1 v2)  = "  mov  " ++ show v1 ++ ", " ++ show v2
    show (Cmp v1 v2)  = "  cmp  " ++ show v1 ++ ", " ++ show v2
    show (Jmp label)  = "  jmp  " ++ label
    show (Jz  label)  = "  jz   " ++ label
    show (Push val)   = "  push " ++ show val
    show (Call fun)   = "  call " ++ fun
    show LeaveRet     = "  leave\n  ret"
    show (IMul v1 v2) = "  imull " ++ show v1 ++ ", " ++ show v2
    show (IDiv v1)    = "  idivl " ++ show v1
    show CDQ          = "  cdq"
    show (Xor v1 v2)  = "  xorl " ++ show v1 ++ ", " ++ show v2
    show (Not v1)     = "  notl " ++ show v1
    show (Custom s)   = s
    show (BinStmt op v1 v2) = "  " ++ opShow op ++ " " ++ show v1 ++ ", " ++ show v2
      where
        opShow Add = "addl "
        opShow Sub = "subl "
    show (CondMov op v1 v2) = "  " ++ opShow op ++ " " ++ show v1 ++ ", " ++ show v2
      where
        opShow = \case
            LTH -> "cmovll"
            LEQ -> "cmovlel"
            GTH -> "cmovgl"
            GEQ -> "cmovgel"
            EQU -> "cmovel"
            NEQ -> "cmovnel"


makeLenses ''Register
makeLenses ''AllocSt
makeLenses ''StmtWithAlive
makeLenses ''LabelWithAlive

isRegLoc :: RealLoc -> Bool
isRegLoc (RegisterLoc _) = True
isRegLoc _ = False

initialAllocSt :: Q.AliveSet -> AllocSt
initialAllocSt inSet = AllocSt (M.fromList $ zip (S.toList inSet) (map (S.singleton . Stack) [0..])) (M.fromList $ map (\r -> (r, Nothing)) [minBound..]) []


stmtsWithAlive :: M.Map Q.Label Q.AliveSet -> Q.ClearBlock -> [StmtWithAlive]
stmtsWithAlive inSets block = foldr aux [] stmts
  where
    Q.ClearBlock stmts out = block
    outSet = setFromOut inSets out
    aux s [] = [StmtWithAlive s outSet]
    aux s acc@(prev:_) = StmtWithAlive s (stmtMod (prev ^. stmt) (prev ^. after)) : acc

outWithAlive :: M.Map Q.Label Q.AliveSet -> Q.OutStmt -> Out
outWithAlive inSets = \case
    Q.Goto nextBlock -> Goto (LabelWithAlive (ALabel nextBlock) (inSets M.! nextBlock))
    Q.Branch trueBlock falseBlock val ->
        Branch (LabelWithAlive (ALabel trueBlock) (inSets M.! trueBlock)) (LabelWithAlive (ALabel falseBlock) (inSets M.! falseBlock)) val
    Q.Ret val -> Ret val
    Q.VRet -> VRet


fixStmts :: Q.AliveSet -> AllocM [StackFixStmt]
fixStmts alive = do
    list <- mapM singleMove (zip (S.elems alive) [0..])
    return $ filter (\(_, from, to) -> from /= to) list
  where
    singleMove :: (Q.Address, Integer) -> AllocM StackFixStmt
    singleMove (addr, idx) = use (stack . at addr) >>= \case
        Just set -> case S.elems (S.map (\(Stack x) -> x) $ S.filter (not . isRegLoc) set) of
            pos:_ -> return (addr, pos, idx)
            [] -> error $ "LOC" ++ show addr ++ " IS NOWHERE IN STACK (AT THE TIME OF PROCESSING OUT STMT"
        Nothing -> error $ "LOC" ++ show addr ++ " IS NOWHERE (AT THE TIME OF PROCESSING OUT STMT"
