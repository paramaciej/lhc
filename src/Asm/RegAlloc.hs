{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Asm.RegAlloc where

import Quattro.Alive
import qualified Quattro.Types as Q
import Quattro.Types (RegType (Ptr, Int))
import Utils.Abstract (ARelOp(LTH, LEQ, GTH, GEQ, EQU, NEQ))
import Utils.Verbose

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Printf


data Register = Register
    { _registerType :: RegType
    , _registerReg :: Reg
    } deriving (Eq, Ord)


data Reg = RAX | RDX | RBX | RCX | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Ord, Enum, Bounded, Show)

argRegs :: [Reg]
argRegs = [RDI, RSI, RDX, RCX, R8, R9]

calleeSaveRegs :: [Reg]
calleeSaveRegs = [RBX, R12, R13, R14, R15]

data RealLoc = RegisterLoc Register | Stack RegType Integer
  deriving (Eq, Ord)

data Value
    = Location RealLoc
    | IntLiteral Integer
    | StrLiteral Integer
  deriving Eq

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
    | Pop RealLoc
    | Call String
    | LeaveRet
    | Label String
    | Globl String
    | Custom String
    | RoString Integer String
    | SectionRoData
    | SectionText
  deriving Eq

data AllocSt = AllocSt
    { _stack :: M.Map Q.Address (S.Set RealLoc)
    , _registers :: M.Map Reg (Maybe Q.Address)
    , _asmStmts :: [AsmStmt]
    } deriving Show

data RoDataSt = RoDataSt
    { _roEmptyString :: Bool
    , _roStrings :: M.Map Integer String
    }

data StmtWithAlive = StmtWithAlive
    { _stmt :: Q.Stmt
    , _after :: Q.AliveSet
    }

type RoDataM = StateT RoDataSt CompilerOptsM

type AllocM = StateT AllocSt RoDataM

type StackFixStmt = (Q.Address, Integer, Integer)

instance Show ALabel where
    show (ALabel int) = "_" ++ show int

instance Show Value where
    show (IntLiteral int)   = "$" ++ show int
    show (StrLiteral nr)    = "$_STRING_" ++ show nr
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
    show (Stack _ pos)          = show ((-8) * (pos + 1)) ++ "(%rbp)"

instance Show AsmStmt where
    show (Globl str)    = ".globl " ++ str
    show (Label str)    = str ++ ":"
    show (Mov v1 v2)    = sufFromVal v1 "mov" ++ show v1 ++ ", " ++ show v2
    show (Cmp v1 v2)    = sufFromVal v1 "cmp" ++ show v1 ++ ", " ++ show v2
    show (Jmp label)    = align "jmp" ++ label
    show (Jz  label)    = align "jz" ++ label
    show (Push val)     = align "pushq" ++ show val
    show (Pop loc)      = align "popq" ++ show loc
    show (Call fun)     = align "call" ++ fun
    show LeaveRet       = align "leave" ++ "\n" ++ align "ret"
    show (IMul v1 v2)   = sufFromVal v1 "imul" ++ show v1 ++ ", " ++ show v2
    show (IDiv v1)      = sufFromVal (Location v1) "idiv" ++ show v1
    show CDQ            = align "cdq"
    show (Xor v1 v2)    = sufFromVal v1 "xor" ++ show v1 ++ ", " ++ show v2
    show (Not v1)       = sufFromVal (Location v1) "not" ++ show v1
    show SectionRoData  = ".section .rodata"
    show SectionText    = ".section .text"
    show (RoString i s) = "_STRING_" ++ show i ++ ":\n  .string " ++ s
    show (Custom s)   = s
    show (BinStmt op v1 v2) = opShow ++ show v1 ++ ", " ++ show v2
      where
        opShow = sufFromVal v1 $ case op of
            Add -> "add"
            Sub -> "sub"
    show (CondMov op v1 v2) = opShow ++ show v1 ++ ", " ++ show v2
      where
        opShow = sufFromVal (Location v1) $ case op of
            LTH -> "cmovl"
            LEQ -> "cmovle"
            GTH -> "cmovg"
            GEQ -> "cmovge"
            EQU -> "cmove"
            NEQ -> "cmovne"

valType :: Value -> RegType
valType (Location (RegisterLoc (Register x _))) = x
valType (Location (Stack x _ )) = x
valType (IntLiteral _) = Int
valType (StrLiteral _) = Ptr

align :: String -> String
align = printf "  %-6s "

sufFromVal :: Value -> String -> String
sufFromVal v op = align $ case valType v of
    Int -> op ++ "l"
    Ptr -> op ++ "q"


makeLenses ''Register
makeLenses ''AllocSt
makeLenses ''RoDataSt
makeLenses ''StmtWithAlive
makeLenses ''LabelWithAlive

isRegLoc :: RealLoc -> Bool
isRegLoc (RegisterLoc _) = True
isRegLoc _ = False

initialAllocSt :: Q.AliveSet -> AllocSt
initialAllocSt inSet = AllocSt (M.fromList $ zipWith aux (S.toList inSet) [0..]) (M.fromList $ map (\r -> (r, Nothing)) [minBound..]) []
  where
    aux addr int = (addr, S.singleton $ Stack (addr ^. Q.addressType) int)


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
        Just set -> case S.elems (S.map (\(Stack _ x) -> x) $ S.filter (not . isRegLoc) set) of
            pos:_ -> return (addr, pos, idx)
            [] -> error $ "LOC" ++ show addr ++ " IS NOWHERE IN STACK (AT THE TIME OF PROCESSING OUT STMT"
        Nothing -> error $ "LOC" ++ show addr ++ " IS NOWHERE (AT THE TIME OF PROCESSING OUT STMT"
