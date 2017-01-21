{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Asm.RegAlloc where

import qualified Quattro.Types as Q
import Quattro.Alive
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

data Reg = RAX | RSI | RDI -- TODO
  deriving (Eq, Ord, Enum, Bounded)

data RealLoc = RegisterLoc Register | Stack Integer
  deriving (Eq, Ord, Show)

data Value = Location RealLoc | Literal Integer

data LabelWithAlive = LabelWithAlive
    { _aliveLabel :: Q.Label
    , _afterLabel :: Q.AliveSet
    } deriving Show

data Out
    = Goto LabelWithAlive
    | Branch LabelWithAlive LabelWithAlive Q.Value
    | Ret Q.Value
    | VRet

data AsmStmt
    = Mov Value Value
    | Cmp Value Value
    | Jmp String
    | Jz String
    | LeaveRet
    | Label String
    | Globl String

data AllocSt = AllocSt
    { _stack :: M.Map Q.Address (S.Set RealLoc)
    , _registers :: M.Map Reg (Maybe Q.Address)
    , _asmStmts :: [AsmStmt]
    }

data StmtWithAlive = StmtWithAlive
    { _stmt :: Q.Stmt
    , _after :: Q.AliveSet
    }

type AllocM = StateT AllocSt CompilerOptsM

type StackFixStmt = (Q.Address, Integer, Integer)

instance Show Value where
    show (Literal int) = "$" ++ show int
    show (Location (Stack pos)) = show pos ++ "(%esp)"
    show (Location (RegisterLoc register)) = show register

instance Show Register where
    show (Register typ reg) = "%" ++ show typ ++ show reg

instance Show RegType where
    show Ptr = "r"
    show Int = "e"

instance Show Reg where
    show RAX = "ax"
    show RSI = "si"
    show RDI = "di"

instance Show AsmStmt where
    show (Globl str) = ".globl " ++ str
    show (Label str) = str ++ ":"
    show (Mov v1 v2) = "  mov " ++ show v1 ++ ", " ++ show v2
    show (Cmp v1 v2) = "  cmp " ++ show v1 ++ ", " ++ show v2
    show (Jmp label) = "  jmp " ++ label
    show (Jz  label) = "  jz " ++ label
    show LeaveRet    = "  leave\n  ret"


makeLenses ''Register
makeLenses ''AllocSt
makeLenses ''StmtWithAlive
makeLenses ''LabelWithAlive

isRegLoc :: RealLoc -> Bool
isRegLoc (RegisterLoc _) = True
isRegLoc _ = False

initialAllocSt :: Q.AliveSet -> AllocSt
initialAllocSt inSet = AllocSt (M.fromList $ zip (S.toList inSet) (map (S.singleton . Stack) [0..])) (M.fromList $ map (\r -> (r, Nothing)) [minBound..]) []


stmtsWithAlive :: M.Map Q.Label Q.AliveSet -> Q.ClearFunction -> Q.ClearBlock -> [StmtWithAlive]
stmtsWithAlive inSets (Q.ClearFunction _ blockMap) block = foldr aux [] stmts
  where
    Q.ClearBlock stmts out = block
    outSet = setFromOut inSets out
    aux s [] = [StmtWithAlive s outSet]
    aux s acc@(prev:_) = StmtWithAlive s (stmtMod (prev ^. stmt) (prev ^. after)) : acc

outWithAlive :: M.Map Q.Label Q.AliveSet -> Q.OutStmt -> Out
outWithAlive inSets = \case
    Q.Goto nextBlock -> Goto (LabelWithAlive nextBlock (inSets M.! nextBlock))
    Q.Branch trueBlock falseBlock val ->
        Branch (LabelWithAlive trueBlock (inSets M.! trueBlock)) (LabelWithAlive falseBlock (inSets M.! falseBlock)) val
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
