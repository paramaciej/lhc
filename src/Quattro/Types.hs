{-# LANGUAGE TemplateHaskell #-}
module Quattro.Types where

import Control.Lens
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Utils.Abstract as A
import Utils.Show
import Utils.Types
import Utils.Verbose


data BinOp = Add | Sub | Mul | Div | Mod
  deriving (Show, Eq)

data UniOp = Neg | Not
  deriving Show

type RelOp = A.ARelOp

data Stmt
    = FunArg Address Integer
    | Mov Address Value
    | BinStmt Address BinOp Value Value
    | CmpStmt Address RelOp Value Value
    | UniStmt Address UniOp Value
    | Call Address String [Value]
    | StringLit Address String

data OutStmt
    = Goto Label
    | Branch Label Label Value
    | Ret Value
    | VRet

data RegType = Ptr | Int
  deriving (Eq, Ord, Show)

data Address = Address
    { _addressLoc :: Integer
    , _addressType :: RegType
    } deriving (Eq, Ord)

type Label = Integer

data Value = Location Address | Literal Integer

data QBlock = QBlock
    { _entry :: [Label]
    , _phiQ :: M.Map Address (A.Ident, M.Map Label Address)
    , _blockStmts :: [Stmt]
    , _out :: Maybe OutStmt
    } deriving Show

data QCode = QCode
    { _entryCodeBlock :: Label
    , _codeBlocks :: M.Map Label QBlock
    } deriving Show

data LocalInfo = LocalInfo
    { _address :: M.Map Label Address
    , _locType :: RegType
    , _defIn :: DefPlace
    , _prev :: Maybe LocalInfo
    } deriving Show

data QuattroSt = QuattroSt
    { _locals :: M.Map A.Ident LocalInfo
    , _currentFun :: Maybe A.Ident
    , _currentBlock :: Label
    , _funCode :: M.Map A.Ident QCode
    , _addressMax :: Integer
    , _labelMax :: Integer
    , _funRetTypes :: M.Map A.Ident RegType
    } deriving Show

type GenM = StateT QuattroSt CompilerOptsM

-- clean types
newtype ProgramCode = ProgramCode
    { _functions :: M.Map String FunctionCode}

data FunctionCode = FunctionCode
    { _entryBlock :: Label
    , _blocks :: M.Map Label Block}

data Block = Block
    { _phi :: M.Map Address (M.Map Label Address)
    , _statements :: [Stmt]
    , _escape :: OutStmt
    }

-- types without phi
data ClearBlock = ClearBlock [Stmt] OutStmt
data ClearFunction = ClearFunction Label (M.Map Label ClearBlock)
newtype ClearProgram = ClearProgram (M.Map String ClearFunction)


type AliveSet = S.Set Address

instance Show Address where
    show (Address x _) = show x

instance Show ProgramCode where
    show (ProgramCode functions) = showAbsProgCode functions

instance Show FunctionCode where
    show (FunctionCode entry blocks) = showAbsFunCode entry blocks

instance Show Block where
    show (Block phi stmts escape) = showPhi phi ++ unlines (map show stmts)
            ++ show escape
      where
        showPhi = unlines . map
            (\(k, v) -> show k ++ yellow " <- " ++ "φ (" ++ intercalate ", "
                (map (\(l,a) -> show l ++ ": " ++ show a)$ M.toAscList v) ++ ")") . M.toAscList


instance Show ClearBlock where
    show (ClearBlock stmts out) = unlines (map show stmts) ++ show out

instance Show ClearFunction where
    show (ClearFunction entry blocks) = showAbsFunCode entry blocks

instance Show ClearProgram where
    show (ClearProgram functions) = showAbsProgCode functions

showAbsProgCode :: Show a => M.Map String a -> String
showAbsProgCode functions = unlines $ map showFunc (M.toAscList functions)
  where
    showFunc (name, funCode) = green "function " ++ name ++ "\n" ++ show funCode

showAbsFunCode :: Show a => Label -> M.Map Label a -> String
showAbsFunCode entry blocks = "( starts in LABEL " ++ show entry ++ " )\n\n" ++ unlines (map showBlock (M.toAscList blocks))
  where
    showBlock (label, block) = yellow ("LABEL " ++ show label) ++ "\n" ++ indentStr (show block)



instance Show Stmt where
    show (Mov a v)              = show a ++ yellow " <- " ++ show v
    show (FunArg a nr)          = show a ++ yellow " <- arg " ++ show nr
    show (BinStmt a op v1 v2)   = show a ++ yellow (" <- " ++ show op) ++ " " ++ show v1 ++ " " ++ show v2
    show (CmpStmt a op v1 v2)   = show a ++ yellow " <- " ++ show v1 ++ " " ++ yellow (show op) ++ " " ++ show v2
    show (UniStmt a op v1)      = show a ++ yellow (" <- " ++ show op) ++ " " ++ show v1
    show (Call a str vs)        = show a ++ yellow " <- call " ++ str ++ " (" ++ intercalate ", " (map show vs) ++ ")"
    show (StringLit a str)      = show a ++ yellow " <- " ++ red str


instance Show OutStmt where
    show (Goto label)         = green "goto " ++ show label
    show (Branch l1 l2 val) = green "if " ++ show val ++ green " then goto " ++ show l1 ++ green " else " ++ show l2
    show (Ret val)            = green "ret " ++ show val
    show VRet                 = green "ret"

instance Show Value where
    show (Location addr)    = show addr
    show (Literal int)      = red $ "$" ++ show int


makeLenses ''Address
makeLenses ''QBlock
makeLenses ''QCode
makeLenses ''LocalInfo
makeLenses ''QuattroSt

makeLenses ''ProgramCode
makeLenses ''FunctionCode
makeLenses ''Block