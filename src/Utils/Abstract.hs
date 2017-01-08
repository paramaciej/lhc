{-# LANGUAGE TemplateHaskell #-}

module Utils.Abstract where

import qualified AbsLatte as L
import Utils.Position
import Utils.Show

import System.Console.ANSI
import Control.Lens hiding (Empty)
import Data.Maybe
import Data.List

data AbsPos a = AbsPos
    { _pos :: Maybe Position
    , _cStrRep :: Maybe [CStr]
    , _aa :: a}

makeLenses ''AbsPos

instance Show a => Show (AbsPos a) where
    show x = fromMaybe (absShow x) (cssShow <$> x ^. cStrRep)

instance Eq a => Eq (AbsPos a) where
    x == y = x ^. aa == y ^. aa

instance Ord a => Ord (AbsPos a) where
    x `compare` y = (x ^. aa) `compare` (y ^. aa)

absShow :: Show a => AbsPos a -> String
absShow = show . (^. aa)

makeAbs :: a -> AbsPos a
makeAbs = AbsPos Nothing Nothing

forgetPos :: AbsPos a -> AbsPos a
forgetPos (AbsPos _ _ x) = AbsPos Nothing Nothing x

ignorePos :: (a -> b) -> AbsPos a -> b
ignorePos f x = f (x ^. aa)


type Ident = AbsPos AIdent
data AIdent = Ident
    { _identString :: String
    } deriving (Eq, Ord)

instance Show AIdent where
    show = _identString

type Program = AbsPos AProgram
data AProgram = Program
    { _programTopDefs :: [TopDef]
    } deriving Show

type TopDef = AbsPos ATopDef
data ATopDef = FnDef
    { _topDefType :: Type
    , _topDefIdent :: Ident
    , _topDefArgs :: [Arg]
    , _topDefBlock ::Block
    } deriving Show

type Arg = AbsPos AArg
data AArg = Arg
    { _argType::Type
    , _argIdent :: Ident
    } deriving Show

type Block = AbsPos ABlock
data ABlock = Block
    { _blockStmts :: [Stmt]
    } deriving Show

sameBlock :: Block -> Block -> Bool
b1 `sameBlock` b2 = b1 ^. pos == b2 ^. pos

type Stmt = AbsPos AStmt
data AStmt
    = Empty
    | BStmt {_bStmtBlock ::Block}
    | Decl  {_declType ::Type, _declItems :: [Item]}
    | Ass   {_assIdent :: Ident, _assExpr :: Expr}
    | Incr  {_incrIdent :: Ident}
    | Decr  {_decrIdent :: Ident}
    | Ret   {_retExpr :: Expr}
    | VRet
    | Cond { _condExpr :: Expr, _condStmt :: Stmt }
    | CondElse { _condElseExpr :: Expr, _condElseStmtTrue :: Stmt, _condElseStmtFalse :: Stmt }
    | While { _whileExpr :: Expr, _whileStmt :: Stmt }
    | SExp { _sExpExpr :: Expr }
  deriving (Show)

type Item = AbsPos AItem
data AItem = NoInit Ident | Init Ident Expr
  deriving Show

type Type = AbsPos AType
data AType
    = Int
    | Str
    | Bool
    | Void
    | Fun Type [Type]
  deriving Eq

instance Show AType where
    show Int = colorize [SetColor Foreground Vivid Blue] "int"
    show Str = colorize [SetColor Foreground Vivid Magenta] "string"
    show Bool = colorize [SetColor Foreground Vivid Cyan] "boolean"
    show Void = colorize [SetColor Foreground Vivid Cyan] "void"
    show (Fun t args) = "(" ++ intercalate ", " (map absShow args) ++ ")"
        ++ colorize [SetColor Foreground Dull Yellow] " -> " ++ absShow t

type Expr = AbsPos AExpr
data AExpr
    = EVar Ident
    | ELitInt Integer
    | ELitBool Bool
    | EApp Ident [Expr]
    | EString String
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr

instance Show AExpr where
    show (EVar ident) = absShow ident
    show (ELitInt int) = colorize [SetColor Foreground Dull Blue] $ show int
    show (ELitBool bool) = colorize [SetColor Foreground Dull Cyan] $ if bool then "true" else "false"
    show (EApp ident args) = absShow ident ++ "(" ++ intercalate ", " (map absShow args) ++ ")"
    show (EString str) = colorize [SetColor Foreground Dull Magenta] str
    show (Neg expr) = "-" ++ absShow expr
    show (Not expr) = "!" ++ absShow expr
    show (EMul e1 op e2) = absShow e1 ++ " " ++ absShow op ++ " " ++ absShow e2
    show (EAdd e1 op e2) = absShow e1 ++ " " ++ absShow op ++ " " ++ absShow e2
    show (ERel e1 op e2) = absShow e1 ++ " " ++ absShow op ++ " " ++ absShow e2
    show (EAnd e1 e2) = absShow e1 ++ colorize [SetColor Foreground Vivid Yellow] " && " ++ absShow e2
    show (EOr e1 e2) = absShow e1 ++ colorize [SetColor Foreground Vivid Yellow] " || " ++ absShow e2

type MulOp = AbsPos AMulOp
data AMulOp = Times | Div | Mod

instance Show AMulOp where
    show op = colorize [SetColor Foreground Vivid Yellow] $ case op of
        Times -> "*"
        Div -> "/"
        Mod -> "%"

type AddOp = AbsPos AAddOp
data AAddOp = Plus | Minus

instance Show AAddOp where
    show op = colorize [SetColor Foreground Vivid Yellow] $ case op of
        Plus -> "+"
        Minus -> "-"

type RelOp = AbsPos ARelOp
data ARelOp = LTH | LEQ | GTH | GEQ | EQU | NEQ

instance Show ARelOp where
    show op = colorize [SetColor Foreground Vivid Yellow] $ case op of
        LTH -> "<"
        LEQ -> "<="
        GTH -> ">"
        GEQ -> ">="
        EQU -> "=="
        NEQ -> "="


makeLenses ''AIdent
makeLenses ''AProgram
makeLenses ''ATopDef
makeLenses ''AArg
makeLenses ''ABlock
makeLenses ''AStmt