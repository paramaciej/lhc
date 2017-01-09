{-# LANGUAGE TemplateHaskell #-}

module Utils.Abstract where

import qualified AbsLatte as L
import Utils.Position

import System.Console.ANSI
import Control.Lens hiding (Empty)
import Data.Maybe
import Data.List
import Utils.Show
import Utils.Verbose

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
    }

instance Show AProgram where
    show (Program topDefs) = intercalate "\n" (map absShow topDefs)

type TopDef = AbsPos ATopDef
data ATopDef = FnDef
    { _topDefType :: Type
    , _topDefIdent :: Ident
    , _topDefArgs :: [Arg]
    , _topDefBlock ::Block
    }

instance Show ATopDef where
    show (FnDef t ident args block) = absShow t ++ " " ++ absShow ident ++ " ("
        ++ intercalate "," (map absShow args) ++ ") " ++ absShow block

type Arg = AbsPos AArg
data AArg = Arg
    { _argType::Type
    , _argIdent :: Ident
    }

instance Show AArg where
    show (Arg t ident) = absShow t ++ " " ++ absShow ident

type Block = AbsPos ABlock
data ABlock = Block
    { _blockStmts :: [Stmt]
    } deriving Eq

instance Show ABlock where
    show (Block stmts) = "{\n" ++ indentStr (unlines $ map absShow stmts) ++ "}"

sameBlock :: Block -> Block -> Bool
b1 `sameBlock` b2 = b1 ^. pos == b2 ^. pos

type Stmt = AbsPos AStmt
data AStmt
    = Empty
    | BStmt {_bStmtBlock :: Block}
    | Decl  {_declType ::Type, _declItems :: [Item]}
    | Ass   {_assIdent :: Ident, _assExpr :: Expr}
    | Incr  {_incrIdent :: Ident}
    | Decr  {_decrIdent :: Ident}
    | Ret   {_retExpr :: Expr}
    | VRet
    | Cond { _condExpr :: Expr, _condBl :: Block }
    | CondElse { _condElseExpr :: Expr, _condElseBlTrue :: Block, _condElseBlFalse :: Block }
    | While { _whileExpr :: Expr, _whileBl :: Block }
    | SExp { _sExpExpr :: Expr }
  deriving Eq

instance Show AStmt where
    show Empty = ";"
    show (BStmt block) = absShow block
    show (Decl t items) = absShow t ++ " " ++ intercalate ", " (map absShow items) ++ ";"
    show (Ass ident expr) = absShow ident ++ " = " ++ absShow expr ++ ";"
    show (Incr ident) = absShow ident ++ "++;"
    show (Decr ident) = absShow ident ++ "--;"
    show (Ret expr) = green "return " ++ absShow expr ++ ";"
    show VRet = green "return" ++ ";"
    show (Cond expr stmt) = green "if " ++ "(" ++ absShow expr ++ ") " ++ absShow stmt
    show (CondElse expr sTrue sFalse) = green "if " ++ "(" ++ absShow expr ++ ") " ++ absShow sTrue ++ green " else " ++ absShow sFalse
    show (While expr stmt) = green "while " ++ "(" ++ absShow expr ++ ") " ++ absShow stmt
    show (SExp expr) = absShow expr ++ ";"

type Item = AbsPos AItem
data AItem = NoInit Ident | Init Ident Expr
  deriving Eq

instance Show AItem where
    show (NoInit ident) = absShow ident
    show (Init ident expr) = absShow ident ++ " = " ++ absShow expr

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
  deriving Eq

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
  deriving Eq

instance Show AMulOp where
    show op = colorize [SetColor Foreground Vivid Yellow] $ case op of
        Times -> "*"
        Div -> "/"
        Mod -> "%"

type AddOp = AbsPos AAddOp
data AAddOp = Plus | Minus
  deriving Eq

instance Show AAddOp where
    show op = colorize [SetColor Foreground Vivid Yellow] $ case op of
        Plus -> "+"
        Minus -> "-"

type RelOp = AbsPos ARelOp
data ARelOp = LTH | LEQ | GTH | GEQ | EQU | NEQ
  deriving Eq

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