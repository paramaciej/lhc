{-# LANGUAGE TemplateHaskell #-}

module Utils.Abstract where

import Utils.Position

import Utils.Show
import Utils.Verbose

import Control.Lens hiding (Empty)
import Data.List
import Data.Maybe
import System.Console.ANSI

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
newtype AIdent = Ident
    { _identString :: String
    } deriving (Eq, Ord)

makeLenses ''AIdent

instance Show AIdent where
    show = _identString

type Program = AbsPos AProgram
data AProgram = Program
    { _programClasses   :: [ClsDef]
    , _programFunctions :: [FnDef]
    }

instance Show AProgram where
    show (Program cls fns) = intercalate "\n" (map absShow cls) ++ "\n" ++ intercalate "\n" (map absShow fns)

type FnDef = AbsPos AFnDef
data AFnDef = FnDef
    { _fnDefType   :: Type
    , _fnDefIdent  :: Ident
    , _fnDefArgs   :: [Arg]
    , _fnDefBlock  :: Block
    }

type ClsDef = AbsPos AClsDef
data AClsDef = ClsDef
    { _clsDefIdent  :: Ident
    , _clsDefExtend :: Maybe Ident
    , _clsDefBody   :: ClassBody
    }

instance Show AFnDef where
    show (FnDef t ident args block) = absShow t ++ " " ++ absShow ident ++ "("
        ++ intercalate ", " (map absShow args) ++ ") " ++ absShow block

instance Show AClsDef where
    show (ClsDef i mExt body) = green "class " ++ dullGreen (absShow i) ++ ext ++ absShow body
      where
        ext = case mExt of
            Nothing -> " "
            Just e -> green " extends " ++ dullGreen (absShow e) ++ " "

type ClassBody = AbsPos AClassBody
newtype AClassBody = ClassBody
    { _classBodyStmts :: [ClassStmt]
    }

instance Show AClassBody where
    show (ClassBody stmts) = "{\n" ++ indentStr (unlines $ map absShow stmts) ++ "}"

type ClassStmt = AbsPos AClassStmt
data AClassStmt
    = Attr { _attrType :: Type, _attrItems :: [AttrItem] }
    | Method
        { _methodRetType :: Type
        , _methodName :: Ident
        , _methodArgs :: [Arg]
        , _methodBlock :: Block
        }

instance Show AClassStmt where
    show (Attr t items) = absShow t ++ " " ++ intercalate ", " (map absShow items) ++ ";"
    show (Method t name args block) = absShow t ++ " " ++ absShow name ++ "("
        ++ intercalate ", " (map absShow args) ++ ") " ++ absShow block

type AttrItem = AbsPos AAttrItem
newtype AAttrItem = AttrItem Ident

instance Show AAttrItem where
    show (AttrItem ident) = absShow ident

type Arg = AbsPos AArg
data AArg = Arg
    { _argType::Type
    , _argIdent :: Ident
    }

instance Show AArg where
    show (Arg t ident) = absShow t ++ " " ++ absShow ident

type Block = AbsPos ABlock
newtype ABlock = Block
    { _blockStmts :: [Stmt]
    } deriving Eq

instance Show ABlock where
    show (Block stmts) = "{\n" ++ indentStr (unlines $ map absShow stmts) ++ "}"

sameBlock :: Block -> Block -> Bool
b1 `sameBlock` b2 = case (b1 ^. pos, b2 ^. pos) of
    (Just p1, Just p2) -> p1 == p2
    _ -> b1 == b2

type Stmt = AbsPos AStmt
data AStmt
    = Empty
    | BStmt {_bStmtBlock :: Block}
    | Decl  {_declType ::Type, _declItems :: [Item]}
    | Ass   {_assLValue :: LValue, _assExpr :: Expr}
    | Incr  {_incrLValue :: LValue}
    | Decr  {_decrLValue :: LValue}
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

type LValue = AbsPos ALValue
data ALValue = LVar Ident | LMember Ident Ident
  deriving Eq

instance Show ALValue where
    show (LVar ident) = absShow ident
    show (LMember obj attr) = absShow obj ++ "." ++ absShow attr

type Type = AbsPos AType
data AType
    = Int
    | Str
    | Bool
    | Void
    | ClsType Ident
    | Fun Type [Type]
  deriving Eq

instance Show AType where
    show Int = colorize [SetColor Foreground Vivid Blue] "int"
    show Str = colorize [SetColor Foreground Vivid Magenta] "string"
    show Bool = colorize [SetColor Foreground Vivid Cyan] "boolean"
    show Void = colorize [SetColor Foreground Vivid Cyan] "void"
    show (ClsType ident) = colorize [SetColor Foreground Vivid Red] (ident ^. aa . identString)
    show (Fun t args) = "(" ++ intercalate ", " (map absShow args) ++ ")"
        ++ colorize [SetColor Foreground Dull Yellow] " -> " ++ absShow t

type Expr = AbsPos AExpr
data AExpr
    = EVar Ident
    | ELitInt Integer
    | ELitBool Bool
    | ENull Type
    | EApp Ident [Expr]
    | EMember Member
    | ENew Type
    | EString (Maybe String)
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
    show (ENull t) = "(" ++ absShow t ++ ")null"
    show (EApp ident args) = absShow ident ++ "(" ++ intercalate ", " (map absShow args) ++ ")"
    show (EMember m) = absShow m
    show (ENew t) = colorize [SetColor Foreground Vivid Green] "new " ++ absShow t
    show (EString str) = colorize [SetColor Foreground Dull Magenta] (fromMaybe "<EMPTY STRING>" str)
    show (Neg expr) = "-" ++ absShow expr
    show (Not expr) = "!" ++ absShow expr
    show (EMul e1 op e2) = absShow e1 ++ " " ++ absShow op ++ " " ++ absShow e2
    show (EAdd e1 op e2) = absShow e1 ++ " " ++ absShow op ++ " " ++ absShow e2
    show (ERel e1 op e2) = absShow e1 ++ " " ++ absShow op ++ " " ++ absShow e2
    show (EAnd e1 e2) = absShow e1 ++ colorize [SetColor Foreground Vivid Yellow] " && " ++ absShow e2
    show (EOr e1 e2) = absShow e1 ++ colorize [SetColor Foreground Vivid Yellow] " || " ++ absShow e2

type Member = AbsPos AMember
data AMember
    = MemberAttr Ident Ident
    | MemberMethod Ident Ident [Expr]
  deriving Eq

instance Show AMember where
    show (MemberAttr obj attr) = absShow obj ++ "." ++ absShow attr
    show (MemberMethod obj method args) = absShow obj ++ "." ++ absShow method ++ "(" ++ intercalate ", " (map absShow args) ++ ")"

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
        NEQ -> "!="


makeLenses ''AProgram
makeLenses ''AFnDef
makeLenses ''AClsDef
makeLenses ''AArg
makeLenses ''ABlock
makeLenses ''AStmt