module Utils.Position
    ( Position (Position, begin, end)
    , Positioned (position)
    ) where

import AbsLatte

combineBeginEnd :: (Positioned a, Positioned b) => a -> b -> Position
combineBeginEnd a b = Position (begin $ position a) (end $ position b)


calculate :: ((Int, Int), String) -> Position
calculate (begin@(row, col), string) = Position begin (row, col + length string)

data Position = Position
    { begin :: (Int, Int)
    , end :: (Int, Int)
    } deriving Eq

instance Show Position where
    show (Position (bL, bC) (eL, eC))
        | bL == eL  = show bL ++ ":" ++ show bC ++ "-" ++ show eC
        | otherwise = show bL ++ ":" ++ show bC ++ "-" ++ show eL ++ ":" ++ show eC

class Positioned a where
    position :: a -> Position


instance Positioned PRet where
    position (PRet x) = calculate x
instance Positioned PIf where
    position (PIf x) = calculate x
instance Positioned PElse where
    position (PElse x) = calculate x
instance Positioned PWhile where
    position (PWhile x) = calculate x
instance Positioned PSemicolon where
    position (PSemicolon x) = calculate x
instance Positioned PCurlyBegin where
    position (PCurlyBegin x) = calculate x
instance Positioned PCurlyEnd where
    position (PCurlyEnd x) = calculate x
instance Positioned PParBegin where
    position (PParBegin x) = calculate x
instance Positioned PParEnd where
    position (PParEnd x) = calculate x
instance Positioned PAss where
    position (PAss x) = calculate x
instance Positioned PIncr where
    position (PIncr x) = calculate x
instance Positioned PDecr where
    position (PDecr x) = calculate x
instance Positioned PPlus where
    position (PPlus x) = calculate x
instance Positioned PMinus where
    position (PMinus x) = calculate x
instance Positioned PTimes where
    position (PTimes x) = calculate x
instance Positioned PDiv where
    position (PDiv x) = calculate x
instance Positioned PMod where
    position (PMod x) = calculate x
instance Positioned PLTH where
    position (PLTH x) = calculate x
instance Positioned PLE where
    position (PLE x) = calculate x
instance Positioned PGTH where
    position (PGTH x) = calculate x
instance Positioned PGE where
    position (PGE x) = calculate x
instance Positioned PEQU where
    position (PEQU x) = calculate x
instance Positioned PNE where
    position (PNE x) = calculate x
instance Positioned PNot where
    position (PNot x) = calculate x
instance Positioned PAnd where
    position (PAnd x) = calculate x
instance Positioned POr where
    position (POr x) = calculate x
instance Positioned PFalse where
    position (PFalse x) = calculate x
instance Positioned PTrue where
    position (PTrue x) = calculate x
instance Positioned PInteger where
    position (PInteger x) = calculate x
instance Positioned PString where
    position (PString x) = calculate x
instance Positioned PIdent where
    position (PIdent x) = calculate x
instance Positioned PClass where
    position (PClass x) = calculate x
instance Positioned PExtends where
    position (PExtends x) = calculate x
instance Positioned PNew where
    position (PNew x) = calculate x
instance Positioned PDot where
    position (PDot x) = calculate x
instance Positioned PNull where
    position (PNull x) = calculate x


instance Positioned Program where
    position (Program tds) = combineBeginEnd (head tds) (last tds)

instance Positioned TopDef where
    position (FnDef t _ _ _ _ block)  = combineBeginEnd t block
    position (ClsDef c _ body)        = combineBeginEnd c body
    position (ClsDefExt c _ _ _ body) = combineBeginEnd c body

instance Positioned ClassStmt where
    position (Attr t _ semicolon)       = combineBeginEnd t semicolon
    position (Method t _ _ _ _ block)   = combineBeginEnd t block

instance Positioned AttrItem where
    position (AttrItem i) = position i

instance Positioned Arg where
    position (Arg t i) = combineBeginEnd t i

instance Positioned Block where
    position (Block curlyBegin _ curlyEnd) = Position (begin $ position curlyBegin) (end $ position curlyEnd)

instance Positioned ClassBody where
    position (ClassBody curlyBegin _ curlyEnd) = Position (begin $ position curlyBegin) (end $ position curlyEnd)

instance Positioned Stmt where
    position (Empty semicolon)          = position semicolon
    position (BStmt block)              = position block
    position (Decl t _ semicolon)       = combineBeginEnd t semicolon
    position (Ass i _ _ semicolon)      = combineBeginEnd i semicolon
    position (Incr i _ semicolon)       = combineBeginEnd i semicolon
    position (Decr i _ semicolon)       = combineBeginEnd i semicolon
    position (Ret ret _ semicolon)      = combineBeginEnd ret semicolon
    position (VRet ret semicolon)       = combineBeginEnd ret semicolon
    position (Cond pIf _ _ _ s)         = combineBeginEnd pIf s
    position (CondElse pIf _ _ _ _ _ s) = combineBeginEnd pIf s
    position (While pWhile _ _ _ s)     = combineBeginEnd pWhile s
    position (SExp expr semicolon)      = combineBeginEnd expr semicolon

instance Positioned LValue where
    position (LVar i) = position i
    position (LMember i _ n) = combineBeginEnd i n

instance Positioned Item where
    position (NoInit i)     = position i
    position (Init i _ expr)  = combineBeginEnd i expr

instance Positioned Type where
    position (VType x)  = position x
    position Fun {}     = error "impossible happened"

instance Positioned Expr where
    position (ERVal rval)       = position rval
    position (ELitInt int)      = position int
    position (ELitTrue true)    = position true
    position (ELitFalse false)  = position false
    position (ENull pB _ pNull) = combineBeginEnd pB pNull
    position (ENew n cls)       = combineBeginEnd n cls
    position (EString str)      = position str
    position (Neg neg expr)     = combineBeginEnd neg expr
    position (Not not expr)     = combineBeginEnd not expr
    position (EMul e1 _ e2)     = combineBeginEnd e1 e2
    position (EAdd e1 _ e2)     = combineBeginEnd e1 e2
    position (ERel e1 _ e2)     = combineBeginEnd e1 e2
    position (EAnd e1 _ e2)     = combineBeginEnd e1 e2
    position (EOr e1 _ e2)      = combineBeginEnd e1 e2
    position (ECoerc pB _ pE)   = combineBeginEnd pB pE

instance Positioned RValue where
    position (RLValue lval)     = position lval
    position (RApp lval _ _ pE) = combineBeginEnd lval pE

instance Positioned AddOp where
    position (Plus x)   = position x
    position (Minus x)  = position x

instance Positioned MulOp where
    position (Times x)  = position x
    position (Div x)    = position x
    position (Mod x)    = position x

instance Positioned RelOp where
    position (LTH x)    = position x
    position (LE x)     = position x
    position (GTH x)    = position x
    position (GE x)     = position x
    position (EQU x)    = position x
    position (NE x)     = position x
