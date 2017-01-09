{-# LANGUAGE MultiParamTypeClasses #-}

module Utils.ToAbstract where

import Utils.Abstract
import Utils.Position
import Utils.Show
import qualified AbsLatte as L

class (ColorShow a, Positioned a) => ToAbstract a b where
    _to :: a -> b
    toA :: a -> AbsPos b
    toA a = AbsPos (Just $ position a) (Just $ cShow a) (_to a)

instance ToAbstract L.Program AProgram where
    _to (L.Program tds) = Program (map toA tds)

instance ToAbstract L.TopDef ATopDef where
    _to (L.FnDef t i _ args _ block) = FnDef (toA t) (toA i) (map toA args) (toA block)

instance ToAbstract L.Arg AArg where
    _to (L.Arg t i) = Arg (toA t) (toA i)

instance ToAbstract L.Block ABlock where
    _to (L.Block _ stmts _) = Block (map toA stmts)

instance ToAbstract L.Stmt AStmt where
    _to (L.Empty _) = Empty
    _to (L.BStmt block) = BStmt (toA block)
    _to (L.Decl t is _) = Decl (toA t) (map toA is)
    _to (L.Ass i _ e _) = Ass (toA i) (toA e)
    _to (L.Incr i _ _) = Incr (toA i)
    _to (L.Decr i _ _) = Decr (toA i)
    _to (L.Ret _ e _) = Ret (toA e)
    _to (L.VRet _ _) = VRet
    _to (L.Cond _ _ e _ s) = Cond (toA e) $ case s of
        L.BStmt block -> toA block
        _ -> makeAbs (Block [toA s])
    _to (L.CondElse _ _ e _ s1 _ s2) =
        let b1 = case s1 of
                L.BStmt block -> toA block
                _ -> makeAbs (Block [toA s1])
            b2 = case s2 of
                L.BStmt block -> toA block
                _ -> makeAbs (Block [toA s2])
        in CondElse (toA e) b1 b2
    _to (L.While _ _ e _ s) = While (toA e) $ case s of
        L.BStmt block -> toA block
        _ -> makeAbs (Block [toA s])
    _to (L.SExp e _) = SExp (toA e)

instance ToAbstract L.Item AItem where
    _to (L.NoInit i) = NoInit (toA i)
    _to (L.Init i _ e) = Init (toA i) (toA e)

instance ToAbstract L.Type AType where
    _to (L.Int _) = Int
    _to (L.Str _) = Str
    _to (L.Bool _) = Bool
    _to (L.Void _) = Void
    -- TODO fun?

instance ToAbstract L.Expr AExpr where
    _to (L.EVar i) = EVar (toA i)
    _to (L.ELitInt (L.PInteger (_, int))) = ELitInt (read int)
    _to (L.ELitTrue _) = ELitBool True
    _to (L.ELitFalse _) = ELitBool False
    _to (L.EApp i _ exprs _) = EApp (toA i) (map toA exprs)
    _to (L.EString (L.PString (_, str))) = EString str
    _to (L.Neg _ e) = Neg (toA e)
    _to (L.Not _ e) = Not (toA e)
    _to (L.EMul e1 op e2) = EMul (toA e1) (toA op) (toA e2)
    _to (L.EAdd e1 op e2) = EAdd (toA e1) (toA op) (toA e2)
    _to (L.ERel e1 op e2) = ERel (toA e1) (toA op) (toA e2)
    _to (L.EAnd e1 _ e2) = EAnd (toA e1) (toA e2)
    _to (L.EOr e1 _ e2) = EOr (toA e1) (toA e2)
    _to (L.ECoerc _ e _) = _to e

instance ToAbstract L.AddOp AAddOp where
    _to (L.Plus _)  = Plus
    _to (L.Minus _) = Minus

instance ToAbstract L.MulOp AMulOp where
    _to (L.Times _) = Times
    _to (L.Div _)   = Div
    _to (L.Mod _)   = Mod

instance ToAbstract L.RelOp ARelOp where
    _to (L.LTH _)   = LTH
    _to (L.LE _)    = LEQ
    _to (L.GTH _)   = GTH
    _to (L.GE _)    = GEQ
    _to (L.EQU _)   = EQU
    _to (L.NE _)    = NEQ

instance ToAbstract L.PIdent AIdent where
    _to (L.PIdent (_, i)) = Ident i

