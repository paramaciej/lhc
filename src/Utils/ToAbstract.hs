{-# LANGUAGE MultiParamTypeClasses #-}

module Utils.ToAbstract where

import Utils.Abstract
import Utils.Position
import Utils.Show
import qualified AbsLatte as L

import Control.Lens hiding (Empty)

class (ColorShow a, Positioned a) => ToAbstract a b where
    _to :: a -> b
    toA :: a -> AbsPos b
    toA a = AbsPos (Just $ position a) (Just $ cShow a) (_to a)

instance ToAbstract L.Program AProgram where
    _to (L.Program tds) = Program cls fn
      where
        (cls, fn) = foldr aux ([], []) tds
        aux a (accCls, accFn) = case a of
            L.FnDef t i _ args _ block -> (accCls, auxAbs a (FnDef (toA t) (toA i) (map toA args) (toA block)) : accFn)
            L.ClsDef _ i body          -> (auxAbs a (ClsDef (toA i) Nothing (toA body)) : accCls, accFn)
            L.ClsDefExt _ i _ ext body -> (auxAbs a (ClsDef (toA i) (Just $ toA ext) (toA body)) : accCls, accFn)
        auxAbs a = AbsPos (Just $ position a) (Just $ cShow a)

instance ToAbstract L.ClassBody AClassBody where
    _to (L.ClassBody _ stmts _) = ClassBody (map toA stmts)

instance ToAbstract L.ClassStmt AClassStmt where
    _to (L.Attr t items _)                = Attr (toA t) (map toA items)
    _to (L.Method t ident _ args _ block) = Method (toA t) (toA ident) (map toA args) (toA block)

instance ToAbstract L.AttrItem AAttrItem where
    _to (L.AttrItem item) = AttrItem (toA item)

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

instance ToAbstract L.LValue ALValue where
    _to (L.LVar i) = LVar (toA i)
    _to (L.LMember i _ a) = LMember (toA i) (toA a)

instance ToAbstract L.Item AItem where
    _to (L.NoInit i) = NoInit (toA i)
    _to (L.Init i _ e) = Init (toA i) (toA e)

instance ToAbstract L.Type AType where
    _to (L.VType ident)  = let aIdent = toA ident in case aIdent ^. aa . identString of
        "int" -> Int
        "string" -> Str
        "boolean" -> Bool
        "void" -> Void
        _ -> ClsType aIdent
    _to L.Fun{}    = error "impossible happened"

instance ToAbstract L.Expr AExpr where
    _to (L.ERVal rval) = ERVal (toA rval)
    _to (L.ELitInt (L.PInteger (_, int))) = ELitInt (read int)
    _to (L.ELitTrue _) = ELitBool True
    _to (L.ELitFalse _) = ELitBool False
    _to (L.ENull _ t _) = ENull (toA t)
    _to (L.ENew _ typ) = ENew (toA typ)
    _to (L.EString (L.PString (_, str))) = EString $ Just str
    _to (L.Neg _ e) = Neg (toA e)
    _to (L.Not _ e) = Not (toA e)
    _to (L.EMul e1 op e2) = EMul (toA e1) (toA op) (toA e2)
    _to (L.EAdd e1 op e2) = EAdd (toA e1) (toA op) (toA e2)
    _to (L.ERel e1 op e2) = ERel (toA e1) (toA op) (toA e2)
    _to (L.EAnd e1 _ e2) = EAnd (toA e1) (toA e2)
    _to (L.EOr e1 _ e2) = EOr (toA e1) (toA e2)
    _to (L.ECoerc _ e _) = _to e

instance ToAbstract L.RValue ARValue where
    _to (L.RLValue lval) = RLValue (toA lval)
    _to (L.RApp lval _ args _) = RApp (toA lval) (map toA args)

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

