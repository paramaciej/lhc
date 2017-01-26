{-# LANGUAGE LambdaCase #-}
module Utils.Show
    ( ColorShow (cShow, posShow, fullShow)
    , CStr
    , csShow
    , cssShow
    , csLength
    , colorize
    , red
    , green
    , yellow
    , dullGreen
    ) where

import System.Console.ANSI
import Control.Monad.State

import Utils.Position
import AbsLatte

data CStrElem = CC [SGR] | CS String
type CStr = [CStrElem]

csShow :: CStr -> String
csShow [] = ""
csShow (CC sgrs : xs) = setSGRCode sgrs ++ csShow xs
csShow (CS str : xs) = str ++ csShow xs

cssShow :: [CStr] -> String
cssShow = unlines . map csShow

csLength :: CStr -> Int
csLength [] = 0
csLength (CC _ : xs) = csLength xs
csLength (CS str : xs) = length str + csLength xs

colorizeCStr :: [SGR] -> String -> CStr
colorizeCStr sgrs str = [CC sgrs, CS str, CC [Reset]]

colorize :: [SGR] -> String -> String
colorize c = csShow . colorizeCStr c

toCStr :: String -> CStr
toCStr a = [CS a]

addCStrAt :: (Int, Int) -> CStr -> State [CStr] ()
addCStrAt (line, col) str = modify aux
  where
    aux xs = if length xs < line
        then aux ([] : xs)
        else let y:ys = xs in (y ++ [CS ([1..(col - csLength y - 1)] >> " ")] ++ str) : ys

addFromToken :: ((Int, Int), String) -> (String -> CStr) -> State [CStr] ()
addFromToken ((line, col), str) f = addCStrAt (line, col) (f str)

addStr :: String -> State [CStr] ()
addStr str = let cStr = toCStr str in modify $ \case
    [] -> [cStr]
    x:xs -> (x ++ cStr) : xs

csvShow :: ColorShow a => [a] -> State [CStr] ()
csvShow [] = return ()
csvShow (x:xs) = stShow x >> mapM_ (\c -> addStr "," >> stShow c) xs

class Positioned a => ColorShow a where
    stShow :: a -> State [CStr] ()

    cShow :: a -> [CStr]
    cShow a = dropWhile null $ reverse $ dropWhile null $ execState (stShow a) []

    posShow :: a -> String
    posShow = show . position

    fullShow :: a -> String
    fullShow a = "in " ++ posShow a ++ ":\n" ++ cssShow (cShow a)


instance ColorShow PInt where
    stShow (PInt x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Blue])
instance ColorShow PStr where
    stShow (PStr x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Magenta])
instance ColorShow PBool where
    stShow (PBool x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Cyan])
instance ColorShow PVoid where
    stShow (PVoid x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Cyan])
instance ColorShow PRet where
    stShow (PRet x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Green])
instance ColorShow PIf where
    stShow (PIf x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Green])
instance ColorShow PElse where
    stShow (PElse x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Green])
instance ColorShow PWhile where
    stShow (PWhile x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Green])
instance ColorShow PSemicolon where
    stShow (PSemicolon x) = addFromToken x toCStr
instance ColorShow PCurlyBegin where
    stShow (PCurlyBegin x) = addFromToken x toCStr
instance ColorShow PCurlyEnd where
    stShow (PCurlyEnd x) = addFromToken x toCStr
instance ColorShow PParBegin where
    stShow (PParBegin x) = addFromToken x toCStr
instance ColorShow PParEnd where
    stShow (PParEnd x) = addFromToken x toCStr
instance ColorShow PAss where
    stShow (PAss x) = addFromToken x toCStr
instance ColorShow PIncr where
    stShow (PIncr x) = addFromToken x toCStr
instance ColorShow PDecr where
    stShow (PDecr x) = addFromToken x toCStr
instance ColorShow PPlus where
    stShow (PPlus x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PMinus where
    stShow (PMinus x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PTimes where
    stShow (PTimes x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PDiv where
    stShow (PDiv x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PMod where
    stShow (PMod x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PLTH where
    stShow (PLTH x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PLE where
    stShow (PLE x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PGTH where
    stShow (PGTH x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PGE where
    stShow (PGE x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PEQU where
    stShow (PEQU x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PNE where
    stShow (PNE x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PNot where
    stShow (PNot x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PAnd where
    stShow (PAnd x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow POr where
    stShow (POr x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Yellow])
instance ColorShow PFalse where
    stShow (PFalse x) = addFromToken x (colorizeCStr [SetColor Foreground Dull Cyan])
instance ColorShow PTrue where
    stShow (PTrue x) = addFromToken x (colorizeCStr [SetColor Foreground Dull Cyan])
instance ColorShow PInteger where
    stShow (PInteger x) = addFromToken x (colorizeCStr [SetColor Foreground Dull Blue])
instance ColorShow PString where
    stShow (PString x) = addFromToken x (colorizeCStr [SetColor Foreground Dull Magenta])
instance ColorShow PIdent where
    stShow (PIdent x) = addFromToken x toCStr
instance ColorShow PClass where
    stShow (PClass x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Green])
instance ColorShow PExtends where
    stShow (PExtends x) = addFromToken x (colorizeCStr [SetColor Foreground Vivid Green])


instance ColorShow Program where
    stShow (Program tds) = mapM_ stShow tds

instance ColorShow TopDef where
    stShow (FnDef t name parB args parE block) = do
        stShow t >> stShow name
        stShow parB >> csvShow args >> stShow parE >> stShow block
    stShow (ClsDef cls ident body) = stShow cls >> stShow ident >> stShow body
    stShow (ClsDefExt cls this ext parent body) = stShow cls >> stShow this >> stShow ext >> stShow parent >> stShow body

instance ColorShow Arg where
    stShow (Arg t i) = stShow t >> stShow i

instance ColorShow ClassBody where
    stShow (ClassBody curlyBegin stmts curlyEnd) = stShow curlyBegin >> mapM_ stShow stmts >> stShow curlyEnd

instance ColorShow ClassStmt where
    stShow (Attr t items semicolon) = stShow t >> csvShow items >> stShow semicolon
    stShow (Method t i parB args parE block) = stShow t >> stShow i >> stShow parB >> csvShow args >> stShow parE >> stShow block

instance ColorShow AttrItem where
    stShow (AttrItem ident) = stShow ident

instance ColorShow Block where
    stShow (Block curlyBegin stmts curlyEnd) = stShow curlyBegin >> mapM_ stShow stmts >> stShow curlyEnd

instance ColorShow Stmt where
    stShow (Empty semicolon) = stShow semicolon
    stShow (BStmt block)                        = stShow block
    stShow (Decl t is semicolon)                = stShow t >> csvShow is >> stShow semicolon
    stShow (Ass i ass expr semicolon)           = stShow i >> stShow ass >> stShow expr >> stShow semicolon
    stShow (Incr i incr semicolon)              = stShow i >> stShow incr >> stShow semicolon
    stShow (Decr i decr semicolon)              = stShow i >> stShow decr >> stShow semicolon
    stShow (Ret ret expr semicolon)             = stShow ret >> stShow expr >> stShow semicolon
    stShow (VRet ret semicolon)                 = stShow ret >> stShow semicolon
    stShow (Cond pIf parB expr parE stmt)       = stShow pIf >> stShow parB >> stShow expr >> stShow parE >> stShow stmt
    stShow (CondElse pIf parB e parE s1 pEl s2) = stShow pIf >> stShow parB >> stShow e >> stShow parE >> stShow s1 >> stShow pEl >> stShow s2
    stShow (While pWhile parB expr parE stmt)   = stShow pWhile >> stShow parB >> stShow expr >> stShow parE >> stShow stmt
    stShow (SExp expr semicolon)                = stShow expr >> stShow semicolon

instance ColorShow Item where
    stShow (NoInit i)       = stShow i
    stShow (Init i ass expr)    = stShow i >> stShow ass >> stShow expr

instance ColorShow Type where
    stShow (Int x)      = stShow x
    stShow (Str x)      = stShow x
    stShow (Bool x)     = stShow x
    stShow (Void x)     = stShow x
    stShow Fun {}       = error "impossible happened"

instance ColorShow Expr where
    stShow (EVar i)                 = stShow i
    stShow (ELitInt int)            = stShow int
    stShow (ELitTrue true)          = stShow true
    stShow (ELitFalse false)        = stShow false
    stShow (EApp i parB exprs parE) = stShow i >> stShow parB >> csvShow exprs >> stShow parE
    stShow (EString str)            = stShow str
    stShow (Neg neg expr)           = stShow neg >> stShow expr
    stShow (Not not expr)           = stShow not >> stShow expr
    stShow (EMul e1 op e2)          = stShow e1 >> stShow op >> stShow e2
    stShow (EAdd e1 op e2)          = stShow e1 >> stShow op >> stShow e2
    stShow (ERel e1 op e2)          = stShow e1 >> stShow op >> stShow e2
    stShow (EAnd e1 op e2)          = stShow e1 >> stShow op >> stShow e2
    stShow (EOr e1 op e2)           = stShow e1 >> stShow op >> stShow e2
    stShow (ECoerc parB expr parE)  = stShow parB >> stShow expr >> stShow parE

instance ColorShow AddOp where
    stShow (Plus x)     = stShow x
    stShow (Minus x)    = stShow x

instance ColorShow MulOp where
    stShow (Times x)    = stShow x
    stShow (Div x)      = stShow x
    stShow (Mod x)      = stShow x

instance ColorShow RelOp where
    stShow (LTH x)      = stShow x
    stShow (LE x)       = stShow x
    stShow (GTH x)      = stShow x
    stShow (GE x)       = stShow x
    stShow (EQU x)      = stShow x
    stShow (NE x)       = stShow x


red :: String -> String
red = colorize [SetColor Foreground Vivid Red]

green :: String -> String
green = colorize [SetColor Foreground Vivid Green]

yellow :: String -> String
yellow = colorize [SetColor Foreground Vivid Yellow]

dullGreen :: String -> String
dullGreen = colorize [SetColor Foreground Dull Green]