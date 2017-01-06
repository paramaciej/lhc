{-# LANGUAGE LambdaCase #-}

module Utils.Types where

import Utils.Abstract
import Utils.Position
import Utils.Show

import System.Console.ANSI

import qualified Data.Map as M
import Data.Maybe
import Data.List

import Control.Monad.Except
import Control.Monad.State

topTypes ::Program -> SymbolsSt
topTypes program = M.fromList $ map (aux . aa) (programTopDefs $ aa program)
  where
    aux (FnDef t ident args _) = (identString (aa ident), makeAbs $ Fun (forgetPos t) (map (forgetPos . argType . aa) args))

buildInfoSt :: Program -> InfoSt
buildInfoSt program = InfoSt (topTypes program) program

data InfoSt = InfoSt
    { symbols :: SymbolsSt
    , wholeProgram :: Program
    }

type SymbolsSt = M.Map String Type
type CheckSt = ExceptT String (State InfoSt)


programValid :: Program -> Either String ()
programValid program = evalState (runExceptT $ checkProgram program) (buildInfoSt program)


checkProgram :: Program -> CheckSt ()
checkProgram = ignorePos $ \(Program topDefs) -> mapM_ checkFunction topDefs

checkFunction :: TopDef -> CheckSt ()
checkFunction topDef = do
    let t = topDefType $ aa topDef
    checkBlock t (topDefBlock $ aa topDef)



checkBlock :: Type -> Block -> CheckSt ()
checkBlock returnType = mapM_ checkStmt . blockStmts . aa
  where
    checkStmt stmt = case aa stmt of
        Empty -> return ()
        BStmt bl -> checkBlock returnType bl
        Decl t items -> forM_ items $ ignorePos $ \case
            NoInit ident -> modify $ \(InfoSt s wp) -> InfoSt (M.insert (identString $ aa ident) t s) wp
            Init ident expr ->  do
                modify $ \(InfoSt s wp) -> InfoSt (M.insert (identString $ aa ident) t s) wp
                constrainExprType t expr
        Ass ident expr -> do
            t <- getIdentType ident
            constrainExprType t expr
        Incr ident -> do
            t <- getIdentType ident
            unless (t == makeAbs Int) $ contextError stmt ("Incrementation requires type " ++ show Int ++ ", but `"
                ++ absShow ident ++ "` is of type " ++ absShow t) >>= throwError
        Decr ident -> do
            t <- getIdentType ident
            unless (t == makeAbs Int) $ contextError stmt ("Decrementation requires type " ++ show Int ++ ", but `"
                ++ absShow ident ++ "` is of type " ++ absShow t) >>= throwError
        Ret expr -> constrainExprType returnType expr
        VRet -> unless (returnType == makeAbs Void) $ throwError "Return void! TODO"
        Cond expr s -> do
            constrainExprType (makeAbs Bool) expr
            checkStmt s
        CondElse expr s1 s2 -> do
            constrainExprType (makeAbs Bool) expr
            checkStmt s1
            checkStmt s2
        While expr s -> do
            constrainExprType (makeAbs Bool) expr
            checkStmt s
        SExp expr -> void $ getExprType expr

getExprType :: Expr -> CheckSt Type
getExprType expr = case aa expr of
    EVar ident -> getIdentType ident
    ELitInt integer -> return intType
    ELitBool bool -> return boolType
    EApp ident exprs -> do
        types <- mapM getExprType exprs
        funcType <- getIdentType ident
        funcApplication funcType types >>= \case
            Just t -> return t
            Nothing -> contextError expr ("Application function of type " ++ absShow funcType
                ++ " to arguments of types: " ++ intercalate ", " (map absShow types) ++ " failed!") >>= throwError
    EString string -> return (makeAbs Str)
    Neg e -> constrainExprType (makeAbs Int) e >> return (makeAbs Int)
    Not e -> constrainExprType (makeAbs Bool) e >> return (makeAbs Bool)
    EMul e1 _ e2 -> intIntToInt e1 e2
    EAdd e1 _ e2 -> intIntToInt e1 e2
    ERel e1 _ e2 -> do
        constrainExprType intType e1
        constrainExprType intType e2
        return boolType
    EAnd e1 e2 -> boolBoolToBool e1 e2
    EOr e1 e2 -> boolBoolToBool e1 e2
  where
    intType = makeAbs Int
    boolType = makeAbs Bool
    intIntToInt e1 e2 = do
        constrainExprType intType e1
        constrainExprType intType e2
        return intType
    boolBoolToBool e1 e2 = do
        constrainExprType boolType e1
        constrainExprType boolType e2
        return boolType


getIdentType :: Ident -> CheckSt Type
getIdentType ident = do
    xx <- gets symbols
    let mType = M.lookup (identString $ aa ident) xx
    case mType of
        Just t -> return t
        Nothing -> identUnknown ident >>= throwError

constrainExprType :: Type ->  Expr -> CheckSt ()
constrainExprType expectedType expr = do
    actualType <- getExprType expr
    unless (expectedType == actualType) $ contextError expr ("Expression `" ++ absShow expr ++ "` was expected to be of type "
        ++ absShow expectedType ++ " but it is of type " ++ absShow actualType ++ "!") >>= throwError

funcApplication :: Type -> [Type] -> CheckSt (Maybe Type)
funcApplication funType types = do
    let Fun retType _ = aa funType
    return $ if funType == makeAbs (Fun retType types)
        then Just retType
        else Nothing

-- error handling
getProgramLine :: Int -> CheckSt String
getProgramLine n = do
    program <- gets wholeProgram
    let relLine = n - fst (begin $ fromJust $ pos program)
    return $ lines (show program) !! relLine

identUnknown :: Ident -> CheckSt String
identUnknown ident = contextError ident ("Identifier `" ++ absShow ident ++ "` is unknown!")

contextError :: AbsPos a -> String -> CheckSt String
contextError element errorMsg = do
    let p@(Position b e) = fromJust $ pos element
    line <- getProgramLine (fst b)
    return $ unlines
        [ red ("Error in " ++ show p ++ ": ") ++ errorMsg
        , line
        , ([1 .. snd b - 1] >> " ") ++ red ([(snd b) .. snd e - 1] >> "^")
        ]

red :: String -> String
red = colorize [SetColor Foreground Vivid Red]