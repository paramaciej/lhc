{-# LANGUAGE LambdaCase #-}

module Utils.Eval where

import Utils.Abstract
import Utils.Show

import Control.Lens hiding (Empty)
import Control.Monad.Except
import Data.Maybe


obviousTrue :: Expr -> Bool
obviousTrue = ignorePos $ \case
    ELitBool True -> True
    Not expr -> obviousFalse expr
    EAnd e1 e2 -> obviousTrue e1 && obviousTrue e2
    EOr e1 _ -> obviousTrue e1
    _ -> False

obviousFalse :: Expr -> Bool
obviousFalse = ignorePos $ \case
    ELitBool False -> True
    Not expr -> obviousTrue expr
    EAnd e1 _ -> obviousFalse e1
    EOr e1 e2 -> obviousFalse e1 && obviousFalse e2
    _ -> False


simplifyStmt :: Stmt -> Stmt
simplifyStmt stmt = case stmt ^. aa of
    BStmt bl -> let simplified = simplifyBlock bl in if null (simplified ^. aa . blockStmts)
        then makeAbs Empty
        else makeAbs $ BStmt simplified
    Cond expr sTrue
        | obviousTrue expr  -> sTrue
        | obviousFalse expr -> makeAbs Empty
        | otherwise         -> stmt
    CondElse expr sTrue sFalse
        | obviousTrue expr  -> sTrue
        | obviousFalse expr -> sFalse
        | otherwise         -> stmt
    While expr _
        | obviousFalse expr -> makeAbs Empty
        | otherwise         -> stmt
    _ -> stmt


simplifyBlock :: Block -> Block
simplifyBlock block = let newBlock = makeAbs $ Block $ cutAfterReturn (mapMaybe aux (block ^. aa . blockStmts)) in if newBlock == block
    then block
    else newBlock
  where
    aux stmt = let simplified = simplifyStmt stmt in if simplified ^. aa == Empty
        then Nothing
        else Just simplified

cutAfterReturn :: [Stmt] -> [Stmt]
cutAfterReturn stmts = reverse $ drop (aux stmts) $ reverse stmts
  where
    aux (x:xs) = case x ^. aa of
        Ret _ -> length xs
        VRet -> length xs
        _ -> aux xs
    aux [] = 0

simplifyTopDef :: TopDef -> TopDef
simplifyTopDef = over (aa . topDefBlock) simplifyBlock

simplifyProgram :: Program -> Program
simplifyProgram = forgetPos . over (aa . programTopDefs . traverse) simplifyTopDef


hasReturn :: TopDef -> Either String ()
hasReturn topDef = runExcept $ constrainReturn topDef (topDef ^. aa . topDefBlock)

constrainReturn :: TopDef -> Block -> Except String ()
constrainReturn fun block = aux (block ^. aa . blockStmts)
  where
    aux :: [Stmt] -> Except String ()
    aux (x:xs) = case x ^. aa of
        Ret _ -> return ()
        VRet -> return ()
        BStmt block -> constrainReturn fun block `catchError` (\_ -> aux xs)
        CondElse _ sTrue sFalse -> (aux [sTrue] >> aux [sFalse]) `catchError` const (aux xs)
        _ -> aux xs
    aux [] = throwError $ red ("Error in function " ++ absShow (fun ^. aa . topDefIdent) ++ " (" ++ fromJust (show <$> fun ^. pos) ++ "):") ++ " no return statement!"