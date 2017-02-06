{-# LANGUAGE LambdaCase #-}

module Utils.Eval where

import Utils.Abstract
import Utils.Show

import Control.Lens hiding (Empty)
import Control.Monad.Except
import Data.Either
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
        | obviousTrue expr  -> makeAbs $ BStmt sTrue
        | obviousFalse expr -> makeAbs Empty
        | otherwise         -> stmt
    CondElse expr sTrue sFalse
        | obviousTrue expr  -> makeAbs $ BStmt sTrue
        | obviousFalse expr -> makeAbs $ BStmt sFalse
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

simplifyFnDef :: FnDef -> FnDef
simplifyFnDef td = let simplified = over (aa . fnDefBlock) simplifyBlock td
    in if simplified ^. aa . fnDefType == makeAbs Void && isLeft (fnHasReturn simplified)
        then simplified & aa . fnDefBlock . aa . blockStmts %~ (++ [makeAbs VRet])
        else simplified

simplifyClsStmt :: ClassStmt -> ClassStmt
simplifyClsStmt clsStmt = case clsStmt ^. aa of
    Attr _ _ -> clsStmt
    Method{} -> if methodIsVoid && isLeft (methodHasReturn simplified)
        then simplified & aa . singular methodBlock . aa . blockStmts %~ (++ [makeAbs VRet])
        else simplified
      where
        simplified = over (aa . singular methodBlock) simplifyBlock clsStmt
        methodIsVoid = simplified ^. aa . singular methodRetType == makeAbs Void

simplifyProgram :: Program -> Program
simplifyProgram = forgetPos
    . over (aa . programFunctions . traverse) simplifyFnDef
    . over (aa . programClasses . traverse . aa . clsDefBody . aa . classBodyStmts . traverse) simplifyClsStmt


fnHasReturn :: FnDef -> Either String ()
fnHasReturn fnDef = runExcept $ constrainReturn fnDef (fnDef ^. aa . fnDefIdent) "function" (fnDef ^. aa . fnDefBlock)

methodHasReturn :: ClassStmt -> Either String ()
methodHasReturn clsStmt = runExcept $ constrainReturn clsStmt (clsStmt ^. aa . singular methodName) "method" (clsStmt ^. aa . singular methodBlock)

constrainReturn :: AbsPos a -> Ident -> String -> Block -> Except String ()
constrainReturn context ident display block = aux (block ^. aa . blockStmts)
  where
    constrain = constrainReturn context ident display
    aux :: [Stmt] -> Except String ()
    aux (x:xs) = case x ^. aa of
        Ret _ -> return ()
        VRet -> return ()
        BStmt block -> constrain block `catchError` (\_ -> aux xs)
        CondElse _ bTrue bFalse -> (constrain bTrue >> constrain bFalse) `catchError` const (aux xs)
        _ -> aux xs
    aux [] = throwError $ red ("Error in " ++ display ++ " " ++ absShow ident ++ " (" ++ fromJust (show <$> context ^. pos) ++ "):") ++ " no return statement!"