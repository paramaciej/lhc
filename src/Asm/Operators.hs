{-# LANGUAGE LambdaCase #-}
module Asm.Operators where

import Control.Lens
import Control.Monad.State
import qualified Data.Set as S
import Data.Maybe

import qualified Quattro.Types as Q
import Asm.RegAlloc
import Asm.Utils


genBinOp :: Q.BinOp -> Q.AliveSet -> Q.Address -> Q.Value -> Q.Value -> AllocM ()
genBinOp = \case
    Q.Add -> commutativeOp Q.Add
    Q.Sub -> nonCommutativeOp Q.Sub
    Q.Mul -> commutativeOp Q.Mul
    _ -> (\_ _ _ _ -> return ())
  where
    commutativeOp op aliveAfter addr val1 val2 = case (val1, val2) of
        (Q.Location addr1, Q.Location addr2)
            | not (addrStayAlive addr2 aliveAfter) -> genBinOpWithReplacement addr val1 addr2 op
            | not (addrStayAlive addr1 aliveAfter) -> genBinOpWithReplacement addr val2 addr1 op
            | otherwise -> genBinOpWithMov addr op val1 val2
        (Q.Location argAddr, Q.Literal lit)
            | not (addrStayAlive argAddr aliveAfter) -> genBinOpWithReplacement addr val2 argAddr op
            | otherwise -> genBinOpWithMov addr op val1 val2
        (Q.Literal lit, Q.Location argAddr)
            | not (addrStayAlive argAddr aliveAfter) -> genBinOpWithReplacement addr val1 argAddr op
            | otherwise -> genBinOpWithMov addr op val1 val2
        (Q.Literal _, Q.Literal _) -> genBinOpWithMov addr op val1 val2
    nonCommutativeOp op aliveAfter addr dst src = case dst of
        Q.Location addrDst
            | not (addrStayAlive addrDst aliveAfter) -> genBinOpWithReplacement addr src addrDst op
            | otherwise -> genBinOpWithMov addr op src dst
        _ -> genBinOpWithMov addr op src dst


genBinOpWithReplacement :: Q.Address -> Q.Value -> Q.Address -> Q.BinOp -> AllocM ()
genBinOpWithReplacement newAddr (Q.Location argAddr) destAddr op = do
    (vArg, lDest) <- atLeastOneReg argAddr destAddr
    asmStmts %= (++ [BinStmt op vArg lDest]) -- TODO czemu??? chyba jest ok
    replaceAddr destAddr newAddr
genBinOpWithReplacement newAddr (Q.Literal literal) destAddr op = do
    let vArg = Literal literal
    lDest <- fastestReadLoc destAddr
    asmStmts %= (++ [BinStmt op vArg lDest])
    replaceAddr destAddr newAddr


genBinOpWithMov :: Q.Address -> Q.BinOp -> Q.Value -> Q.Value -> AllocM ()
genBinOpWithMov addr op val1 val2 = do
    movValToAddrLocatedIn val2 addr S.empty
    genBinOpWithReplacement addr val1 addr op

genCmp :: Q.RelOp -> Q.Address -> Q.Value -> Q.Value -> AllocM ()
genCmp op addr val1 val2 = do
    dest <- fastestReadLoc addr
    (val, loc) <- atLeastOneRegFromValues val1 val2
    asmStmts %= (++ [Cmp val loc, Mov (Literal 0) dest])
    free <- getFreeRegister
    dest <- fastestReadLoc addr
    let litLoc = RegisterLoc $ valueMatchRegister free (Q.Literal 1)
    asmStmts %= (++ [Mov (Literal 1) litLoc, CondMov op litLoc dest])

genCall :: Q.Address -> String -> [Q.Value] -> AllocM ()
genCall addr funName args = do
    let (argRegisters, restRegisters) = splitAt (length args) [RDI, RSI, RDX] -- TODO add 6 arg registers!
    mapM_ callerSave $ [RAX, RDX, RSI, RDI] ++ restRegisters -- save caller-save registers
    mapM_ safeMoveToReg $ zip (take 6 args) argRegisters -- move arguments to registers
    -- TODO (drop 6 args) -- na stack, oraz: wyrównanie stosu
    asmStmts %= (++ [Call funName])
  where
    callerSave reg = use (registers . at reg) >>= \case
        Just (Just addr) -> moveToStackAndForget addr
        _ -> return ()
    safeMoveToReg :: (Q.Value, Reg) -> AllocM ()
    safeMoveToReg (val, reg) = fromJust <$> use (registers . at reg) >>= \case
        Nothing -> emitMov val reg
        Just addr -> case val of
            Q.Location valAddr -> unless (valAddr == addr) $ moveToStackAndForget addr >> emitMov val reg
            Q.Literal _ -> moveToStackAndForget addr >> emitMov val reg
    emitMov val reg = do
        srcVal <- fastestReadVal val
        asmStmts %= (++ [Mov srcVal (RegisterLoc $ valueMatchRegister reg val)])