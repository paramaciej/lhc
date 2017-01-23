{-# LANGUAGE LambdaCase #-}
module Asm.Operators where

import Control.Lens
import Control.Monad.State
import qualified Data.Set as S
import Data.Maybe

import qualified Quattro.Types as Q
import Asm.RegAlloc
import Asm.Utils


genBinOp :: OrdinaryOp -> Q.AliveSet -> Q.Address -> Q.Value -> Q.Value -> AllocM ()
genBinOp = \case
    Add -> commutativeOp Add
    Sub -> nonCommutativeOp Sub
  where
    commutativeOp op aliveAfter addr val1 val2 = case (val1, val2) of
        (Q.Location addr1, Q.Location addr2)
            | not (addrStayAlive addr2 aliveAfter) -> genBinOpWithReplacement addr val1 addr2 op
            | not (addrStayAlive addr1 aliveAfter) -> genBinOpWithReplacement addr val2 addr1 op
            | otherwise -> genBinOpWithMov addr op val1 val2
        (Q.Location argAddr, Q.Literal _)
            | not (addrStayAlive argAddr aliveAfter) -> genBinOpWithReplacement addr val2 argAddr op
            | otherwise -> genBinOpWithMov addr op val1 val2
        (Q.Literal _, Q.Location argAddr)
            | not (addrStayAlive argAddr aliveAfter) -> genBinOpWithReplacement addr val1 argAddr op
            | otherwise -> genBinOpWithMov addr op val1 val2
        (Q.Literal _, Q.Literal _) -> genBinOpWithMov addr op val1 val2
    nonCommutativeOp op aliveAfter addr dst src = case dst of
        Q.Location addrDst
            | not (addrStayAlive addrDst aliveAfter) -> genBinOpWithReplacement addr src addrDst op
            | otherwise -> genBinOpWithMov addr op src dst
        _ -> genBinOpWithMov addr op src dst


genBinOpWithReplacement :: Q.Address -> Q.Value -> Q.Address -> OrdinaryOp -> AllocM ()
genBinOpWithReplacement newAddr (Q.Location argAddr) destAddr op = do
    (vArg, lDest) <- atLeastOneReg argAddr destAddr
    asmStmts %= (++ [BinStmt op vArg lDest]) -- TODO czemu??? chyba jest ok
    replaceAddr destAddr newAddr
genBinOpWithReplacement newAddr (Q.Literal literal) destAddr op = do
    let vArg = IntLiteral literal
    lDest <- fastestReadLoc destAddr
    asmStmts %= (++ [BinStmt op vArg lDest])
    replaceAddr destAddr newAddr


genBinOpWithMov :: Q.Address -> OrdinaryOp -> Q.Value -> Q.Value -> AllocM ()
genBinOpWithMov addr op val1 val2 = do
    movValToAddrLocatedIn val2 addr S.empty
    genBinOpWithReplacement addr val1 addr op


genIMul :: Q.Address -> Q.Value -> Q.Value -> AllocM ()
genIMul addr val1 val2 = do -- addr = val1 * val2
    v1 <- fastestReadVal val1
    v2 <- fastestReadVal val2
    dstReg <- getFreeRegister
    let dstRegister = valueMatchRegister dstReg val1
    asmStmts %= (++ [Mov v1 (RegisterLoc dstRegister), IMul v2 dstRegister])
    registers . at dstReg .= Just (Just addr)
    stack . at addr .= Just (S.singleton (RegisterLoc dstRegister))

genIDiv :: Q.Address -> Q.Value -> Q.Value -> AllocM ()
genIDiv addr val1 val2 = do -- addr = val1 / val2
    forceFreeReg RAX
    forceFreeReg RDX

    let eaxLoc = RegisterLoc $ valueMatchRegister RAX val1
    registers . at RAX .= Just (Just addr)
    stack . at addr .= Just (S.singleton eaxLoc)

    registers . at RDX .= Just (Just addr) -- reserve EDX register
    v1 <- fastestReadVal val1
    l2 <- valAsLocation  val2
    registers .at RDX .= Just Nothing -- free EDX register
    asmStmts %= (++ [Mov v1 eaxLoc, CDQ, IDiv l2])

genIMod :: Q.Address -> Q.Value -> Q.Value -> AllocM ()
genIMod addr val1 val2 = do
    forceFreeReg RAX
    forceFreeReg RDX

    registers . at RDX .= Just (Just addr)
    stack . at addr .= Just (S.singleton (RegisterLoc $ valueMatchRegister RDX val1))

    registers . at RAX .= Just (Just addr) -- reserve EAX register
    v1 <- fastestReadVal val1
    l2 <- valAsLocation  val2
    registers . at RAX .= Just Nothing -- free EAX register
    asmStmts %= (++ [Mov v1 (RegisterLoc $ valueMatchRegister RAX val1), CDQ, IDiv l2])

genCmp :: Q.RelOp -> Q.Address -> Q.Value -> Q.Value -> AllocM ()
genCmp op addr val1 val2 = do
    dest <- fastestReadLoc addr
    (val, loc) <- atLeastOneRegFromValues val1 val2
    asmStmts %= (++ [Cmp val loc, Mov (IntLiteral 0) dest])
    free <- getFreeRegister
    dest <- fastestReadLoc addr
    let litLoc = RegisterLoc $ valueMatchRegister free (Q.Literal 1)
    asmStmts %= (++ [Mov (IntLiteral 1) litLoc, CondMov op litLoc dest])

genUni :: Q.UniOp -> Q.Address -> Q.Value -> AllocM ()
genUni op addr value = case op of
    Q.Not -> do
        v <- fastestReadVal value
        loc <- fastestReadLoc addr
        asmStmts %= (++ [Mov v loc, Xor (IntLiteral 1) loc])
    Q.Neg -> do
        v <- fastestReadVal value
        loc <- fastestReadLoc addr
        asmStmts %= (++ [Mov v loc, Not loc, BinStmt Add (IntLiteral 1) loc])

genCall :: Q.Address -> String -> [Q.Value] -> AllocM ()
genCall _ funName args = do
    let (argRegisters, restRegisters) = splitAt (length args) argRegs
    mapM_ callerSave $ [RAX, R10, R11] ++ restRegisters -- save caller-save registers -- TODO CALLERsave regs! uzupałenić
    mapM_ safeMoveToReg $ zip (take 6 args) argRegisters -- move arguments to registers

    when (length args > 6 && odd (length args)) $ asmStmts %= (++ [Custom $ align "subq" ++ "$8, %rsp"])
    forM_ (reverse $ drop 6 args) $ \arg -> do
        val <- fastestReadVal arg
        asmStmts %= (++ [Push val])
    asmStmts %= (++ [Call funName])

    let rspAdd = (length args - 6 + 1) `div` 2 * 16
    when (rspAdd > 0) $ asmStmts %= (++ [Custom $ align "addq" ++ "$" ++ show rspAdd ++ ", %rsp"])
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
