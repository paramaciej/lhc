{-# LANGUAGE LambdaCase #-}
module Asm.Generator where

import Asm.Operators
import Asm.RegAlloc
import Asm.Utils
import Quattro.Alive
import qualified Quattro.Types as Q
import Utils.Show
import Utils.Verbose

import Control.Lens
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

genAsm :: Q.ClearProgram -> CompilerOptsM [AsmStmt]
genAsm (Q.ClearProgram functions) = concat <$> mapM genFunction (M.toAscList functions)

genFunction :: (String, Q.ClearFunction) -> CompilerOptsM [AsmStmt]
genFunction (funName, fun@(Q.ClearFunction entry blocks)) = do -- TODO dobra kolejność bloków!
    allStmts <- cons (Globl funName) . concat <$> mapM (genBlock fun) (M.toAscList blocksWithStringLabels)
    let rspShift = (localsUsed allStmts + 1) `div` 2 * 16
    let (before, label:after) = break (== Label funName) allStmts
    return $ before ++
        [ label
        , Custom $ align "pushq" ++ "%rbp"
        , Custom $ align "movq" ++ "%rsp, %rbp"
        ] ++ [Custom $ align "subq" ++ "$" ++ show rspShift ++ ", %rsp" | rspShift > 0] ++ after
  where
    inSets = calculateInSets fun
    labelMod label = if label == entry then (True, funName) else (False, show (ALabel label))
    blocksWithInSets = M.mapWithKey (\label block -> (block, inSets M.! label)) blocks
    blocksWithStringLabels = M.mapKeys labelMod blocksWithInSets

genBlock :: Q.ClearFunction -> ((Bool, String), (Q.ClearBlock, Q.AliveSet)) -> CompilerOptsM [AsmStmt]
genBlock fun ((isEntry, label), (block@(Q.ClearBlock _ out), thisInSet)) = do
    verbosePrint $ green "\nBLOCK " ++ label
    fromStmts     <- execStateT (genAndAllocBlock withAlive) (initialAllocSt thisInSet)
    stackRestored <- execStateT (restoreStack outSet) fromStmts
    fromOut       <- execStateT (genAndAllocEscape newOut) (initialAllocSt outSet)
    let ro = [RoString 0 "\"\"" | stackRestored ^. roEmptyString] ++ (M.elems . M.mapWithKey RoString) (stackRestored ^. roStrings)
    let roMod = if null ro then id else ((SectionRoData : ro ++ [SectionText]) ++)

    return $ roMod $ Label label : stackRestored ^. asmStmts ++ fromOut ^. asmStmts
  where
    inSets = calculateInSets fun
    outSet = setFromOut inSets out
    withAlive = stmtsWithAlive inSets block
    newOut = outWithAlive inSets out


genAndAllocBlock :: [StmtWithAlive] -> AllocM ()
genAndAllocBlock = mapM_ genAndAllocStmt

genAndAllocStmt :: StmtWithAlive -> AllocM ()
genAndAllocStmt stmtWithAlive = do
    prevStmts <- length <$> use asmStmts
    case stmtWithAlive ^. stmt of
        Q.Mov addr val -> when (addrStayAlive addr $ stmtWithAlive ^. after) $ -- we do nothing when addr isn't alive after current statement
            M.lookup addr <$> use stack >>= \case
                Just realLocs -> movValToAddrLocatedIn val addr realLocs
                Nothing -> movValToAddrLocatedIn val addr S.empty
        Q.FunArg addr nr -> if nr < 6
            then do
                let reg = argRegs `genericIndex` nr
                registers . at reg .= Just (Just addr)
                stack . at addr .= Just (S.singleton (RegisterLoc $ addressMatchRegister reg addr))
            else stack . at addr .= Just (S.singleton (Stack (addr ^. Q.addressType) (3 - nr))) -- we want to refer stack above RBP
        Q.BinStmt addr op val1 val2 -> case op of
            Q.Add -> genBinOp Add (stmtWithAlive ^. after) addr val1 val2
            Q.Sub -> genBinOp Sub (stmtWithAlive ^. after) addr val1 val2
            Q.Mul -> genIMul addr val1 val2
            Q.Div -> genIDiv addr val1 val2
            Q.Mod -> genIMod addr val1 val2
        Q.CmpStmt addr op val1 val2 -> genCmp op addr val2 val1
        Q.UniStmt addr op value -> genUni op addr value
        Q.Call addr funName args -> do
            genCall addr funName args
            when (addrStayAlive addr $ stmtWithAlive ^. after) $ do
                registers . at RAX .= Just (Just addr)
                stack . at addr .= Just (S.singleton $ RegisterLoc $ addressMatchRegister RAX addr)
        Q.StringLit addr string -> do
            loc <- fastestReadLoc addr
            nr <- case string of
                Nothing -> do
                    already <- use roEmptyString
                    unless already $ roEmptyString .= True
                    return 0
                Just str -> do
                    strNumber <- (+1) . toInteger . length <$> use roStrings
                    roStrings %= M.insert strNumber str
                    return strNumber
            asmStmts %= (++ [Mov (StrLiteral nr) loc])
    killDead stmtWithAlive
    showStmtGeneratedCode stmtWithAlive
    unlines .map show . drop prevStmts <$> use asmStmts >>= \case
        list@(_:_) -> lift $ verbosePrint $ init list
        [] -> return ()

genAndAllocEscape :: Out -> AllocM ()
genAndAllocEscape (Goto next) = do
    fixStack (next ^. afterLabel)
    asmStmts %= (++ [Jmp (show $ next ^. aliveLabel)])
genAndAllocEscape (Branch nextTrue nextFalse val) = do
    let tmpLabel = "_FALSE" ++ show (nextFalse ^. aliveLabel)
    cmpArg <- valAsLocation val
    asmStmts %= (++ [Cmp (IntLiteral 0) cmpArg, Jz tmpLabel])
    fixStack (nextTrue ^. afterLabel)
    asmStmts %= (++ [Jmp (show $ nextTrue ^. aliveLabel), Label tmpLabel])
    fixStack (nextFalse ^. afterLabel)
    asmStmts %= (++ [Jmp (show $ nextFalse ^. aliveLabel)])
genAndAllocEscape (Ret val) = do
    source <- fastestReadVal val
    let rax = RegisterLoc $ valueMatchRegister RAX val
    asmStmts %= (++ [Mov source rax, LeaveRet])
genAndAllocEscape VRet = asmStmts %= (++ [LeaveRet])
