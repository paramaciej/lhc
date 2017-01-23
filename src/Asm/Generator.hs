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
        , Custom "  push \t%rbp"
        , Custom "  movq \t%rsp, %rbp"
        ] ++ [Custom $ "  subq \t$" ++ show rspShift ++ ", %rsp" | rspShift > 0] ++ after
  where
    inSets = calculateInSets fun
    labelMod label = if label == entry then funName else show (ALabel label)
    blocksWithInSets = M.mapWithKey (\label block -> (block, inSets M.! label)) blocks
    blocksWithStringLabels = M.mapKeys labelMod blocksWithInSets

genBlock :: Q.ClearFunction -> (String, (Q.ClearBlock, Q.AliveSet)) -> CompilerOptsM [AsmStmt]
genBlock fun (label, (block@(Q.ClearBlock _ out), thisInSet)) = do
    verbosePrint $ green "\nBLOCK " ++ label
    fromStmts     <- execStateT (genAndAllocBlock withAlive) (initialAllocSt thisInSet)
    stackRestored <- execStateT (restoreStack outSet) fromStmts
    fromOut       <- execStateT (genAndAllocEscape newOut) (initialAllocSt outSet)
    return $ Label label : stackRestored ^. asmStmts ++ fromOut ^. asmStmts
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
        Q.BinStmt addr op val1 val2 -> case op of
            Q.Add -> genBinOp Add (stmtWithAlive ^. after) addr val1 val2
            Q.Sub -> genBinOp Sub (stmtWithAlive ^. after) addr val1 val2
            Q.Mul -> genIMul addr val1 val2
            Q.Div -> genIDiv addr val1 val2
            Q.Mod -> genIMod addr val1 val2
        Q.CmpStmt addr op val1 val2 -> genCmp op addr val2 val1 -- TODO zamieniona kolejność arg, czy dobrze?
        Q.Call addr funName args -> do
            genCall addr funName args
--             when (addrStayAlive addr $ stmtWithAlive ^. after) $ TODO conditional EAX/RAX -> addr

--             x <- get
--             lift $ verbosePrint $ "TEST:\n" ++ show x



        _ -> return () -- TODO TODO TODO !!!
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
    asmStmts %= (++ [Cmp (Literal 0) cmpArg, Jz tmpLabel])
    fixStack (nextTrue ^. afterLabel)
    asmStmts %= (++ [Jmp (show $ nextTrue ^. aliveLabel), Label tmpLabel])
    fixStack (nextFalse ^. afterLabel)
    asmStmts %= (++ [Jmp (show $ nextFalse ^. aliveLabel)])
genAndAllocEscape (Ret val) = do
    source <- fastestReadVal val
    let rax = RegisterLoc $ valueMatchRegister RAX val
    asmStmts %= (++ [Mov source rax, LeaveRet])
genAndAllocEscape VRet = asmStmts %= (++ [LeaveRet])
