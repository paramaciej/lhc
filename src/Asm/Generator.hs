{-# LANGUAGE LambdaCase #-}
module Asm.Generator where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Lens

import Asm.RegAlloc
import Quattro.Alive
import qualified Quattro.Types as Q
import Utils.Verbose


genAsm :: Q.ClearProgram -> CompilerOptsM [AsmStmt]
genAsm (Q.ClearProgram functions) = concat <$> mapM genFunction (M.toAscList functions)

genFunction :: (String, Q.ClearFunction) -> CompilerOptsM [AsmStmt]
genFunction (funName, fun@(Q.ClearFunction entry blocks)) = -- TODO dobra kolejność bloków!
    cons (Globl funName) . concat <$> mapM (genBlock fun) (M.toAscList $ blocksWithStringLabels)
  where
    inSets = calculateInSets fun
    blocksWithInSets = M.mapWithKey (\label block -> (block, inSets M.! label)) blocks
    blocksWithStringLabels = M.mapKeys (\label -> if label == entry then funName else show label) blocksWithInSets

genBlock :: Q.ClearFunction -> (String, (Q.ClearBlock, Q.AliveSet)) -> CompilerOptsM [AsmStmt]
genBlock fun (label, (block@(Q.ClearBlock stmts out), thisInSet)) = do
    allocSt <- execStateT (genAndAllocBlock withAlive) (initialAllocSt thisInSet)
    return $ Label label : allocSt ^. asmStmts -- TODO chyba już nie
  where
    inSets = calculateInSets fun
    withAlive = stmtsWithAlive inSets fun block


genAndAllocBlock :: [StmtWithAlive] -> AllocM ()
genAndAllocBlock (head:tail) = case head ^. stmt of
    Q.Mov addr val -> when (addr `S.member` (head ^. after)) $ -- we do nothing when addr isn't alive after current statement
        M.lookup addr <$> use stack >>= \case
            Just realLocs -> movValToAddrLocatedIn val addr realLocs
            Nothing -> movValToAddrLocatedIn val addr S.empty -- TODO chyba ok, sprawdzić czt to działa
    _ -> return () -- TODO TODO TODO !!!
genAndAllocBlock [] = return () -- TODO TODO TODO

movValToAddrLocatedIn :: Q.Value -> Q.Address -> S.Set RealLoc -> AllocM ()
movValToAddrLocatedIn val addr realLocs = do
    destReg <- case S.elems (S.filter isRegLoc realLocs) of
        [] -> getFreeRegister
        RegisterLoc (Register _ reg):_ -> return reg

    setRegisterToAddr destReg addr
    source <- fastestReadVal val
    dest <- fromJust <$> valueFromReg destReg
    asmStmts %= (++ [Mov source dest])


-- utils

fastestReadVal :: Q.Value -> AllocM Value
fastestReadVal (Q.Literal literal) = return $ Literal literal
fastestReadVal (Q.Location addr) = use (stack . at addr) >>= \case
    Just realLocs -> case S.elems (S.filter isRegLoc realLocs) of
        reg:_ -> return $ Location reg
        [] -> case S.elems realLocs of
            loc:_ -> return $ Location loc
            [] -> error $ "EMPTY VALUE SET FOR LOC " ++ show addr
    Nothing -> error $ "ALIVE LOC " ++ show addr ++ " HASN'T GOT ANY VALUE"

getFreeRegister :: AllocM Reg
getFreeRegister = (M.keys . M.filter isNothing) <$> use registers >>= \case
    freeReg:_ -> return freeReg
    [] -> do
        x <- use stack
        let ok = M.assocs $ M.filter (\(reg, stack) -> not (S.null reg || S.null stack)) $ M.map (S.partition isRegLoc) x
        case ok of
            (addr, (regLocs, stackLocs)):_ -> do
                let regLoc:_ = S.elems regLocs
                let RegisterLoc (Register _ reg) = regLoc
                stack . at addr . _Just %= S.delete regLoc
                registers . at reg .= Just Nothing
                return reg
            [] -> do
                stackLoc <- getFreeStack
                source <- fromJust <$> valueFromReg RAX
                asmStmts %= (++ [Mov source (Location stackLoc)])
                return RAX

getFreeStack :: AllocM RealLoc
getFreeStack = do
    allLocs <- (S.unions . M.elems) <$> use stack
    let stackLocks = S.map (\(Stack x) -> x) $ S.filter (not . isRegLoc) allLocs
    return $ Stack $ if S.null stackLocks
        then 0
        else S.findMin $ S.fromList [0..(S.findMax stackLocks + 1)] S.\\ stackLocks




addressMatchRegister :: Reg -> Q.Address -> Register
addressMatchRegister reg addr = Register Int reg -- TODO WIELKOSC DANYCH!

valueFromReg :: Reg -> AllocM (Maybe Value)
valueFromReg reg = do
    mAddr <- fromJust <$> use (registers . at reg)
    return $ (Location . RegisterLoc . addressMatchRegister reg) <$> mAddr

setRegisterToAddr :: Reg -> Q.Address -> AllocM ()
setRegisterToAddr destReg addr = do
    mPrev <- use (stack . at addr)
    let previous = (map (\(RegisterLoc register) -> register ^. registerReg) . S.elems . S.filter isRegLoc) <$> mPrev
    mapM_ (\r -> registers . at r .= Just Nothing) (fromMaybe [] previous)
    stack . at addr .= Just (S.singleton (RegisterLoc $ addressMatchRegister destReg addr))
    registers . at destReg .= Just (Just addr)
