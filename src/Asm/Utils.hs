{-# LANGUAGE LambdaCase #-}
module Asm.Utils where

import Asm.RegAlloc
import qualified Quattro.Types as Q
import qualified Utils.Abstract as A
import Utils.Show
import Utils.Verbose

import Control.Lens
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Printf
import System.Console.ANSI

addrStayAlive :: Q.Address -> Q.AliveSet -> Bool
addrStayAlive addr aliveSet = addr `S.member` aliveSet

fastestReadLoc :: Q.Address -> AllocM RealLoc
fastestReadLoc addr = use (stack . at addr) >>= \case
    Just realLocs -> case S.elems (S.filter isRegLoc realLocs) of
        reg:_ -> return reg
        [] -> case S.elems realLocs of
            loc:_ -> return loc
            [] -> error $ "EMPTY VALUE SET FOR LOC " ++ show addr
    Nothing -> do
        reg <- getFreeRegister
        registers . at reg .= Just (Just addr)
        let loc = RegisterLoc $ addressMatchRegister reg addr
        stack . at addr .= Just (S.singleton loc)
        return loc

fastestReadVal :: Q.Value -> AllocM Value
fastestReadVal (Q.Literal literal) = return $ IntLiteral literal
fastestReadVal (Q.Location addr) = Location <$> fastestReadLoc addr
fastestReadVal (Q.Null _) = return $ IntLiteral 0 -- TODO czym ma być null

valAsLocation :: Q.Value -> AllocM RealLoc
valAsLocation (Q.Location addr) = fastestReadLoc addr
valAsLocation val@(Q.Literal literal) = do
    reg <- getFreeRegister
    let loc = RegisterLoc $ valueMatchRegister reg val
    asmStmts %= (++ [Mov (IntLiteral literal) loc])
    return loc
valAsLocation (Q.Null typ) = undefined -- undefined

atLeastOneRegFromValues :: Q.Value -> Q.Value -> AllocM (Value, RealLoc)
atLeastOneRegFromValues qVal1 qVal2 = do
    val1 <- fastestReadVal qVal1
    case qVal2 of
        Q.Literal _ -> do
            loc2 <- valAsLocation qVal2
            return (val1, loc2)
        Q.Location addr -> case val1 of
            Location (Stack _ _) -> do
                newLoc <- movAddrToRegister addr
                return (val1, newLoc)
            _ -> do
                loc2 <- fastestReadLoc addr
                return (val1, loc2)
        Q.Null _ -> undefined -- undefined

atLeastOneReg :: Q.Address -> Q.Address -> AllocM (Value, RealLoc)
atLeastOneReg addr1 addr2 = do
    val1@(Location loc1) <- fastestReadVal (Q.Location addr1)
    (Location loc2)      <- fastestReadVal (Q.Location addr2)
    case (loc1, loc2) of
        (RegisterLoc _, _) -> return (val1, loc2)
        (Stack _ _, RegisterLoc _) -> return (val1, loc2)
        (Memory _ _, RegisterLoc _) -> return (val1, loc2)
        (_, _) -> do
            newLoc <- movAddrToRegister addr2
            return (val1, newLoc)

replaceAddr :: Q.Address -> Q.Address -> AllocM ()
replaceAddr old new = do
    realLocs <- use $ stack . at old . _Just
    stack . at old .= Nothing
    stack . at new .= Just realLocs
    registers %= M.map (fmap (\addr -> if addr == old then new else addr))


movAddrToRegister :: Q.Address -> AllocM RealLoc
movAddrToRegister addr = do
    initialVal@(Location loc) <- fastestReadVal (Q.Location addr)
    case loc of
        RegisterLoc _ -> return loc
        Stack _ _ -> do
            free <- getFreeRegister
            let newLoc = RegisterLoc $ addressMatchRegister free addr
            asmStmts %= (++ [Mov initialVal newLoc])
            registers . at free .= Just (Just addr)
            stack . at addr . _Just %= S.insert newLoc
            return newLoc
        Memory _ _ -> undefined -- undefined

getFreeRegister :: AllocM Reg
getFreeRegister = (M.keys . M.filter isNothing) <$> use registers >>= \case
    freeReg:_ -> return freeReg
    [] -> do
        ss <- M.map (S.partition isRegLoc) <$> use stack
        case M.assocs $ M.filter (\(reg, stack) -> not (S.null reg || S.null stack)) ss of
            (addr, (regLocs, _)):_ -> do
                let regLoc:_ = S.elems regLocs
                let RegisterLoc (Register _ reg) = regLoc
                stack . at addr . _Just %= S.delete regLoc
                registers . at reg .= Just Nothing
                return reg
            [] -> do
                oldAddr <- fromJust . fromJust <$> use (registers . at RDX)
                stackLoc <- getFreeStackForType (oldAddr ^. Q.addressType)
                source@(Location regLoc) <- fromJust <$> valueFromReg RDX
                asmStmts %= (++ [Mov source stackLoc])
                stack . at oldAddr . _Just %= S.delete regLoc . S.insert stackLoc
                registers . at RDX .= Just Nothing
                return RDX

getFreeStackForType :: A.Type -> AllocM RealLoc
getFreeStackForType = getFreeStack . Q.typeToRegType

getFreeStack :: Q.RegType -> AllocM RealLoc
getFreeStack typ = do
    allLocs <- (S.unions . M.elems) <$> use stack
    let stackLocks = S.map (\(Stack _ x) -> x) $ S.filter (not . isRegLoc) allLocs
    return $ Stack typ $ if S.null stackLocks
        then 0
        else S.findMin $ S.fromList [0..(S.findMax stackLocks + 1)] S.\\ stackLocks


valueMatchRegister :: Reg -> Q.Value -> Register
valueMatchRegister reg (Q.Literal _) = Register Q.Int reg
valueMatchRegister reg (Q.Location addr) = addressMatchRegister reg addr
valueMatchRegister reg (Q.Null _) = Register Q.Ptr reg

addressMatchRegister :: Reg -> Q.Address -> Register
addressMatchRegister reg addr = Register (Q.typeToRegType $ addr ^. Q.addressType) reg

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

fixStack :: Q.AliveSet -> AllocM ()
fixStack alive = do
    perms <- fixStmts alive
    mapM_ fix perms
  where
    fix :: StackFixStmt -> AllocM ()
    fix (addr, from, to) = do
        let rax = RegisterLoc $ addressMatchRegister RAX addr
        let t = Q.typeToRegType $ addr ^. Q.addressType
        asmStmts %= (++ [Mov (Location (Stack t from)) rax, Mov (Location rax) (Stack t to)])

movValToAddrLocatedIn :: Q.Value -> Q.Address -> S.Set RealLoc -> AllocM ()
movValToAddrLocatedIn val addr realLocs = do
    destReg <- case S.elems (S.filter isRegLoc realLocs) of
        [] -> getFreeRegister
        RegisterLoc (Register _ reg):_ -> return reg
        _ -> error "impossible happened"

    setRegisterToAddr destReg addr
    source <- fastestReadVal val
    Location dest <- fromJust <$> valueFromReg destReg
    asmStmts %= (++ [Mov source dest])

restoreStack :: Q.AliveSet -> AllocM ()
restoreStack alive = do
    mapM_ moveToStackAndForget $ S.elems alive

    ss <- use stack
    let graph = M.fromList $ zip [0..] $ map (getStackFromAddr ss) (S.elems alive)
    let addrMap = M.fromList $ zip [0..] (S.elems alive)
    let stType st = Q.typeToRegType $ (addrMap M.! st) ^. Q.addressType
    let movStToReg st reg = Mov (Location $ Stack (stType st) st) (RegisterLoc $ addressMatchRegister reg (addrMap M.! st))
    let movRegToSt reg st = Mov (Location $ RegisterLoc $ addressMatchRegister reg (addrMap M.! st)) (Stack (stType st) st)

    forM_ (getCycles graph) $ \cycle -> asmStmts %= (
        ++ [movStToReg (last cycle) RAX]
        ++ concatMap (\(from, to) -> [movStToReg from RDX, movRegToSt RDX to]) (reverse $ pairs cycle)
        ++ [movRegToSt RAX (head cycle)])
  where
    getStackFromAddr ss = (\(Stack _ x) -> x) . S.findMin . S.filter (not . isRegLoc) . fromJust . (`M.lookup` ss)
    getCycles graph = filter ((>1) . length) $ snd $ M.foldrWithKey aux (graph, []) graph
    aux before _ (graph, cycles) = if before `M.member` graph
        then let (newGraph, path) = run before (graph, []) in (newGraph, path:cycles)
        else (graph, cycles)
    run start (graph, path) = case start `M.lookup` graph of
        Just next -> run next (M.delete start graph, start:path)
        Nothing -> (graph, path)
    pairs (x:y:ys) = (x,y) : pairs (y:ys)
    pairs [_] = []
    pairs [] = []

moveToStackAndForget :: Q.Address -> AllocM ()
moveToStackAndForget addr = do
    (regLocs, stackLocs) <- S.partition isRegLoc . fromJust <$> use (stack . at addr)
    case S.elems stackLocs of
        loc:_ -> stack . at addr .= Just (S.singleton loc)
        [] -> do
            let minRegLoc@(RegisterLoc (Register typ _)) = S.findMin regLocs
            stackLoc <- getFreeStack typ
            asmStmts %= (++ [Mov (Location minRegLoc) stackLoc])
            stack . at addr .= Just (S.singleton stackLoc)
    let regs = S.map (\(RegisterLoc (Register _ reg)) -> reg) regLocs
    registers %= M.mapWithKey (\r a -> if r `S.member` regs then Nothing else a)
    return ()

forceFreeReg :: Reg -> AllocM ()
forceFreeReg reg = fromJust <$> use (registers . at reg) >>= \case
    Just addr -> moveToStackAndForget addr
    Nothing -> return ()

killDead :: StmtWithAlive -> AllocM ()
killDead stmtWithAlive = do
    let alive = stmtWithAlive ^. after
    registers . traverse %= \case
        Just x -> if x `S.member` alive then Just x else Nothing
        Nothing -> Nothing
    stack %= M.filterWithKey (\a _ -> a `S.member` alive)

localsUsed :: [AsmStmt] -> Integer
localsUsed stmts = if S.null locals then 0 else S.findMax locals + 1
  where
    locals = foldr (stmtInfoExtractor fromVal fromLoc (const id)) S.empty stmts
    fromVal (IntLiteral _)  = id
    fromVal (StrLiteral _)  = id
    fromVal (Location loc)  = fromLoc loc
    fromLoc (RegisterLoc _) = id
    fromLoc (Stack _ i)     = S.insert i
    fromLoc (Memory _ _)    = id

registersUsed :: [AsmStmt] -> S.Set Reg
registersUsed = foldr (stmtInfoExtractor fromVal fromLoc fromReg) S.empty
  where
    fromVal (IntLiteral _)      = id
    fromVal (StrLiteral _)      = id
    fromVal (Location loc)      = fromLoc loc
    fromLoc (RegisterLoc reg)   = fromReg reg
    fromLoc (Stack _ _)         = id
    fromLoc (Memory reg _)      = fromReg reg
    fromReg (Register _ r)      = S.insert r

stmtInfoExtractor :: (Value -> a -> a) -> (RealLoc -> a -> a) -> (Register -> a -> a) -> AsmStmt -> (a -> a)
stmtInfoExtractor valMod locMod regMod = \case
    Mov v l             -> valMod v . locMod l
    Cmp v l             -> valMod v . locMod l
    BinStmt _ v l       -> valMod v . locMod l
    IMul v r            -> valMod v . regMod r
    IDiv l              -> locMod l
    CDQ                 -> id
    Xor v l             -> valMod v . locMod l
    Not l               -> locMod l
    CondMov _ l1 l2     -> locMod l1 . locMod l2
    Jmp _               -> id
    Jz _                -> id
    Push v              -> valMod v
    Pop l               -> locMod l
    Call _              -> id
    LeaveRet            -> id
    Label _             -> id
    Globl _             -> id
    Custom _            -> id
    RoString _ _        -> id
    SectionRoData       -> id
    SectionText         -> id

registersAndStackInfo :: AllocM String
registersAndStackInfo = do
    regs <- M.elems <$> use registers
    ss <- M.assocs <$> use stack
    let regInfo = intercalate "|" $ map (maybe " - " (printf "%3d" . (^. Q.addressLoc))) regs
    let stackInfo = intercalate " |" $ map addrInfo ss
    return $ regInfo ++ red "|" ++ stackInfo
  where
    addrInfo (addr, set) = yellow (printf "%3d" $ addr ^. Q.addressLoc) ++ ": " ++ intercalate ", " (map showRealLoc $ S.elems set)
    showRealLoc (RegisterLoc r) = show r
    showRealLoc (Stack _ i)     = printf "%4s" $ "St" ++ show i
    showRealLoc (Memory r i)    = show i ++ show r


showRegistersAndStack :: AllocM ()
showRegistersAndStack = registersAndStackInfo >>= verbosePrint

showStmtGeneratedCode :: StmtWithAlive -> AllocM ()
showStmtGeneratedCode stmtWithAlive = do
    let s = show (stmtWithAlive ^. stmt) ++ setCursorColumnCode 30
    info <- registersAndStackInfo
    verbosePrint $ s ++ red "|" ++ info ++ red " | " ++ "alive: " ++ show (S.elems (stmtWithAlive ^. after))