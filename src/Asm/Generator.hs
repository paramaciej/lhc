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
genAsm (Q.ClearProgram functions) = do
    (asm, roData) <- runStateT (concat <$> mapM genFunction (M.toAscList functions)) (RoDataSt False M.empty M.empty)
    let ro = [RoString 0 "\"\"" | roData ^. roEmptyString] ++ (M.elems . M.mapWithKey RoString) (roData ^. roStrings)
    let vtables = (M.elems . M.mapWithKey VTable . M.map M.elems) (roData ^. roVTables)
    let asmMod = if null vtables && null ro then id else ((SectionRoData : vtables ++ ro ++ [SectionText]) ++)
    return $ asmMod asm

genFunction :: (String, Q.ClearFunction) -> RoDataM [AsmStmt]
genFunction (funName, fun@(Q.ClearFunction entry blocks)) = do
    allStmts <- cons (Globl funName) . concat <$> mapM (genBlock fun) (M.toAscList blocksWithStringLabels)
    let usedCalleeSaveRegs = map (Register Q.Ptr) $ S.elems $ registersUsed allStmts  `S.intersection` S.fromList calleeSaveRegs
    let localsSlots = let locals = localsUsed allStmts in if even (length usedCalleeSaveRegs)
        then locals + locals `mod` 2
        else (locals + 1) - locals `mod` 2
    let rspShift = (localsUsed allStmts + 1) `div` 2 * 16
    unless (null usedCalleeSaveRegs) $ verbosePrint $ "Function " ++ green funName ++ " has used callee-save registers: "
        ++ intercalate ", " (map show usedCalleeSaveRegs)
    return $ concatMap (prologueEpilogue localsSlots usedCalleeSaveRegs) allStmts
  where
    inSets = calculateInSets fun
    labelMod label = if label == entry then (False, funName) else (True, show (ALabel label))
    blocksWithInSets = M.mapWithKey (\label block -> (block, inSets M.! label)) blocks
    blocksWithStringLabels = M.mapKeys labelMod blocksWithInSets
    prologueEpilogue localsSlots saveRegs stmt
        | stmt == Label funName =
            [ stmt
            , Custom $ align "pushq" ++ "%rbp"
            , Custom $ align "movq" ++ "%rsp, %rbp"
            ] ++
            [ Custom $ align "subq" ++ "$" ++ show (localsSlots * 8) ++ ", %rsp" | localsSlots > 0] ++
            map (Push . Location . RegisterLoc) saveRegs
        | stmt == LeaveRet = map (Pop . RegisterLoc) (reverse saveRegs) ++ [stmt]
        | otherwise = [stmt]

genBlock :: Q.ClearFunction -> ((Bool, String), (Q.ClearBlock, Q.AliveSet)) -> RoDataM [AsmStmt]
genBlock fun ((_, label), (block@(Q.ClearBlock _ out), thisInSet)) = do
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
        Q.IsMethod method mp -> forM_ (M.toAscList mp) $ \(cls, int) -> lift $ roVTables . at cls %= \case
            Nothing -> Just $ M.singleton int method
            Just vt -> Just $ M.insert int method vt
        Q.Mov addr val -> when (addrStayAlive addr $ stmtWithAlive ^. after) $ -- we do nothing when addr isn't alive after current statement
            M.lookup addr <$> use stack >>= \case
                Just realLocs -> movValToAddrLocatedIn val addr realLocs
                Nothing -> movValToAddrLocatedIn val addr S.empty
        Q.FunArg addr nr -> if nr < 6
            then do
                let reg = argRegs `genericIndex` nr
                registers . at reg .= Just (Just addr)
                stack . at addr .= Just (S.singleton (RegisterLoc $ addressMatchRegister reg addr))
            else stack . at addr .= Just (S.singleton (Stack (Q.typeToRegType (addr ^. Q.addressType)) (3 - nr))) -- we want to refer stack above RBP
        Q.BinStmt addr op val1 val2 -> case op of
            Q.Add -> case Q.valType val1 of
                Q.Int -> genBinOp Add (stmtWithAlive ^. after) addr val1 val2
                Q.Ptr -> call addr (Right "_concatString") [val1, val2]
            Q.Sub -> genBinOp Sub (stmtWithAlive ^. after) addr val1 val2
            Q.Mul -> genIMul addr val1 val2
            Q.Div -> genIDiv addr val1 val2
            Q.Mod -> genIMod addr val1 val2
        Q.CmpStmt addr op val1 val2 -> genCmp op addr val2 val1
        Q.UniStmt addr op value -> genUni op addr value
        Q.Call addr funName args -> call addr (Right funName) args
        Q.StringLit addr string -> do
            loc <- fastestReadLoc addr
            nr <- case string of
                Nothing -> do
                    already <- lift $ use roEmptyString
                    unless already $ lift $ roEmptyString .= True
                    return 0
                Just str -> do
                    strNumber <- (+1) . toInteger . length <$> lift (use roStrings)
                    lift $ roStrings %= M.insert strNumber str
                    return strNumber
            asmStmts %= (++ [Mov (RoDataLiteral $ "_STRING_" ++ show nr) loc])
        Q.New addr clsName size isClass -> when (addrStayAlive addr $ stmtWithAlive ^. after) $ do
            call addr (Right "_new") [Q.Literal 1, Q.Literal size]
            when isClass $ do
                RegisterLoc objReg <- movAddrToRegister addr
                asmStmts %= (++ [Mov (RoDataLiteral $ "_vtable_" ++ clsName ++ "_" ++ show (length clsName)) (Memory objReg 0)])
        Q.CallVirtual addr obj virtualNumber args -> do
            RegisterLoc objReg <- movAddrToRegister obj
            free <- getFreeRegister
            let freeRegister = addressMatchRegister free obj
            asmStmts %= (++ [Mov (Location $ Memory objReg 0) (RegisterLoc freeRegister)
                , Mov (Location $ Memory freeRegister virtualNumber) (RegisterLoc freeRegister)
                ])

            call addr (Left (RegisterLoc freeRegister)) (Q.Location obj : args)
        Q.SetAttr addr obj attrNumber val -> do
            RegisterLoc objReg <- movAddrToRegister obj
            value <- fastestReadVal val
            regValue <- case value of
                Location (Stack _ _) -> do
                    free <- getFreeRegister
                    let freeLoc = RegisterLoc $ valueMatchRegister free val
                    asmStmts %= (++ [Mov value freeLoc])
                    return $ Location freeLoc
                _ -> return value

            asmStmts %= (++ [ Mov regValue (Memory objReg (1 + attrNumber))])

            let val = Q.Location obj
            when (addrStayAlive addr $ stmtWithAlive ^. after) $
                M.lookup addr <$> use stack >>= \case
                    Just realLocs -> movValToAddrLocatedIn val addr realLocs
                    Nothing -> movValToAddrLocatedIn val addr S.empty

        Q.GetAttr addr obj attrNumber -> do
            RegisterLoc objReg <- movAddrToRegister obj
            loc <- fastestReadLoc addr
            asmStmts %= (++ [Mov (Location $ Memory objReg (1 + attrNumber)) loc])

    killDead stmtWithAlive
    showStmtGeneratedCode stmtWithAlive
    unlines . map show . drop prevStmts <$> use asmStmts >>= \case
        list@(_:_) -> verbosePrint $ init list
        [] -> return ()
  where
    call addr funName args = do
        genCall addr funName args
        when (addrStayAlive addr $ stmtWithAlive ^. after) $ do
            registers . at RAX .= Just (Just addr)
            stack . at addr .= Just (S.singleton $ RegisterLoc $ addressMatchRegister RAX addr)

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
