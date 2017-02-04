{-# LANGUAGE LambdaCase #-}
module Quattro.Alive where

import Quattro.Types

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S


clearProgram :: ProgramCode -> ClearProgram
clearProgram program =
    evalState (performProgramClear program) maxLabel
  where
    maxLabel = maximum $ M.mapMaybe (\fun -> fst <$> lookupMax (fun ^. blocks)) (program ^. functions)

    performProgramClear :: ProgramCode -> State Label ClearProgram
    performProgramClear program = do
        clearedFunctions <- mapM clearFunction (program ^. functions)
        return $ ClearProgram clearedFunctions

clearFunction :: FunctionCode -> State Label ClearFunction
clearFunction fCode = do
    clearedBlocks <- foldM genClearBlocks M.empty (M.toList $ fCode ^. blocks)
    let blocks = mergeBlocks clearedBlocks
    return $ ClearFunction (fCode ^. entryBlock) blocks
  where
    genClearBlocks :: M.Map Label ClearBlock -> (Label, Block) -> State Label (M.Map Label ClearBlock)
    genClearBlocks mp (label, block) = case block ^. escape of
        Goto nextLabel -> do
            newLabel <- freshLabel
            return
                $ M.insert label (ClearBlock (block ^. statements) (Goto newLabel))
                $ M.insert newLabel (ClearBlock (stmtsForPhi nextLabel label) (Goto nextLabel)) mp
        Branch labelTrue labelFalse val -> do
            newLabelTrue <- freshLabel
            newLabelFalse <- freshLabel
            return
                $ M.insert label (ClearBlock (block ^. statements) (Branch newLabelTrue newLabelFalse val))
                $ M.insert newLabelTrue (ClearBlock (stmtsForPhi labelTrue label) (Goto labelTrue))
                $ M.insert newLabelFalse (ClearBlock (stmtsForPhi labelFalse label) (Goto labelFalse)) mp
        Ret val -> return $ M.insert label (ClearBlock (block ^. statements) (Ret val)) mp
        VRet -> return $ M.insert label (ClearBlock (block ^. statements) VRet) mp

    stmtsForPhi :: Label -> Label -> [Stmt]
    stmtsForPhi phiBlock currentBlock = movs
      where
        phiToDo = M.toList $ M.mapMaybe (M.lookup currentBlock) (fCode ^. blocks . at phiBlock . _Just . phi)
        movs = map (\(dest, source) -> Mov dest (Location source)) phiToDo

    mergeableBlocks :: M.Map Label ClearBlock -> S.Set Label
    mergeableBlocks mp = S.fromList $ map fst $ filter (\(_, qty) -> qty == 1) $ M.toList $ foldr aux (M.fromList (map (\x -> (x, 0)) $ M.keys mp)) $ M.elems mp
      where
        aux (ClearBlock _ out) acc = case out of
            Ret _ -> acc
            VRet -> acc
            Goto label -> M.adjust (+1) label acc
            Branch label1 label2 _ -> M.adjust (+10) label1 $ M.adjust (+10) label2 acc -- links from Branch doesn't interest us, so we add 10 instead of 1

    mergeBlocks :: M.Map Label ClearBlock -> M.Map Label ClearBlock
    mergeBlocks mp = M.foldrWithKey aux M.empty mp
      where
        mergeable = mergeableBlocks mp
        aux label block acc = if label `S.member` mergeable
            then acc
            else M.insert label (merged label block) acc
        merged _ block@(ClearBlock stmts out) = case out of
            Goto nextLabel -> if nextLabel `S.member` mergeable
                then let ClearBlock mStmts mOut = merged nextLabel (mp M.! nextLabel) in ClearBlock (stmts ++ mStmts) mOut
                else block
            _ -> block

-- calculate IN alive sets

calculateInSets :: ClearFunction -> M.Map Label AliveSet
calculateInSets (ClearFunction _ blockMap) = findFixPoint
  where
    initial :: M.Map Label AliveSet
    initial = M.map (const S.empty) blockMap

    findFixPoint :: M.Map Label AliveSet
    findFixPoint = snd $ until (uncurry (==)) (\(_, current) -> (current, nextPoint current)) (initial, nextPoint initial)

    nextPoint :: M.Map Label AliveSet -> M.Map Label AliveSet
    nextPoint prevSolution = M.map (blockMod prevSolution) blockMap

stmtMod :: Stmt -> (AliveSet -> AliveSet)
stmtMod = \case
    Mov dest val                -> auxMov dest val
    FunArg dest _               -> S.delete dest
    BinStmt dest _ val1 val2    -> auxBinOp dest val1 val2
    CmpStmt dest _ val1 val2    -> auxBinOp dest val1 val2
    UniStmt dest _ val          -> auxMov dest val
    Call dest _ vs              -> auxValues vs . S.delete dest
    StringLit dest _            -> S.delete dest
    New dest _                  -> S.delete dest
    CallVirtual dest obj _ vs   -> auxValues vs . S.insert obj . S.delete dest
    SetAttr dest obj _ val      -> auxVal val . S.insert obj . S.delete dest
    GetAttr dest obj _          -> S.insert obj . S.delete dest
  where
    auxValues = foldr ((.) . auxVal) id
    auxMov dest val = auxVal val . S.delete dest
    auxBinOp dest val1 val2 = auxVal val1 . auxVal val2 . S.delete dest

blockMod :: M.Map Label AliveSet -> ClearBlock -> AliveSet
blockMod inSets (ClearBlock stmts out) = blockStmtsMod stmts $ setFromOut inSets out

blockStmtsMod :: [Stmt] -> (AliveSet -> AliveSet)
blockStmtsMod = foldr ((.) . stmtMod) id

setFromOut :: M.Map Label AliveSet -> OutStmt -> AliveSet
setFromOut mp = \case
    Goto nextBlock -> mp M.! nextBlock
    Branch trueBlock falseBlock val -> auxVal val $ (mp M.! trueBlock) `S.union` (mp M.! falseBlock)
    Ret val -> case val of
        Literal _ -> S.empty
        Location addr -> S.singleton addr
    VRet -> S.empty

auxVal :: Value -> (AliveSet -> AliveSet)
auxVal (Literal _) = id
auxVal (Location addr) = S.insert addr

lookupMax :: M.Map k a -> Maybe (k, a)
lookupMax mp
    | M.null mp = Nothing
    | otherwise = Just $ M.findMax mp

freshLabel :: State Label Label
freshLabel = modify (+1) >> get

