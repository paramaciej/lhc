module Quattro.Validator where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.List
import qualified Data.Map as M

import Quattro.Types
import Quattro.Generator
import qualified Utils.Abstract as A
import Utils.Show
import Utils.Verbose

type ValM = ExceptT String CompilerOptsM

generateValidatedQuattro :: A.Program -> ValM ProgramCode
generateValidatedQuattro program = do
    let initialState = QuattroSt M.empty Nothing 0 M.empty 1 1
    finalState <- lift $ execStateT (genProgram program) initialState
    validateQuattro finalState


validateQuattro :: QuattroSt -> ValM ProgramCode
validateQuattro state = do
    forFun <- mapM genFun $ M.toList (state ^. funCode)
    return $ ProgramCode $ M.fromList forFun
  where
    genFun (ident, qCode) = do
        let cleanedCode = cutUnreachableBlocks qCode
        validateEscapes cleanedCode
        validatePhis cleanedCode

        return
            ( ident ^. A.aa . A.identString
            , FunctionCode (cleanedCode ^. entryCodeBlock) (M.map qBlockToBlock (cleanedCode ^. codeBlocks))
            )

cutUnreachableBlocks :: QCode -> QCode
cutUnreachableBlocks qCode = if null unreachable
    then modifiedCode
    else cutUnreachableBlocks modifiedCode
  where
    partitionFunction key block = key /= (qCode ^. entryCodeBlock) && null (block ^. entry)
    (unreachableMap, rest) = M.partitionWithKey partitionFunction (qCode ^. codeBlocks)
    unreachable = M.keys unreachableMap
    modifiedCode = qCode & codeBlocks .~ M.map (entry %~ (\\ unreachable)) rest

qBlockToBlock :: QBlock -> Block
qBlockToBlock (QBlock _ phis stmts (Just out)) = Block (M.map snd phis) (reverse stmts) out


validateEscapes :: QCode -> ValM ()
validateEscapes qCode = forM_ (qCode ^. codeBlocks . to M.toList) $ \(label, qBlock) -> case qBlock ^. out of
    Nothing -> throwValErr "block hasn't got a escape statement!"
    Just out -> case out of
        Goto goto -> checkGoto goto
        Branch br1 br2 _ -> checkGoto br1 >> checkGoto br2 >> return ()
        _ -> return ()
      where
        checkGoto :: Label -> ValM ()
        checkGoto goto = unless (elem label $ qCode ^. codeBlocks . at goto . _Just . entry) $ throwValErr "incomplete entry list"

validatePhis :: QCode -> ValM ()
validatePhis qCode = forM_ (qCode ^. codeBlocks) $ \qBlock ->
        unless (all (== qBlock ^. entry . to sort) ((M.keys . snd) <$> M.elems (qBlock ^. phiQ))) $ throwValErr "phi inconsistency"

throwValErr :: String -> ValM ()
throwValErr = throwError . (red "QUATTRO GENERATION ERROR: " ++)