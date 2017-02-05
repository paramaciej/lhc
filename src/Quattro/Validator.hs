{-# LANGUAGE LambdaCase #-}
module Quattro.Validator where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Quattro.Types
import Quattro.Generator
import qualified Utils.Abstract as A
import Utils.Show
import Utils.Verbose
import Utils.Types

type ValM = ExceptT String CompilerOptsM

generateValidatedQuattro :: A.Program -> ValM ProgramCode
generateValidatedQuattro program = do
    let initialState = QuattroSt M.empty Nothing 0 M.empty 1 1 funReturns classInfo
    finalState <- lift $ execStateT (genProgram program) initialState
    validateQuattro finalState
  where
    funReturns = M.fromList $ runtimes ++ map aux (program ^. A.aa . A.programFunctions ^.. traverse . A.aa)
    aux fnDef = (fnDef ^. A.fnDefIdent, fnDef ^. A.fnDefType)
    runtimes =
        [ runtimeFun "printInt"     A.Int
        , runtimeFun "printString"  A.Int
        , runtimeFun "error"        A.Int
        , runtimeFun "readInt"      A.Int
        , runtimeFun "readString"   A.Str]
    runtimeFun name retType = (A.makeAbs (A.Ident name), A.makeAbs retType)
    classInfo = M.fromList $ map xxx (program ^. A.aa . A.programClasses ^.. traverse . A.aa)
    xxx clsDef = ( clsDef ^. A.clsDefIdent
                 , ClsInfo
                    (createEnumeratedAttrMap   $ getClsAttrs   clsDef)
                    (createEnumeratedMethodMap $ getClsMethods clsDef)
                    (clsDef ^. A.clsDefExtend)
                 )
      where
        createEnumeratedAttrMap = M.fromList . map (\(nr, (i, t)) -> (i, (t, nr))) . zip [0..]
        createEnumeratedMethodMap = M.fromList . map (\(nr, (c, m, t)) -> (m, (t, nr, c))) . zip [0..]

        getClsAttrs :: A.AClsDef -> [(A.Ident, A.Type)]
        getClsAttrs clsDef = case clsDef ^. A.clsDefExtend of
            Just super -> superAttrs super ++ thisAttrs clsDef
            Nothing -> thisAttrs clsDef

        getClsMethods :: A.AClsDef ->  [(A.Ident, A.Ident, A.Type)]
        getClsMethods clsDef = case clsDef ^. A.clsDefExtend of
            Just super -> foldl aux (superMethods super) (thisMethods clsDef)
              where
                aux acc c@(_, cM, _) = case findIndex (\(_,i,_) -> i == cM) acc of
                    Just idx -> acc & element idx .~ c
                    Nothing -> acc ++ [c]
            Nothing -> thisMethods clsDef

        thisAttrs clsDef = foldr attrAux [] (clsDef ^. A.clsDefBody . A.aa . A.classBodyStmts ^.. traverse . A.aa)
        attrAux stmt acc = case stmt of
            A.Method{} -> acc
            A.Attr t items -> acc ++ map (\(A.AttrItem i) -> (i, t)) (items ^.. traverse . A.aa)
        thisMethods clsDef = foldl (methodAux clsDef) [] (clsDef ^. A.clsDefBody . A.aa . A.classBodyStmts ^.. traverse . A.aa)
        methodAux clsDef acc = \case
            A.Attr{} -> acc
            A.Method t ident args _ -> acc ++ [(clsDef ^. A.clsDefIdent, ident, makeFunType t args)]
        superAttrs = getClsAttrs . getClsDef
        superMethods = getClsMethods . getClsDef
        superMethodsSet = S.fromList . map (\(x, _, _) -> x) . superMethods
        getClsDef ident = fromJust $ find (\c -> c ^. A.clsDefIdent == ident) (program ^. A.aa . A.programClasses ^.. traverse . A.aa)



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
qBlockToBlock (QBlock _ _ _ Nothing) = error "impossible happened"


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