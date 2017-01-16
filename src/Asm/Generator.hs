module Asm.Generator where

import qualified Data.Map as M
import Control.Lens

import Quattro.Types
import Utils.Verbose

programAsm :: ProgramCode -> String
programAsm program = concatMap yyy $ M.toList (program ^. functions)

yyy :: (String, FunctionCode) -> String
yyy (name, code) = ".globl " ++ name ++ "\n"
    ++ zzz initialBlock ++ concatMap zzz restBlocks ++ "\n"
  where
    (initial, rest) = M.partitionWithKey (\k _ -> k == code ^. entryBlock) (code ^. blocks)
    initialBlock = (name, head $ M.elems initial)
    restBlocks = M.toList $ M.mapKeys show rest

zzz :: (String, Block) -> String
zzz (label, block) = label ++ ":\n" ++ indentStr "no witam\ndrugalinia"