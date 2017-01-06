module Utils.Types where

import Utils.Abstract
import qualified Data.Map as M

topTypes ::Program -> M.Map String (TopDef, AType)
topTypes (AbsPos _ _ (Program tds)) = M.fromList $ map aux tds
  where
    aux td@(AbsPos _ _ (FnDef t i args _)) = (identString (aa i), (td, Fun (forgetPos t) (map (forgetPos . argType . aa) args)))