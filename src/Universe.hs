{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Universe where

import qualified Data.Map as DM
import qualified Grid
import qualified LifeForm
import           Types
import qualified Data.Set.Monad as DSM
import Control.Monad
import qualified SpaceHash

new size = Universe (Grid.new size) (SpaceHash.new 10) size 0 (DSM.empty)

addLifeForm uv@(Universe _ sh _ _ lfs) lf =
  uv { uLifeForms = DSM.insert lf lfs
     , uSpaceHash = let w = LifeForm.width lf
                        h = LifeForm.height lf
                        n = simpleId lf
                    in SpaceHash.add sh (simpleLoc lf) n w h }



step :: Universe -> HCell Universe
step (Universe g sh s t lfs) = do
  let xs = DSM.toList lfs
  -- this conversion to from set to list back to set is sad
  -- reason why sets can't find Ord instance for (Monad Lifeform)
  -- hrm. maybe a better way to do it.
  lfs' <- mapM LifeForm.step xs 
  return $ Universe g sh s (t+1) (DSM.fromList lfs')

stepN n u = do
  if n <= 0
    then return u
    else do u' <- step u
            stepN (n-1) u'
