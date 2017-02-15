{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module CollisionGrid ( new
                     , doesCollide
                     , rmLifeForm
                     , insertLifeForm
                     ) where

import Types
import qualified Data.Map as DM
import qualified Data.Set as DS
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Except
import qualified Grid
import qualified LifeForm
import qualified Loc
import qualified Debug.Trace as Debug

new size@(Size w h) =
  let cgrid = DM.fromList []
  in CollisionGrid size cgrid

clearCell :: CollisionGrid -> Loc -> HCell CollisionGrid
clearCell cg@CollisionGrid{..} loc@(Loc x y) = do
  --checkOutOfRange cg loc
  return $ cg{cGridMap = DM.delete loc cGridMap}

fillCell :: CollisionGrid -> Loc -> HCell CollisionGrid
fillCell cg@CollisionGrid{..} loc@(Loc x y) = do
  --checkOutOfRange cg loc
  return $ cg{cGridMap = DM.insert loc True cGridMap}

insertLifeForm :: CollisionGrid -> LifeForm -> HCell CollisionGrid
insertLifeForm cgrid lifeForm =
  let relativeLocs = LifeForm.getNonEmptyCellLocs (cGridSize cgrid) lifeForm
  in foldM fillCell cgrid relativeLocs

rmLifeForm :: CollisionGrid -> LifeForm -> HCell CollisionGrid
rmLifeForm cgrid lifeForm =
  let locs = LifeForm.getNonEmptyCellLocs (cGridSize cgrid) lifeForm
  in foldM clearCell cgrid locs

checkOutOfRange :: CollisionGrid -> Loc -> HCell ()
checkOutOfRange CollisionGrid{..} (Loc x y) = do
  when (x > (sizeW cGridSize))
    (throwError "CollisionGrid.clearCell encounters Loc with x value larger than grid width")
  when (y > (sizeH cGridSize))
    (throwError "CollisionGrid.clearCell encounters Loc with y value larger than grid height")

cellEmpty cgrid@CollisionGrid{..} loc = do
  checkOutOfRange cgrid loc
  case DM.lookup loc cGridMap of
    Nothing -> return True
    Just False -> return True
    Just True -> return False


doesCollide :: CollisionGrid -> LifeForm -> HCell Bool
doesCollide cgrid lifeForm = do
  let locs = LifeForm.getNonEmptyCellLocs (cGridSize cgrid) lifeForm
  -- check the cgrid to make sure all the locs are False
  checkedCells <- mapM (cellEmpty cgrid) locs
  return $ not $ and checkedCells











