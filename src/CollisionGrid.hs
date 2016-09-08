{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module CollisionGrid ( new
                     , doesCollide
                     , clearLifeCells
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

new size@(Size w h) =
  let cgrid = DM.fromList [(Loc x y, False) | x <- [0 .. w], y <- [0 .. h]]
  in CollisionGrid size cgrid

clearCell :: CollisionGrid -> Loc -> HCell CollisionGrid
clearCell cg@CollisionGrid{..} loc@(Loc x y) = do
  --checkOutOfRange cg loc
  return $ cg{cGridMap = DM.insert loc False cGridMap}

fillCell :: CollisionGrid -> Loc -> HCell CollisionGrid
fillCell cg@CollisionGrid{..} loc@(Loc x y) = do
  --checkOutOfRange cg loc
  return $ cg{cGridMap = DM.insert loc True cGridMap}

insertLifeForm :: CollisionGrid -> LifeForm -> HCell CollisionGrid
insertLifeForm cgrid lifeForm =
  let relativeLocs = LifeForm.getNonEmptyCellLocs (cGridSize cgrid) lifeForm
  in foldM fillCell cgrid relativeLocs

clearLifeCells :: CollisionGrid -> LifeForm -> HCell CollisionGrid
clearLifeCells cgrid lifeForm =
  -- clear a lifeforms grid a collision grid
  let relativeLocs = LifeForm.getNonEmptyCellLocs (cGridSize cgrid) lifeForm
  in foldM clearCell cgrid relativeLocs

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

doesCollide cgrid lifeForm = do
  let locs = LifeForm.getNonEmptyCellLocs (cGridSize cgrid) lifeForm
  -- check the cgrid to make sure all the locs are False
  liftM (not . and) $ mapM (cellEmpty cgrid) locs











