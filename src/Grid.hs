{-# LANGUAGE RecordWildCards #-}

module Grid where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DVi
import qualified Loc
import Types

new size =
  let (Size h w) = size
      cells = DM.fromList [(Loc x y, EmptyCell) | x <- [0 .. w], y <- [0 .. h]]
  in Grid cells size      

contains :: Grid -> Loc -> Bool
contains (Grid _ (Size w h)) (Loc x y) = x < w && y < h


putCell :: Grid -> Loc -> CellType -> Grid
putCell g@(Grid cells (Size w h)) (Loc x y) cell =
  let loc' = Loc (x `mod` w) (y `mod` h)
  in g{gridCells = (DM.insert loc' cell cells)}


offsetGrid (Grid cells _) size@(Size w h) dx dy =
  let offsetCells = [(Loc (x+dx `mod` w) (y+dy `mod` h), ct)
                    | (Loc x y, ct) <- DM.toList cells]
  in Grid (DM.fromList offsetCells) size


-- will need to do collision detection here.
addLifeForm grid@(Grid cells size) Simple{..} = --offset code gridLf) = 
  let (Loc dx dy) = simpleLoc
      (Grid cellsLf _) = offsetGrid simpleGrid size dx dy
      augmentedGrid = Grid (DM.union cellsLf cells) size
  in augmentedGrid


addLifeForms :: Foldable t => Grid -> t LifeForm -> Grid
addLifeForms grid = foldl addLifeForm grid


numCells (Grid _ (Size w h)) = w * h
getCellAt (Grid cells _) loc = DM.lookup loc cells


hasNeighbor grid loc =
  case getCellAt grid loc of
    Nothing -> False
    Just EmptyCell -> False
    _ -> True

-- 1 2 3
-- 4 5 6
-- 7 8 9


toDisplayBlock :: Grid -> Loc -> DisplayBlock
toDisplayBlock grid loc =
  let nbrs = [ Loc.toNorth $ Loc.toWest loc
             , Loc.toNorth loc
             , Loc.toNorth $ Loc.toEast loc
             , Loc.toWest loc
             , Loc.toEast loc
             , Loc.toSouth $ Loc.toWest loc
             , Loc.toSouth loc
             , Loc.toSouth $ Loc.toEast loc
             ]
      gotNbrs = map (hasNeighbor grid) nbrs
      shards = [D1, D2, D3, D4, D6, D7, D8, D9]      
  in let block = case getCellAt grid loc of
           Nothing -> EmptyDblock
           Just EmptyCell -> EmptyDblock
           Just LifeCell -> Dblock LifeCell (D5:[shard | (b, shard) <- zip gotNbrs shards, b])
     in block


toDisplayGrid :: Grid -> DisplayGrid
toDisplayGrid grid@(Grid cells size) =
  let keys = DM.keys cells
      cellBlocks = map (toDisplayBlock grid) keys
  in DisplayGrid (DM.fromList $ zip keys cellBlocks) size





