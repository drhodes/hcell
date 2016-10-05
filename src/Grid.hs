{-# LANGUAGE RecordWildCards #-}

module Grid where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DVi
import qualified Loc
import Types

new size =
  let (Size w h) = size
      cells = DM.fromList [(Loc x y, EmptyCell) | x <- [0 .. w-1], y <- [0 .. h-1]]
  in Grid cells size      

contains :: Grid -> Loc -> Bool
contains (Grid _ (Size w h)) (Loc x y) = x < w && y < h

getNonEmptyCellLocs Grid{..} =
  [loc | (loc, cellType) <- DM.toList gridCells, cellType /= EmptyCell]

putCell :: Grid -> Loc -> CellType -> Grid
putCell g@(Grid cells (Size w h)) (Loc x y) cell =
  let loc' = Loc (x `mod` w) (y `mod` h)
  in g{gridCells = (DM.insert loc' cell cells)}


offsetGrid (Grid cells _) size@(Size w h) dx dy =
  let offsetCells = [(Loc (x+dx `mod` w) (y+dy `mod` h), ct)
                    | (Loc x y, ct) <- DM.toList cells]
  in Grid (DM.fromList offsetCells) size


testg = Grid cells size
  where cells = DM.fromList $ zip locs cts
        locs = [Loc 0 0, Loc 1 0, Loc 0 1, Loc 1 1]
        cts = [LifeCell, EmptyCell, Transporter, LifeCell]
        size = Size 3 3

transpose (Grid cells (Size w h)) = Grid diagFlip (Size h w)
  where xs = DM.toList cells
        diagFlip = DM.fromList [(Loc y x, ct) | (Loc x y, ct) <- xs]

flipV (Grid cells size@(Size w h)) = Grid flipV size
  where xs = DM.toList cells
        f y = let y' = fromIntegral y
                  ys = reverse [0 .. (h-1)]
              in if y' > length ys - 1
                 then error $ show (y', ys, xs, size)
                 else ys !! (fromIntegral y)
        flipV = DM.fromList [(Loc x (f y), ct) | (Loc x y, ct) <- xs]

rotate CCW = flipV . transpose
-- rotate CW = rotate CCW . rotate CCW . rotate CCW


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






