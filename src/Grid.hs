module Grid where

import qualified Data.Map as DM
import qualified Data.Set as DS
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
addLifeForm grid@(Grid cells size) (Simple _ offset code gridLf) = 
  let (Loc dx dy) = offset
      (Grid cellsLf _) = offsetGrid gridLf size dx dy
      augmentedGrid = Grid (DM.union cellsLf cells) size
  in augmentedGrid

addLifeForms :: Foldable t => Grid -> t LifeForm -> Grid
addLifeForms grid = foldl addLifeForm grid

numCells (Grid _ (Size w h)) = w * h
