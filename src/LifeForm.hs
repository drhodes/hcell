{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module LifeForm where

import           Control.Monad
import qualified Data.Map as DM
import qualified Data.Set.Monad as DSM
import qualified Grid
import qualified Loc
import qualified Program
import qualified System.Random as Random
import           Types
import qualified Data.Hashable as Hash
import qualified Util

import qualified DisplayGrid.Api as DG
import qualified DisplayGrid.Types as DT

new :: Loc -> Program -> [String] -> HCell LifeForm
new loc@(Loc x y) code pattern = do
  lid <- liftM LifeId Util.nextNonce
  c <- Util.randomColor
  let g = buildLifeFormGrid pattern
      x' = fromIntegral y
      y' = fromIntegral y
  return $ Simple lid loc code g 0

width = sizeW . gridSize . simpleGrid 
height = sizeH . gridSize . simpleGrid 

buildCell x y char =
  let loc = Loc x y
      cell = case char of
        '+' -> WallCell PosWC
        '-' -> WallCell NegWC
        '.' -> EmptyCell
        't' -> Transporter
        x -> error $ "LifeForm.buildCell need to implement " ++ show x
  in (loc, cell)

rowCells :: (Integer, String) -> [(Loc, CellType)]
rowCells (y, row) = [buildCell x y char | (char, x) <- zip row [0..]]

buildLifeFormGrid rows =
  let enumRows = zip [0..] rows
      w = fromIntegral $ length $ head rows
      h = fromIntegral $ length rows
      g = DM.fromList (concatMap rowCells enumRows)
  in Grid g (Size w h) 

moveRandom s@Simple{..} size = do
  d <- Loc.randomDir
  let nextLoc = Loc.wrap size (Loc.toDir d simpleLoc)
  return $ s{simpleLoc = nextLoc}

step :: LifeForm -> Size -> HCell LifeForm
step lf@(Simple lid loc code grid age) size = do
  let inst = Program.curInstruction code
      code' = Program.rotate code
      age' = age + 1
  
  case inst of
    Move dir -> do let nextLoc = Loc.wrap size (Loc.toDir dir loc)
                   return $ Simple lid nextLoc code' grid age'
                   
    NOP -> return $ Simple lid loc code' grid age' 

    Rotate rot -> return $ Simple lid loc code' (Grid.rotate rot grid) age' 
    FlipV -> return $ Simple lid loc code' (Grid.flipV grid) age' 
    Transpose -> return $ Simple lid loc code' (Grid.transpose grid) age' 
    
    MoveRandom -> do lf' <- moveRandom lf size
                     return $ lf'{ simpleProg = code'
                                 , simpleAge = age'}
                       
    _ -> return $ lf{ simpleProg = code'
                    , simpleAge = age'}

spin lf = lf{simpleProg = Program.rotate (simpleProg lf)}

getNonEmptyCellLocs uSize Simple{..} =
  map (Loc.wrap uSize . Loc.add simpleLoc) (Grid.getNonEmptyCellLocs simpleGrid)

-- getCellType Simple{..} loc =
--   let (Grid cells _) = simpleGrid
--   in (loc, DM.lookup cells loc)



displayOne :: LifeForm -> DT.GridT ()
displayOne s@Simple{..} = do
  w <- DG.getWindowWidth
  h <- DG.getWindowHeight
  let cellLocs = Grid.getNonEmptyCellLocs simpleGrid
  --let colorCells =  [(loc, getCellType s loc) |
  let (Grid cells _) = simpleGrid
  let (Loc x' y') = simpleLoc

  
  let f (Loc x y) = do
        let color = case DM.lookup (Loc x y) cells  of
              Just (JointCell _) -> DG.colorFromHex 0x222222FF
              Just (WallCell PosWC) -> DG.colorFromHex 0xFF0000FF
              Just (WallCell NegWC) -> DG.colorFromHex 0x0000FFFF
              Just EmptyCell -> DG.colorFromHex 0x000000FF
              Just Transporter -> DG.colorFromHex 0x0000FFFF
              Nothing -> error "displayOne explodes because impossible happened"
        DG.setCellColor color (DT.CellLoc (x + x') (y + y'))
        
  mapM_ f cellLocs

displayAll :: DM.Map LifeId LifeForm -> DT.GridT ()
displayAll xs = mapM_ displayOne (DM.elems xs)
  
