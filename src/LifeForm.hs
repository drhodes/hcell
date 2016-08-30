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

new :: Loc -> Program -> [String] -> HCell LifeForm
new loc@(Loc x y) code pattern = do
  lid <- Util.nextNonce
  let g = buildLifeFormGrid pattern
      x' = fromIntegral y
      y' = fromIntegral y
  return $ Simple lid loc code g 0

addLifeForm uv@(Universe _ _ _ lfs) lf = uv { uLifeForms = DSM.insert lf lfs }

buildCell x y char =
  let loc = Loc x y
      cell = case char of
        '*' -> LifeCell
        '.' -> EmptyCell
        't' -> Transporter
        x -> error $ "LifeForm.buildCell need to implement " ++ (show x)
  in (loc, cell)


rowCells :: (Integer, String) -> [(Loc, CellType)]
rowCells (y, row) = [buildCell x y char | (char, x) <- zip row [0..]]

buildLifeFormGrid rows =
  let enumRows = zip [0..] rows
      w = fromIntegral $ length $ head enumRows
      h = fromIntegral $ length rows
      g = DM.fromList (concat $ map rowCells enumRows)
  in Grid g (Size w h) 

moveRandom s@Simple{..} = do
  d <- Loc.randomDir
  return $ s{simpleLoc = Loc.toDir d simpleLoc}

step :: LifeForm -> HCell LifeForm
step lf@(Simple lid loc code grid age) = do
  let inst = Program.curInstruction code
      code' = Program.rotate code
      age' = age - 1
  case inst of
    Move dir -> return $ Simple lid (Loc.toDir dir loc) code' grid age'
    NOP -> return $ Simple lid loc code' grid age'
    MoveRandom -> do lf' <- moveRandom lf
                     return $ lf'{simpleProg = code'}{simpleAge = age'}
    _ -> return $ lf{simpleProg = code'}{simpleAge = age'}


