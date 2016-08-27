module LifeForm where

import           Control.Monad
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Grid
import qualified Loc
import qualified Program
import qualified System.Random as Random
import           Types
import qualified Data.Hashable as Hash

new :: Integer -> Loc -> Program -> [String] -> LifeForm
new lid loc@(Loc x y) code pattern =
  let g = buildLifeFormGrid pattern
      x' = fromIntegral y
      y' = fromIntegral y
  in Simple lid loc code g $ Random.mkStdGen (fromIntegral lid)

addLifeForm uv@(Universe _ _ _ lfs) lf = uv { uLifeForms = DS.insert lf lfs }

temp = [ "..."
       , "..."
       , "..."
       ] 

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

moveRandom (Simple lid loc prog grid gen) =
  let (x, gen') = Random.next gen        
      dir = toEnum $ x `mod` 4
  in Simple lid (Loc.toDir dir loc) prog grid gen'


step lf@(Simple lid loc code grid seed) =
  let inst = Program.curInstruction code
      code' = Program.rotate code
  in case inst of
    Move dir -> Simple lid (Loc.toDir dir loc) code' grid seed
    NOP -> Simple lid loc code' grid seed
    MoveRandom -> (moveRandom lf) {simpleProg = code'}
    _ -> lf

