module Program where

import qualified Data.Vector as DV
import Types



new xs = Program 0 (DV.fromList xs)

rotate (Program idx xs) =
  let idx' = (idx + 1) `mod` (fromIntegral $ DV.length xs)
  in Program idx' xs

curInstruction (Program idx xs) = xs DV.! (fromIntegral idx)

length (Program _ xs) = DV.length xs
