{-# LANGUAGE FlexibleContexts #-}
module Loc where
import Types
import qualified Util
import Control.Monad

toNorth (Loc x y) = Loc x (y-1)
toEast (Loc x y) = Loc (x+1) y
toWest (Loc x y) = Loc (x-1) y
toSouth (Loc x y) = Loc x (y+1)

add (Loc x y) (Loc x' y') = Loc (x + x') (y + y')

toDir d loc =
  case d of
    N -> toNorth loc
    E -> toEast loc
    S -> toSouth loc
    W -> toWest loc

randomDir :: HCell Dir
randomDir = do
  n <- Util.randomInt
  return $ toEnum (n `mod` 4)

wrap (Size w h) (Loc x y) = Loc (x `mod` w) (y `mod` h)
