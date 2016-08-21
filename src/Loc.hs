module Loc where
import Types

toNorth (Loc x y) = Loc x (y-1)
toEast (Loc x y) = Loc (x+1) y
toWest (Loc x y) = Loc (x-1) y
toSouth (Loc x y) = Loc x (y+1)

toDir d loc =
  case d of
    N -> toNorth loc
    E -> toEast loc
    S -> toSouth loc
    W -> toWest loc
  
