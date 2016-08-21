{-# LANGUAGE FlexibleInstances #-}
module Lib where

{-
import qualified Data.Map as DM
import qualified Data.Set as DS

class Mass a where
  mass :: a -> Integer

data Loc = Loc { locX :: Integer
               , locY :: Integer
               } deriving (Show, Eq, Ord)
           
data Dir = E | N | W | S
         deriving (Show, Eq)

data CellType = JointCell Joint
              | LifeCell
              | EmptyCell
              | Transporter
                deriving (Show, Eq)

instance Mass CellType where
  mass (JointCell _) = 1
  mass LifeCell = 1
  mass EmptyCell = 0
  mass Transporter = 0

instance Draw CellType where
  draw (JointCell _) = putStr "j "
  draw LifeCell = putStr "* "
  draw EmptyCell = putStr ". "
  draw Transporter = putStr "T "

data SimpleCode = Move Dir
                | MoveRandom
                | NOP
                deriving (Show, Eq)
                         
data ComplexCode = Flip
                 | CCode SimpleCode
                 deriving (Show, Eq)


data Size = Size { sizeH :: Integer
                 , sizeW :: Integer
                 } deriving (Show, Eq)

data Grid = Grid { gridCells :: DM.Map Loc CellType
                 , gridSize :: Size
                 }

instance Draw [(Loc, Maybe CellType)] where
  draw [] = putStrLn ""
  draw ((_, Just cell):xs) = do draw cell
                                draw xs

instance Draw Grid where
  draw (Grid cells size) = do
    let Size h w = size
    putStrLn $ concat $ take (fromIntegral w) (repeat "--")
    mapM_ draw [[(Loc x y, DM.lookup (Loc x y) cells) | x <- [0 .. w]] | y <- [0 .. h]]
    putStrLn $ concat $ take (fromIntegral w) (repeat "--")
            
newGrid size =
  let (Size h w) = size
      cells = DM.fromList [(Loc x y, EmptyCell) | x <- [0 .. w], y <- [0 .. h]]
  in Grid cells size      

-- transporters are one way paths.
-- send grids can't overlap recv grids.
data Transporter = Send Grid
                 | Recv Grid

data Universe = Universe { uGrid :: Grid
                         , uSize :: Size
                         , uLifeForms :: DS.Set LifeForm
                         } 

newUniverse size = Universe (newGrid size) size (DS.empty)

instance Draw Universe where
  draw (Universe g s ls) = do
    putStrLn $ "A Universe of size: " ++ (show s)
    draw g

instance Mass Universe where
  mass (Universe _ _ lifeForms) = sum $ DS.map mass lifeForms

data Orientation = TopBottom { topOffset :: Integer
                             , bottomOffset :: Integer
                             } 
                 | LeftRight { leftOffset :: Integer
                             , rightOffset :: Integer
                             } deriving (Show, Eq)

data Joint = NonRotating Orientation
           | Rotatating Orientation
           deriving (Show, Eq)

instance Mass Joint where
  mass _ = 1

data LifeForm = Complex [ComplexCode] Joint LifeForm LifeForm
              | Simple [SimpleCode] Grid

instance Mass LifeForm where
  mass (Complex _ j lf1 lf2) = mass j + mass lf1 + mass lf2

class Draw a where
  draw :: a -> IO ()


-}
