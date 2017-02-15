{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Types where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified System.Random as Random
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Functor.Identity
import Control.Monad.Except
import qualified SDL
import qualified DisplayGrid.Types as DT
import Linear

type HCell b = forall m. ( MonadState CellState m,
                           MonadError String m ) => m b

runExceptStateT :: s -> StateT s (ExceptT String m) a -> m (Either String (a, s))
runExceptStateT s = runExceptT . flip runStateT s

data CellState = CellState { csRand :: Random.StdGen
                           , csNonce :: Integer
                           } deriving (Show)

runHCell :: s -> StateT s (ExceptT String m) a -> m (Either String (a, s))
runHCell = runExceptStateT 

newCS :: CellState
newCS = CellState (Random.mkStdGen 0) 0

runCellState :: Monad m => HCell a -> m (Either String (a, CellState))
runCellState = runHCell newCS

data LifeId = LifeId Integer
            deriving (Show, Eq, Ord)

class Mass a where
  mass :: a -> Integer

data Loc = Loc { locX :: Integer
               , locY :: Integer
               } deriving (Show, Eq, Ord)
           
data Dir = E | N | W | S
         deriving (Show, Eq, Ord, Enum)

data CellType = JointCell Joint
              | WallCell 
              | EmptyCell
              | Transporter
                deriving (Show, Eq, Ord)

instance Mass CellType where
  mass (JointCell _) = 1
  mass WallCell = 1
  mass EmptyCell = 0
  mass Transporter = 0

data Program = Program Integer (DV.Vector Code)
               deriving (Show, Eq, Ord)

data Rot = CCW | CW deriving (Show, Eq, Ord)
                        
data Code = Move Dir
          | MoveRandom
          | Rotate Rot
          | NOP
          | PassLeft
          | PassRight
          | PassBoth
          | FlipV
          | Transpose
            deriving (Show, Eq, Ord)

data Size = Size { sizeW :: !Integer
                 , sizeH :: !Integer
                 } deriving (Show, Eq, Ord)

data Grid = Grid { gridCells :: !(DM.Map Loc CellType)
                 , gridSize :: !Size
                 } deriving (Show, Eq, Ord)

data BoundingBox = BoundingBox Loc Loc


instance Mass Grid where
  mass (Grid g _) = sum $ map mass (DM.elems g)

-- transporters are one way paths.
-- send grids can't overlap recv grids in space.
data Transporter = Send Grid
                 | Recv Grid

data Universe = Universe { uGrid :: !Grid
                         , uCollisionGrid :: !CollisionGrid
                         , uSize :: !Size
                         , uTime :: !Integer
                         , uLifeForms :: !(DM.Map LifeId LifeForm)
                         } deriving (Show)

instance Mass Universe where
  mass (Universe _ _ _ _ lifeForms) =
    sum $ map mass (DM.elems lifeForms)

data Orientation = TopBottom { topOffset :: Integer
                             , bottomOffset :: Integer
                             } 
                 | LeftRight { leftOffset :: Integer
                             , rightOffset :: Integer
                             } 
                 | OrientationUndetermined
                 deriving (Show, Eq, Ord)

data Joint = NonRotating Orientation
           | Rotating Orientation
           deriving (Show, Eq, Ord)

instance Mass Joint where
  mass _ = 1

data LifeForm = Complex Loc Program Joint LifeForm LifeForm
              | Simple { simpleId :: LifeId 
                       , simpleLoc :: Loc
                       , simpleProg :: Program
                       , simpleGrid :: Grid
                       , simpleAge :: Integer                       
                       } deriving (Show)

instance Ord LifeForm where
  (<=) x _ = True

instance Eq LifeForm where
  (==) l1 l2 = simpleId l1 == simpleId l2

lifeFormGrid (Complex _ _ _ lf1 lf2) = lifeFormGrid lf1 ++ lifeFormGrid lf2
lifeFormGrid Simple{..} = [simpleGrid]
                         
instance Mass LifeForm where
  mass (Complex _ _ j lf1 lf2) = mass j + mass lf1 + mass lf2
  mass Simple{..} = mass simpleGrid

class Draw a where
  draw :: a -> IO ()

data CollisionGrid = CollisionGrid { cGridSize :: Size
                                   , cGridMap :: DM.Map Loc Bool 
                                   } deriving (Show)
                                              
                                              

-- 1 2 3
-- 4 5 6
-- 7 8 9





{- extremely fast 2d bit array

bits of a 64-bit integer.

1 0 0 1 0 1 1 0
1 0 0 1 0 0 1 0
1 1 0 1 0 0 1 0
1 0 0 1 0 0 1 0
1 1 0 1 0 0 1 0
1 1 0 1 0 0 1 0
1 1 1 0 1 1 0 1
1 0 1 0 1 1 1 1


zero out the Z positions?

1 0 0 1 0 1 1 0
1 0 0 1 0 0 1 0
1 1 0 1 0 0 1 0
1 0 0 1 Z Z Z 0
1 1 0 1 Z 0 Z 0
1 1 0 1 Z Z Z 0
1 1 1 0 1 1 0 1
1 0 1 0 1 1 1 1

first coord (4,3)

00                70                E0
+-----------------+-----------------+-----------------+-----------------+
| 1 0 0 1 0 1 1 0 | 1 0 0 1 0 1 1 0 | 1 0 0 1 0 1 1 0 | 1 0 0 1 0 1 1 0 |
| 1 0 0 1 0 0 1 0 | 1 0 0 1 0 0 1 0 | 1 0 0 1 0 0 1 0 | 1 0 0 1 0 0 1 0 |
| 1 1 0 1 0 0 1 0 | 1 1 0 1 0 0 1 0 | 1 1 0 1 0 0 1 0 | 1 1 0 1 0 0 1 0 |
| 1 0 0 1 * B C 0 | 1 0 0 1 A B C 0 | 1 0 0 1 A B C 0 | 1 0 0 1 A B C 0 |
| 1 1 0 1 D 0 E 0 | 1 1 0 1 D 0 E 0 | 1 1 0 1 D 0 E 0 | 1 1 0 1 D 0 E 0 |
| 1 1 0 1 F G H 0 | 1 1 0 1 F G H 0 | 1 1 0 1 F G H 0 | 1 1 0 1 F G H 0 |
| 1 1 1 0 1 1 0 1 | 1 1 1 0 1 1 0 1 | 1 1 1 0 1 1 0 1 | 1 1 1 0 1 1 0 1 |
| 1 0 1 0 1 1 1 1 | 1 0 1 0 1 1 1 1 | 1 0 1 0 1 1 1 1 | 1 0 1 0 1 1 1 1 |
+-----------------+-----------------+-----------------+-----------------+
| 1 0 0 1 0 1 1 0 | 1 0 0 1 0 1 1 0 | 1 0 0 1 0 1 1 0 | 1 0 0 1 0 1 1 0 |
| 1 0 0 1 0 0 1 0 | 1 0 0 1 0 0 1 0 | 1 0 0 1 0 0 1 0 | 1 0 0 1 0 0 1 0 |
| 1 1 0 1 0 0 1 0 | 1 1 0 1 0 0 1 0 | 1 1 0 1 0 0 1 0 | 1 1 0 1 0 0 1 0 |
| 1 0 0 1 A B C 0 | 1 0 0 1 A B C 0 | 1 0 0 1 A B C 0 | 1 0 0 1 A B C 0 |
| 1 1 0 1 D 0 E 0 | 1 1 0 1 D 0 E 0 | 1 1 0 1 D 0 E 0 | 1 1 0 1 D 0 E 0 |
| 1 1 0 1 F G H 0 | 1 1 0 1 F G H 0 | 1 1 0 1 F G H 0 | 1 1 0 1 F G H 0 |
| 1 1 1 0 1 1 0 1 | 1 1 1 0 1 1 0 1 | 1 1 1 0 1 1 0 1 | 1 1 1 0 1 1 0 1 |
| 1 0 1 0 1 1 1 1 | 1 0 1 0 1 1 1 1 | 1 0 1 0 1 1 1 1 | 1 0 1 0 1 1 1 1 |
+-----------------+-----------------+-----------------+-----------------+
| 1 0 0 1 0 1 1 0 | 1 0 0 1 0 1 1 0 | 1 0 0 1 0 1 1 0 | 1 0 0 1 0 1 1 0 |
| 1 0 0 1 0 0 1 0 | 1 0 0 1 0 0 1 0 | 1 0 0 1 0 0 1 0 | 1 0 0 1 0 0 1 0 |
| 1 1 0 1 0 0 1 0 | 1 1 0 1 0 0 1 0 | 1 1 0 1 0 0 1 0 | 1 1 0 1 0 0 1 0 |
| 1 0 0 1 A B C 0 | 1 0 0 1 A B C 0 | 1 0 0 1 A B C 0 | 1 0 0 1 A B C 0 |
| 1 1 0 1 D 0 E 0 | 1 1 0 1 D 0 E 0 | 1 1 0 1 D 0 E 0 | 1 1 0 1 D 0 E 0 |
| 1 1 0 1 F G H 0 | 1 1 0 1 F G H 0 | 1 1 0 1 F G H 0 | 1 1 0 1 F G H 0 |
| 1 1 1 0 1 1 0 1 | 1 1 1 0 1 1 0 1 | 1 1 1 0 1 1 0 1 | 1 1 1 0 1 1 0 1 |
| 1 0 1 0 1 1 1 1 | 1 0 1 0 1 1 1 1 | 1 0 1 0 1 1 1 1 | 1 0 1 0 1 1 1 1 |
+-----------------+-----------------+-----------------+-----------------+

-}
