{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import qualified Data.Map as DM
import qualified Data.Set.Monad as DSM
import qualified Data.Vector as DV
import qualified System.Random as Random
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Functor.Identity
import Control.Monad.Except

type HCell b = forall m. ( MonadState CellState m,
                           MonadError String m ) => m b

-- runStateExceptT :: s -> ExceptT e (StateT s m) a -> m (Either e a, s)
-- runStateExceptT s = flip runStateT s . runExceptT

runExceptStateT :: s -> StateT s (ExceptT String m) a -> m (Either String (a, s))
runExceptStateT s = runExceptT . flip runStateT s

data CellState = CellState { csRand :: Random.StdGen
                           , csNonce :: Integer
                           } deriving (Show)


---runHCell :: s -> Control.Monad.Trans.Except.ExceptT e (Control.Monad.Trans.State.Lazy.StateT s m) a -> m (Either e a, s)


runHCell :: s -> StateT s (ExceptT String m) a -> m (Either String (a, s))
runHCell f x = runExceptStateT f x

newCS :: CellState
newCS = CellState (Random.mkStdGen (fromIntegral 0)) 0

runCellState :: Monad m => HCell a -> m (Either String (a, CellState))
runCellState = runHCell newCS



class Mass a where
  mass :: a -> Integer

data Loc = Loc { locX :: Integer
               , locY :: Integer
               } deriving (Show, Eq, Ord)
           
data Dir = E | N | W | S
         deriving (Show, Eq, Ord, Enum)

data CellType = JointCell Joint
              | LifeCell
              | EmptyCell
              | Transporter
                deriving (Show, Eq, Ord)

instance Mass CellType where
  mass (JointCell _) = 1
  mass LifeCell = 1
  mass EmptyCell = 0
  mass Transporter = 0

data Program = Program Integer (DV.Vector Code)
               deriving (Show, Eq, Ord)
                         
data Code = Move Dir
          | MoveRandom
          | NOP
          | PassLeft
          | PassRight
          | PassBoth
          | Flip
            deriving (Show, Eq, Ord)

data Size = Size { sizeW :: Integer
                 , sizeH :: Integer
                 } deriving (Show, Eq, Ord)

data Grid = Grid { gridCells :: DM.Map Loc CellType
                 , gridSize :: Size
                 } deriving (Show, Eq, Ord)

instance Mass Grid where
  mass (Grid g _) = sum $ map mass (DM.elems g)

-- transporters are one way paths.
-- send grids can't overlap recv grids in space.
data Transporter = Send Grid
                 | Recv Grid

data Universe = Universe { uGrid :: Grid
                         , uSize :: Size
                         , uTime :: Integer
                         , uLifeForms :: DSM.Set LifeForm
                         } deriving (Show)

instance Mass Universe where
  mass (Universe _ _ _ lifeForms) = sum $ DSM.map mass lifeForms

data Orientation = TopBottom { topOffset :: Integer
                             , bottomOffset :: Integer
                             } 
                 | LeftRight { leftOffset :: Integer
                             , rightOffset :: Integer
                             } deriving (Show, Eq, Ord)

data Joint = NonRotating Orientation
           | Rotatating Orientation
           deriving (Show, Eq, Ord)

instance Mass Joint where
  mass _ = 1

data LifeForm = Complex Loc Program Joint LifeForm LifeForm
              | Simple { simpleId :: Integer
                       , simpleLoc :: Loc
                       , simpleProg :: Program
                       , simpleGrid :: Grid
                       } deriving (Show)

instance Ord LifeForm where
  (<=) x _ = True

instance Eq LifeForm where
  (==) l1 l2 = simpleId l1 == simpleId l2



lifeFormGrid (Complex _ _ _ lf1 lf2) = lifeFormGrid lf1 ++ lifeFormGrid lf2
lifeFormGrid (Simple _ _ _ g) = [g]
                         
instance Mass LifeForm where
  mass (Complex _ _ j lf1 lf2) = mass j + mass lf1 + mass lf2
  mass (Simple _ _ _ g) = mass g

class Draw a where
  draw :: a -> IO ()


