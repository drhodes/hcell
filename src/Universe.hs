{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Universe where

import qualified Data.Map as DM
import qualified Grid
import qualified LifeForm
import           Types
import Control.Monad
import qualified System.Random.Shuffle as Shuffle
import Control.Monad.State
import qualified CollisionGrid

new size = Universe (Grid.new size) (CollisionGrid.new size) size 0 (DM.empty)

addLifeForm u@(Universe _ cg _ _ lfs) lf = do
  let forms = DM.insert (simpleId lf) lf lfs
  cgrid <- CollisionGrid.insertLifeForm cg lf  
  return $ u { uLifeForms = forms, uCollisionGrid = cgrid }

step1 :: Universe -> LifeForm -> HCell Universe
step1 u@Universe{..} lf = do
  -- clear this lifeForms cells, so after stepping the lifeform it
  -- doesn't collide with itself  
  cgrid' <- CollisionGrid.clearLifeCells uCollisionGrid lf
  lifeForm' <- LifeForm.step lf uSize
  collides <- CollisionGrid.doesCollide cgrid' lifeForm'

  if collides
    then return u
    else do cgrid'' <- CollisionGrid.insertLifeForm cgrid' lifeForm'
            let lifeForms = DM.insert (simpleId lf) lifeForm' uLifeForms -- replace 
            return u{uLifeForms = lifeForms, uCollisionGrid = cgrid'}

step :: Universe -> HCell Universe
step u@(Universe g cg s t lfs) = do
  seed <- liftM csRand get
  -- randomize the lifeforms to make things fair
  let lifeforms = DM.elems lfs
      numforms = length lifeforms
      shuffledLife = Shuffle.shuffle' lifeforms numforms seed
  liftM incrementTime $ foldM step1 u shuffledLife

stepN n u = do
  if n <= 0
    then return u
    else do u' <- step u
            stepN (n-1) u'

incrementTime u = let t = uTime u in u{uTime = t + 1}

-------------------------------------------------------



