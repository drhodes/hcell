{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad
import           Draw
import qualified LifeForm
import qualified Program
import           Types
import qualified Universe
import qualified Display
import qualified Util

algea :: Dir -> HCell LifeForm
algea d = do
  x' <- Util.randomInt
  y' <- Util.randomInt
  let x = fromIntegral $ x' `mod` 100
      y = fromIntegral $ y' `mod` 100
  LifeForm.new (Loc x y) (Program.new [ MoveRandom , Move d ]) ["*"]

newU = Universe.new (Size 50 50)

main :: IO ()
main = do
  Right (lf1, cs) <- runHCell newCS (replicateM 100 (algea N))
  Right (lf2, cs) <- runHCell newCS (replicateM 100 (algea E))
  
  let u = foldl Universe.addLifeForm newU (lf1 ++ lf2)
  Display.mainLoop u cs


  
