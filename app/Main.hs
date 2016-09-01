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

amoeba :: HCell LifeForm
amoeba = do
  x' <- Util.randomInt
  y' <- Util.randomInt
  let x = fromIntegral $ x' `mod` 100
      y = fromIntegral $ y' `mod` 100
  LifeForm.new (Loc x y) (Program.new [ MoveRandom ]) [ "**"
                                                      , ".*"]

uBeast :: HCell LifeForm
uBeast = do
  x' <- Util.randomInt
  y' <- Util.randomInt
  let x = fromIntegral $ x' `mod` 100
      y = fromIntegral $ y' `mod` 100
  LifeForm.new (Loc x y) (Program.new [ MoveRandom ]) [ "*.*"
                                                      , "*.*"
                                                      , "***"
                                                      ]


newU = Universe.new (Size 50 50)

main :: IO ()
main = do
  Right (lf1, cs) <- runHCell newCS (replicateM 10 (algea N))
  Right (uBeasts, cs') <- runHCell cs (replicateM 10 uBeast)
  Right (amoebas, cs'') <- runHCell cs' (replicateM 10 amoeba) 
  
  let u = foldl Universe.addLifeForm newU (lf1 ++ uBeasts ++ amoebas)
  Display.mainLoop u cs''
  


  
