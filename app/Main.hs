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
  let x = fromIntegral $ x' `mod` 1
      y = fromIntegral $ y' `mod` 1
  LifeForm.new (Loc x y) (Program.new [ MoveRandom , Move d ]) ["*"]

amoeba :: HCell LifeForm
amoeba = do
  x' <- Util.randomInt
  y' <- Util.randomInt
  let x = fromIntegral $ x' `mod` 1
      y = fromIntegral $ y' `mod` 1
  LifeForm.new (Loc x y) (Program.new [ MoveRandom
                                      , NOP
                                      ]) [ "**"
                                         , ".*"]

blob :: HCell LifeForm
blob = do
  x' <- Util.randomInt
  y' <- Util.randomInt
  let x = fromIntegral $ x' `mod` 1
      y = fromIntegral $ y' `mod` 1
  LifeForm.new (Loc x y) (Program.new [ MoveRandom
                                      , NOP
                                      ]) [ "**"
                                         , "**"]


beast :: HCell LifeForm
beast = do
  x' <- Util.randomInt
  y' <- Util.randomInt
  let x = fromIntegral $ x' `mod` 1
      y = fromIntegral $ y' `mod` 1
      prog = Program.new [ MoveRandom, NOP, NOP, NOP]
      prog' = head $ drop (x' `mod` 4) $ iterate Program.rotate prog
  LifeForm.new (Loc x y) prog' [ "*.*"
                               , "*.*"
                               , "***"
                               ]


newU = Universe.new (Size 50 50)


buildCritters = do
  x <- replicateM 10 $ algea N
  y <- replicateM 10 beast
  z <- replicateM 10 amoeba
  w <- replicateM 10 blob
  return $ concat [x, y, z, w]

main :: IO ()
main = do
  Right (critters, cs) <- runHCell newCS buildCritters
  
  let u = foldl Universe.addLifeForm newU critters
  Display.mainLoop u cs
  


  
