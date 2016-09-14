{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad
import           Draw
import qualified LifeForm
import qualified Program
import           Types
import qualified Universe
-- import qualified Display
import qualified Util
import qualified DisplayGrid.Api as DG
import qualified DisplayGrid.Types as DT

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
  LifeForm.new (Loc x y) (Program.new [ MoveRandom
                                      , NOP
                                      ]) [ "**"
                                         , ".*"]

blob :: HCell LifeForm
blob = do
  x' <- Util.randomInt
  y' <- Util.randomInt
  let x = fromIntegral $ x' `mod` 100
      y = fromIntegral $ y' `mod` 100
  LifeForm.new (Loc x y) (Program.new [ MoveRandom
                                      , NOP
                                      ]) [ "**"
                                         , "**"]


beast :: HCell LifeForm
beast = do
  x' <- Util.randomInt
  y' <- Util.randomInt
  let x = fromIntegral $ x' `mod` 100
      y = fromIntegral $ y' `mod` 100
      prog = Program.new [ MoveRandom, NOP, NOP, NOP]
      prog' = head $ drop (x' `mod` 4) $ iterate Program.rotate prog
  LifeForm.new (Loc x y) prog' [ "*.*"
                               , "*.*"
                               , "***"
                               ]

newU = Universe.new (Size 100 100)


buildCritters :: HCell Universe
buildCritters = do
  x <- replicateM 10 $ algea N
  y <- replicateM 10 beast
  z <- replicateM 10 amoeba
  w <- replicateM 10 blob
  let critters = concat [x, y, z, w]
  foldM Universe.addLifeForm newU critters

setup = do
  DG.setWindowTitle "Heirarchical Cellular"
  DG.setCellSize 6
  DG.setWindowHeight 100
  DG.setWindowWidth 100

everyFrame (u, cs) = do
  frame <- DG.getCurrentFrame
  when (frame == 0) setup
  
  DG.clearScreen DG.grayA
  
  stepResult <- runHCell cs (Universe.step u)
  case stepResult of
      Right (u', cs') -> do
        Universe.display u
        return (u', cs')
      
      Left (msg) -> do
        DG.pushInstruction $ DT.Print msg
        error msg

main :: IO ()
main = do
  Right (u, cs) <- runHCell newCS buildCritters
  DG.mainLoop everyFrame (u, cs)


  


  
