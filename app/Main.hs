{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Main where

import           Control.Monad
import qualified LifeForm
import qualified Program
import           Types
import qualified Universe
import qualified Util
import qualified DisplayGrid.Api as DG
import qualified DisplayGrid.Types as DT
import qualified SDL
  
algeaPos :: Dir -> HCell LifeForm
algeaPos d = do
  Loc x y <- randomLoc
  LifeForm.new (Loc x y) (Program.new [ MoveRandom , Move d ]) ["+"] 

algeaNeg :: Dir -> HCell LifeForm
algeaNeg d = do
  Loc x y <- randomLoc
  LifeForm.new (Loc x y) (Program.new [ MoveRandom , Move d ]) ["-"] 

amoeba :: HCell LifeForm
amoeba = do
  Loc x y <- randomLoc
  LifeForm.new (Loc x y) (Program.new [ MoveRandom
                                      , NOP
                                      ]) [ "++"
                                         , ".+"]

blob :: HCell LifeForm
blob = do
  Loc x y <- randomLoc
  LifeForm.new (Loc x y) (Program.new [ MoveRandom
                                      , NOP
                                      ]) [ "++"
                                         , "++"]
    
randomLoc :: HCell Loc
randomLoc = do
  x' <- Util.randomInt
  y' <- Util.randomInt
  
  let x = fromIntegral $ x' `mod` 75
      y = fromIntegral $ y' `mod` 75
  return $ Loc x y

beast :: HCell LifeForm
beast = do
  Loc x y <- randomLoc
  let prog = Program.new [ MoveRandom
                         , FlipV
                         , MoveRandom
                         , MoveRandom
                         ]
      prog' = head $ drop (fromIntegral x `mod` 4) $ iterate Program.rotate prog
  LifeForm.new (Loc x y) prog' [ "+.+"
                               , "+.+"
                               , "+++"
                               ]

newU = Universe.new (Size 200 200)

buildCritters :: HCell Universe
buildCritters = do
  x <- replicateM 300 $ algeaPos N
  p <- replicateM 300 $ algeaNeg N
  c <- replicateM 30 $ beast
  let critters = concat [x, p, c]
  foldM Universe.addLifeForm newU critters

setup :: Universe -> DT.GridT ()
setup u = do
  let (Size w h) = uSize u
  DG.setWindowTitle "Localized Cellular Heirarchy"
  DG.setCellSize 5
  DG.setWindowHeight (fromIntegral w)
  DG.setWindowWidth (fromIntegral h)

everyFrame :: (Universe, CellState) -> DT.GridT (Universe, CellState)
everyFrame (u, cs) = do
  frame <- DG.getCurrentFrame
  when (frame == 0) $ setup u
  
  DG.clearScreen DG.grayA
  DG.getEvents >>= handleEvents
  
  stepResult <- runHCell cs (Universe.step u)
  case stepResult of
      Right (u', cs') -> do
        Universe.display u
        return (u', cs')
      
      Left (msg) -> do
        DG.pushInstruction $ DT.Print msg
        error msg

handleEvent :: SDL.Event -> DT.GridT ()
handleEvent event =
  case SDL.eventPayload event of
    SDL.MouseMotionEvent payload ->
      case payload of
        SDL.MouseMotionEventData _ _ _ pos _ -> do
          cellSize <- DG.getCellSize
          let (SDL.P (SDL.V2 x y)) = pos
              x' = x `div` fromIntegral cellSize
              y' = y `div` fromIntegral cellSize
              loc = DT.CellLoc (fromIntegral x') (fromIntegral y')
          DG.setCellColor (DG.colorFromHex 0xFF0000FF) loc
    _ -> return ()
    
handleEvents :: [SDL.Event] -> DT.GridT ()
handleEvents events = mapM_ handleEvent events

main :: IO ()
main = do
  Right (u, cs) <- runHCell newCS buildCritters
  DG.mainLoop everyFrame (u, cs)
