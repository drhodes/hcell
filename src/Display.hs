{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Display (mainLoop) where

import Prelude hiding (init)
import qualified Data.Set.Monad as DSM
import Control.Applicative
import Control.Monad
import Data.Monoid
import Foreign.C.Types
import Linear
import Linear.Affine ( Point(P) )
import qualified SDL
import SDL (($=))
import Types
import qualified Universe
import qualified Control.Parallel.Strategies as CPS
import Color

#if !MIN_VERSION_base(4,8,0)    
import Data.Foldable
#endif

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 800)

step :: Integer -> Universe -> CellState -> IO (Universe, CellState, [LifeForm])
step n u cs = do
  result <- runHCell cs (Universe.stepN n u)
  case result of
    Left errmsg -> do putStrLn errmsg
                      return (u, cs, [])
    Right (u', cs') -> do
      let lfs = uLifeForms u'          
      return (u', cs', DSM.toList lfs)

-- these will go into a State value.
tileSize = 8
smidge = 1

renderLifeForm renderer lf = do
  let (Loc x' y') = simpleLoc lf
  let x = tileSize * fromIntegral x' `mod` screenWidth
  let y = tileSize * fromIntegral y' `mod` screenHeight
  let square1 = SDL.Rectangle (P (V2 x y)) (V2 tileSize tileSize)
  let foo = tileSize - smidge * 2
  let square2 = SDL.Rectangle (P (V2 (x+smidge :: CInt) (y+smidge :: CInt))) (V2 foo foo)
  
  SDL.rendererDrawColor renderer $= gray1
  SDL.fillRect renderer (Just square1)
  SDL.rendererDrawColor renderer $= cellBlue
  SDL.fillRect renderer (Just square2)

mainLoop :: Universe -> CellState -> IO ()
mainLoop uv cellState = do
  SDL.initialize [ SDL.InitVideo ]

  let winConfig = SDL.defaultWindow {
        SDL.windowInitialSize = V2 screenWidth screenHeight }

  window <- SDL.createWindow "hcell" winConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  
  let loop frameNumber u cs = do
        (u', cs', lfs) <- step 1 u cs
        events <- SDL.pollEvents
        let (Any quit, Last newSpriteRect) =
              foldMap (\case
                SDL.QuitEvent -> (Any True, mempty)
                SDL.KeyboardEvent e ->
                  if | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                         case SDL.keysymScancode (SDL.keyboardEventKeysym e) of
                           SDL.ScancodeQ -> (Any True,  mempty)
                           _ -> mempty
                     | otherwise -> mempty
                _ -> mempty) $
              map SDL.eventPayload events
        SDL.rendererDrawColor renderer $= grayD
        SDL.clear renderer
        --SDL.rendererDrawColor renderer $= gray7
        mapM (renderLifeForm renderer) lfs
        SDL.present renderer

        unless quit $ (if frameNumber `mod` 5 == 0
                       then loop (frameNumber+1) u' cs'
                       else loop (frameNumber+1) u cs)
        
  loop 0 uv cellState

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


