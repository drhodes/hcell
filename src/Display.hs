{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Display (mainLoop) where

import Prelude hiding (init)
import Control.Applicative
import Control.Monad
import Data.Monoid
import Foreign.C.Types
import Linear
import Linear.Affine ( Point(P) )
import qualified SDL
import SDL (($=))

-- #if !MIN_VERSION_base(4,8,0)    
-- import Data.Foldable
-- #endif

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 640)

mainLoop :: IO ()
mainLoop = do
  SDL.initialize [ SDL.InitVideo ]

  let winConfig = SDL.defaultWindow {
        SDL.windowInitialSize = V2 screenWidth screenHeight }

  window <- SDL.createWindow "hcell" winConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  
  let loop x = do
        let square1 = SDL.Rectangle (P (V2 x 20)) (V2 20 20)
        let square2 = SDL.Rectangle (P (V2 (x+2) 22)) (V2 16 16)
        events <- SDL.pollEvents
        let (Any quit, Last newSpriteRect) =
              foldMap (\case
                SDL.QuitEvent -> (Any True, mempty)
                SDL.KeyboardEvent e ->
                  if | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                         case SDL.keysymScancode (SDL.keyboardEventKeysym e) of
                           -- SDL.Scancode1 -> (Any False, Last (Just spriteOne))
                           -- SDL.Scancode2 -> (Any False, Last (Just spriteTwo))
                           -- SDL.Scancode3 -> (Any False, Last (Just spriteThree))
                           -- SDL.Scancode4 -> (Any False, Last (Just spriteFour))
                           SDL.ScancodeQ -> (Any True,  mempty)
                           _ -> mempty
                     | otherwise -> mempty
                _ -> mempty) $
              map SDL.eventPayload events
            
            --spriteRect' = newSpriteRect <|> spriteRect
        let gray3 = V4 0x3A 0x3A 0x3A 0xFF
        let gray7 = V4 0x7A 0x7A 0x7A 0xFF 
        let gray9 = V4 0x9A 0x9A 0x9A 0xFF
       
        SDL.rendererDrawColor renderer $= gray3
        SDL.clear renderer
        SDL.rendererDrawColor renderer $= gray7
        SDL.fillRect renderer (Just square1)
        SDL.rendererDrawColor renderer $= gray9
        SDL.fillRect renderer (Just square2)
        SDL.present renderer

        unless quit $ loop (0 `mod` 640)

  loop 0

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
