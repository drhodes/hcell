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

-- #if !MIN_VERSION_base(4,8,0)    
-- import Data.Foldable
-- #endif

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 640)

step :: Universe -> CellState -> IO (Universe, CellState, Loc)
step u cs = do
  result <- runHCell cs (Universe.step u)
  case result of
    Left errmsg -> do putStrLn errmsg
                      return (u, cs, Loc 1 1)
    Right (u', cs') -> do
      let lfs = uLifeForms u'          
      return (u', cs', simpleLoc $ head $ take 1 (DSM.toList lfs))


mainLoop :: Universe -> CellState -> IO ()
mainLoop uv cellState = do
  SDL.initialize [ SDL.InitVideo ]

  let winConfig = SDL.defaultWindow {
        SDL.windowInitialSize = V2 screenWidth screenHeight }

  window <- SDL.createWindow "hcell" winConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  let tileSize = 4
  let smidge = 2
  
  let loop u cs = do
        (u', cs', Loc x' y') <- step u cs
        let x = tileSize * fromIntegral x'
        let y = tileSize * fromIntegral y'
        print (x, y)
        let square1 = SDL.Rectangle (P (V2 x y)) (V2 tileSize tileSize)
        let foo = tileSize - smidge * 2
        let square2 = SDL.Rectangle (P (V2 (x+2 :: CInt) (y+2 :: CInt))) (V2 foo foo)
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

        unless quit $ loop u' cs'
        
  loop uv cellState

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
