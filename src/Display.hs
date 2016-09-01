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
import qualified Grid
import qualified Data.Map as DM
import qualified Loc

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (1024, 1024)

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
tileSize = 16
smidge = 2

renderLifeForm renderer lf = do
  let (DisplayGrid dgrid _) = Grid.toDisplayGrid (simpleGrid lf)
      dblocks = DM.toList dgrid
      offset = simpleLoc lf
  mapM (renderDisplayBlock renderer offset) dblocks

renderDisplayBlock renderer offset (loc@(Loc x' y'), Dblock cellType shards) =
  case cellType of
    EmptyCell -> return ()
    LifeCell -> 
      if cellType == LifeCell
      then do let loc'@(Loc x'' y'') = Loc.add loc offset
              let x = (tileSize * fromIntegral x'' `mod` screenWidth) 
              let y = (tileSize * fromIntegral y'' `mod` screenHeight)
              let square1 = SDL.Rectangle (P (V2 x y)) (V2 tileSize tileSize)
              SDL.rendererDrawColor renderer $= gray4
              SDL.fillRect renderer (Just square1)
              mapM_ (renderShard renderer gray7 loc') shards
      else return () -- this could seem like a BUG! ALERT
           
renderDisplayBlock _ _ _ = return ()


-- 1 2 3
-- 4 5 6 
-- 7 8 9      
renderShard :: SDL.Renderer -> t -> Loc -> DisplayShard -> IO ()
renderShard renderer color (Loc x' y') shard = do
  let (dx, dy) = case shard of
        D2 -> (smidge, -smidge)
        D6 -> (smidge*2, smidge)
        D4 -> (0, smidge)
        D8 -> (smidge, smidge*2)
        _ -> (smidge, smidge)
  let x = (tileSize * fromIntegral x' `mod` screenWidth) + dx
  let y = (tileSize * fromIntegral y' `mod` screenHeight) + dy
  let w = tileSize - (smidge * 2)
  let square2 = SDL.Rectangle (P (V2 (x :: CInt) (y :: CInt))) (V2 w w)

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

        unless quit $ (if frameNumber `mod` 1 == 0
                       then loop (frameNumber+1) u' cs'
                       else loop (frameNumber+1) u cs)
        
  loop 0 uv cellState

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


