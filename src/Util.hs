{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Util where

import           Control.Monad
import qualified Data.Set as DS
import           Types
import qualified Data.Map as DM
import qualified Data.Vector as DV
import qualified System.Random as Random
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Functor.Identity
import Control.Monad.Except
import qualified SDL
import qualified DisplayGrid.Types as DT
import Linear

randomInt :: HCell Int
randomInt = do
  cs <- get
  let (x, gen') = Random.next (csRand cs)
  put (cs { csRand = gen' })
  return x

nextNonce :: HCell Integer
nextNonce = do
  cs <- get
  let n = csNonce cs
  put cs{csNonce = n + 1}
  return n
  

randomColor :: HCell DT.Color
randomColor = do
  r' <- randomInt
  g' <- randomInt
  b' <- randomInt
  
  let r = fromIntegral $ 50 + r' `mod` 100
      g = fromIntegral $ 50 + g' `mod` 100
      b = fromIntegral $ 50 + b' `mod` 100
      a = fromIntegral $ 0xFF
      
  return $ SDL.V4 r g b a
