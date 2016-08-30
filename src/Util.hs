{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Util where

import           Control.Monad
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified System.Random as Random
import           Types
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified System.Random as Random
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Functor.Identity
import Control.Monad.Except


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
  
