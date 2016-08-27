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



algea :: HCell LifeForm
algea = LifeForm.new (Loc 5 5) (Program.new [MoveRandom]) ["*"]

-- amoeba :: _ -> _ -> HCell LifeForm
-- amoeba x y = LifeForm.new (Loc x y) (Program.new [MoveRandom]) [ "**" ]

-- amoebas = mapM amoeba [amoeba x 25 25 | x <- [0 .. 40]]

stepN :: Universe -> CellState -> IO ()
stepN u cs = do
  n <- liftM (\str -> read str :: Integer) getLine
  let f :: Integer -> Universe -> CellState -> IO ()
      f i u' cs' =
        if i == 0
        then step u' cs'
        else do result <- runHCell cs' (Universe.step u')
                case result of
                  Left errmsg -> print errmsg
                  Right (u'', cs'') -> f (i - 1) u'' cs''
  f n u cs
  
step :: Universe -> CellState -> IO ()
step u cs = do
  draw u
  c <- getLine
  case c of
    "q" -> return ()
    "s" -> stepN u cs
    _ -> do result <- runHCell cs (Universe.step u)
            case result of
              Left errmsg -> putStrLn errmsg
              Right (u', cs') -> step u' cs'
  
-- main = let u = (Universe.new (Size 50 50))
--            u' = foldl Universe.addLifeForm u $ amoebas
--        in step u'

newU = Universe.new (Size 50 50)


main :: IO ()
main = do
  Right (lf, cs) <- runHCell newCS algea
  let u = Universe.addLifeForm newU lf
  --step u cs
--   -- pass it off to Display, or lift Display into it.
  Display.mainLoop u cs
--   -- putStrLn "done"


  
