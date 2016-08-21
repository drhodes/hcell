module Main where

import           Control.Monad
import           Draw
import qualified LifeForm
import qualified Program
import           Types
import qualified Universe


algea lid = LifeForm.new lid (Loc 5 5) (Program.new [MoveRandom]) ["*"]
amoeba lid x y = LifeForm.new lid (Loc x y) (Program.new [MoveRandom]) [ "**" ]

amoebas = [amoeba x 25 25 | x <- [0 .. 400]]

stepN u = do
  n <- liftM (\str -> read str :: Integer) getLine
  let f i u' = if i == 0
               then step u'
               else do f (i - 1) (Universe.step u')
  f n u


step u = do
  draw u
  c <- getLine
  case c of
    "q" -> return ()
    "s" -> stepN u
    _ -> step (Universe.step u)

  
main = let u = (Universe.new (Size 50 50))
           u' = foldl Universe.addLifeForm u $ amoebas
       in step u'
  
  
