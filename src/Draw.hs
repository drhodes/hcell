{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Draw where

import           Control.Monad
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Grid
import           Types

instance Draw CellType where
  draw (JointCell _) = putStr "j "
  draw LifeCell = putStr "* "
  draw EmptyCell = putStr "· "
  draw Transporter = putStr "T "

instance Draw [(Loc, Maybe CellType)] where
  draw [] = return () 
  draw ((_, Just cell):xs) = do draw cell
                                draw xs

instance Draw Grid where
  draw (Grid cells size) = do
    let Size h w = size
        hr cornerL cornerR = putStrLn $ concat
          [ cornerL
          , concat $ take (1 + fromIntegral w) (repeat "══")
          , cornerR ]
    hr "╔" "╗"
    forM_  [[(Loc x y, DM.lookup (Loc x y) cells) | x <- [0 .. w]] | y <- [0 .. h]]
      (\row -> do putStr "║"
                  draw row
                  putStr "║\n")
    hr "╚" "╝"
    
instance Draw Universe where
  draw (Universe g _ s time ls) = do
    putStrLn $ concat [ "A Universe of size: "
                      , show s
                      , ", at time: ", show time]
    draw (Grid.addLifeForms g ls)
