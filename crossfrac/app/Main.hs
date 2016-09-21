module Main where

import Graphics.Gloss
import qualified LifeSpan
import qualified TreeFrac
import qualified ClockFrac
import qualified Zen

import CrossFrac

main :: IO ()
-- main = LifeSpan.mn
-- main = TreeFrac.mn
-- main = ClockFrac.mn
-- main = Zen.mn

main = do
  n <- getLine
  dispFrac (read n :: Int)
