module Main where

import Graphics.Gloss

import PegBoard
import PlayGame
import CriticalPoints
import Analytics
import Symmetries
import Graphics

b =
  removePeg (Coord 1 1) $
  -- removePeg (Coord 2 1) .
  -- removePeg (Coord 2 2) .
  -- removePeg (Coord 3 1) .
  -- removePeg (Coord 3 2) .
  -- removePeg (Coord 3 3) .
  -- removePeg (Coord 4 1) .
  -- removePeg (Coord 4 2) .
  -- removePeg (Coord 4 3) $
  -- removePeg (Coord 4 4) .
  -- removePeg (Coord 5 1) .
  -- removePeg (Coord 5 2) .
  -- removePeg (Coord 5 3) .
  -- removePeg (Coord 5 4) .
  -- removePeg (Coord 5 5) $
  makeBoard 5
concB = concentricTriangles b
concB' = concentricTrianglesExclusive b

b' :: [Board]
b' = collectLog . head . endWith 1 .  playGameLog $ b

main :: IO ()
-- main = do
--   let printBoard = putStrLn . showBoard
--
--   putStrLn "Base Board"
--   printBoard b
--   putStrLn "\nClockwise Rotation"
--   printBoard . clockRotate $ b
--   putStrLn "\nCounter-clockwise Rotation"
--   printBoard . counterClockRotate $ b
--   putStrLn "\nZed Flip"
--   printBoard . zedFlip $ b
--   putStrLn "\nPos Flip"
--   printBoard . posFlip $ b
--   putStrLn "\nNeg Flip"
--   printBoard . negFlip $ b
--
--   putStrLn "\nSymmetries:"
--   mapM_ print . findSymmetries $ b

-- If negSymmetric, clockRotate == zedFlip and
--                  counterClockRotate == posFlip

-- main = displayBoard b
-- main = displayBoards b'
-- main = displayBoardsSquare b'

-- main = do
--   -- mapM_ (putStrLn . showBoard) concB
--   mapM_ (putStrLn . showBoard) concB'
--   print . last $ concB'
--
--   -- putStrLn . showBoard . zedFlip $ b
--   putStrLn . showBoard . clockRotate $ b

main = displayInteractive b'
