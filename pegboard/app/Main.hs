module Main where

import Lib
import PegBoard
import PegBoardCriticalPoints
import PegBoardSymmetries

b =
  -- removePeg (Coord 1 1) .
  -- removePeg (Coord 2 1) .
  -- removePeg (Coord 2 2) .
  -- removePeg (Coord 3 1) .
  removePeg (Coord 3 2) .
  -- removePeg (Coord 3 3) .
  -- removePeg (Coord 4 1) .
  removePeg (Coord 4 2) .
  removePeg (Coord 4 3) $
  -- removePeg (Coord 4 4) .
  -- removePeg (Coord 5 1) .
  -- removePeg (Coord 5 2) .
  -- removePeg (Coord 5 3) .
  -- removePeg (Coord 5 4) .
  -- removePeg (Coord 5 5) $
  makeBoard 5
concB = concentricTriangles b
concB' = concentricTrianglesExclusive b

main :: IO ()
main = do
  let printBoard = putStrLn . showBoard

  putStrLn "Base Board"
  printBoard b
  putStrLn "\nClockwise Rotation"
  printBoard . clockRotate $ b
  putStrLn "\nCounter-clockwise Rotation"
  printBoard . counterClockRotate $ b
  putStrLn "\nZed Flip"
  printBoard . zedFlip $ b
  putStrLn "\nPos Flip"
  printBoard . posFlip $ b
  putStrLn "\nNeg Flip"
  printBoard . negFlip $ b

  putStrLn "Symmetries"
  mapM_ print . findSymmetries $ b

-- If negSymmetric, clockRotate == zedFlip and
--                  counterClockRotate == posFlip
