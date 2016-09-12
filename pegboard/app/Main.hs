module Main where

import Lib
import PegBoard
import PegBoardCriticalPoints
import PegBoardSymmetries

b =
  removePeg (Coord 1 1) .
  removePeg (Coord 2 1) .
  removePeg (Coord 3 1) .
  removePeg (Coord 3 2) .
  removePeg (Coord 4 1) .
  removePeg (Coord 4 2) .
  removePeg (Coord 5 1) .
  removePeg (Coord 5 2) .
  removePeg (Coord 6 1) .
  removePeg (Coord 6 2) .
  removePeg (Coord 7 1) $
  makeBoard 7
concB = concentricTriangles b
concB' = concentricTrianglesExclusive b

main :: IO ()
main = do
  -- let printBoard = putStrLn . showBoard
  -- printBoard b
  -- printBoard $ zedSym b
  -- mapM_ printBoard concB
  putStrLn "Concentric Triangles"
  mapM_ print concB
  putStrLn "Concentric Triangles Exclusive"
  mapM_ print concB'
