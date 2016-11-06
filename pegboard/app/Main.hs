module Main where

import PegBoard
import PegBoardMove
import PlayGame
import CriticalPoints
import Analytics
import Symmetries
import Graphics
import Solve

import Graphics.Gloss
import Helpers.Lists(concatZip)

b =
  removePeg (Coord 1 1) .
  removePeg (Coord 2 1) .
  -- removePeg (Coord 2 2) $
  removePeg (Coord 3 1) .
  removePeg (Coord 3 2) $
  -- removePeg (Coord 3 3) .
  -- removePeg (Coord 4 1) .
  -- removePeg (Coord 4 2) .
  -- removePeg (Coord 4 3) .
  -- removePeg (Coord 4 4) .
  -- removePeg (Coord 5 1) .
  -- removePeg (Coord 5 2) .
  -- removePeg (Coord 5 3) .
  -- removePeg (Coord 5 4) $
  -- removePeg (Coord 5 5) $
  makeBoard 5
concB = concentricTriangles b
concB' = concentricTrianglesExclusive b

solveFor :: Board -> Int -> BoardLog
solveFor b n = head . endWith n . playGameLog $ b

solution :: Board -> Int -> [Board]
solution b = collectLog . solveFor b

bl :: BoardLog
bl = solveFor b 1

b' :: [Board]
b' = collectLog bl

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard

boardInfo :: BoardLog -> [String]
boardInfo blog = let
  prsym = zipWith (curry show) (movePegRatios bl) (boardSymmetries bl)
  prsym' = map ("\n" ++) prsym
  boards = showBoardLog bl
  in concatZip prsym' boards

main :: IO ()
main = do

-- CRITICAL POINTS
  -- printBoard b
  -- mapM_ printBoard concB
  -- mapM_ printBoard concB'
  -- mapM_ print [topLeftRowBR' b, topRightRowBR' b, bottomRowBR' b]
  -- print . map brCoord . rowsZ $ b
  -- print . map brCoord . rowsP $ b
  -- print . map brCoord . rowsN $ b

--------------------------------------------------------------------------------

-- BOARD DISPLAYS

  -- displayBoard b
  -- displayBoards b'
  -- displayBoardsSquare b'
--------------------------------------------------------------------------------

-- TRANSFORMATIONS

  -- putStrLn "Base Board"
  -- printBoard b
  -- putStrLn "\nClockwise Rotation"
  -- printBoard . clockRotate $ b
  -- putStrLn "\nCounter-clockwise Rotation"
  -- printBoard . counterClockRotate $ b
  -- putStrLn "\nZed Flip"
  -- printBoard . zedFlip $ b
  -- putStrLn "\nPos Flip"
  -- printBoard . posFlip $ b
  -- putStrLn "\nNeg Flip"
  -- printBoard . negFlip $ b
--------------------------------------------------------------------------------

-- SYMMETRIES

  -- putStrLn "\nSymmetries:"
  -- mapM_ print . findSymmetries $ b
--------------------------------------------------------------------------------

-- INTERACTIVE

  -- displayInteractive b'
  promptSolve
--------------------------------------------------------------------------------

-- ANALYTICS

  -- let
  --   points = critPoints $ makeBoard 5
  --   boards = map (`removePeg` makeBoard 5) points
  --   blogs = map (`solveFor` 1) boards
  --   infos = map boardInfo blogs
  -- mapM_ (mapM_ putStrLn) infos
--------------------------------------------------------------------------------

-- Transform board to a critical point form
-- Solve special cases (the critical point forms)
