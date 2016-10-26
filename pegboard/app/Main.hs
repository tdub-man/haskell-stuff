module Main where

import Graphics.Gloss

import PegBoard
import PlayGame
import CriticalPoints
import Analytics
import Symmetries
import Graphics
import Solve

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

testPromptR :: Picture
testPromptR = labeledPrompt where
  board = renderBoard . makeBoard $ 5
  rowLabels = pictures . offset 0 16 $ [ scale 0.1 0.1 $ Text (show n) | n <- [5,4..1] ]
  rowLabels' = translate 0 (-4) . color white $ rowLabels
  rowLines  = pictures [ Line [(0,16*n),(l+16,16*n)] | (n,l) <- zip [1..5] [0,8..32] ]
  rowLines' = color white rowLines
  labeledPrompt = pictures . offset 16 0 $ [rowLabels',rowLines',board]

testPromptC :: Picture
testPromptC = labeledPrompt where
  board = renderBoard . makeBoard $ 5
  colLabels = [ translate 20 36 . scale 0.1 0.1 . Text $ show n | n <- [5,4..1] ]
  colLines = replicate 5 (Line [(0,0),(16,32)])
  colLabelLines = [ pictures [lb,ln] | (lb,ln) <- zip colLabels colLines ]
  colLabelLines' = color white . translate 104 8 . pictures . offset (-8) 16 $ colLabelLines
  labeledPrompt = pictures [board,colLabelLines']

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

-- main = displayInteractive b'

-- main = display
--   (InWindow "TestPrompt" (600,600) (0,0))
--   black
--   testPromptC

main = promptSolve
