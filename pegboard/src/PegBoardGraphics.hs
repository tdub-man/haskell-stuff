module PegBoardGraphics
    ( renderBoard
    , renderBoards
    , displayBoard
    , displayBoards
    ) where

import PegBoard
import PegBoardCriticalPoints
import Graphics.Gloss
import Helpers.Lists

-- Problem with offset/offsetF

offset :: Float -> Float -> [Picture] -> [Picture]
-- offset x y ps = ps' where
--   offN n = map (*n) [1..]
--   offsetXY = zip (offN x) (offN y)
--   psOffsets = zip ps offsetXY
--   ps' = map (\(p,(ox,oy)) -> translate ox oy p) psOffsets
offset x y ps = ps' where
  f m = (*m) . fromIntegral
  ps' = offsetF (f x) (f y) ps

offsetF :: (Integral a) => (a -> Float) -> (a -> Float) -> [Picture] -> [Picture]
offsetF fx fy ps = ps' where
  offF f = map f [1..]
  offsetXY = zip (offF fx) (offF fy)
  psOffsets = zip ps offsetXY
  ps' = map (\(p,(ox,oy)) -> translate ox oy p) psOffsets

renderBoard :: Board -> Picture
renderBoard b = b' where
  rs = rows' b
  rs' = map (map snd) rs
  assignCircle c = if c
    then color green (circleSolid 8)
    else color red   (circleSolid 8)
  circs = map (map assignCircle) rs'
  circs' = map (offset 16 0) circs
  circRows = map pictures circs'
  circRows' = offset 8 16 (reverse circRows)
  b' = pictures circRows'

rBoards :: [Board] -> [Picture]
rBoards = map renderBoard

renderBoards :: [Board] -> Picture
renderBoards [] = blank
renderBoards bs = bs' where
  -- bsR = map renderBoard bs
  bsR = rBoards bs
  nRows = fromIntegral . length . rows' . head $ bs
  -- offN n = (16 * n) * (nRows + 1)
  offN n = fromIntegral (16 * n) * (nRows + 1)
  bsR' = zip (reverse bsR) [1..]
  offsetB (b,n) = translate 0 (offN n) b
  bsrOff = map offsetB bsR'
  -- bsrOff = offsetF (const 0) offN bsR
  bs' = pictures bsrOff

renderBoardsSquare :: [Board] -> Picture
renderBoardsSquare [] = blank
renderBoardsSquare bs = blank where
  sqLen = ceiling . sqrt . fromIntegral . length $ bs
  nRows = fromIntegral . length . rows' . head $ bs
  bsR = rBoards bs
  offN n = (16 * n) * (nRows + 1)
  bsrRow = subDivide sqLen bsR
  bsrRow' = map ( (`zip` [1..]) . reverse) bsrRow
  offsetB (b,n) = translate 0 (offN n) b
  -- bsrRowOff =

-- Animate?

displayBoard :: Board -> IO ()
displayBoard b = display
                 (InWindow "PegBoard" (600,600) (20,20))
                 black
                 (renderBoard b)

displayBoards :: [Board] -> IO ()
displayBoards bs = display
                   (InWindow "PegBoard" (600,600) (20,20))
                   black
                   (renderBoards bs)
