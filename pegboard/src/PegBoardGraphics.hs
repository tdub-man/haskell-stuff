module PegBoardGraphics
    ( renderBoard
    , renderBoards
    , displayBoard
    , displayBoards
    ) where

import PegBoard
import PegBoardCriticalPoints
import Graphics.Gloss

offset :: Float -> Float -> [Picture] -> [Picture]
offset x y ps = ps' where
  offN n = map (*n) [1..]
  offsetXY = zip (offN x) (offN y)
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

renderBoards :: [Board] -> Picture
renderBoards [] = blank
renderBoards bs = bs' where
  bsR = map renderBoard bs
  nRows = fromIntegral . length . rows' . head $ bs
  offN n = (16 * n) * (nRows + 1)
  bsR' = zip (reverse bsR) [1..]
  offsetB (b,n) = translate 0 (offN n) b
  bsrOff = map offsetB bsR'
  bs' = pictures bsrOff

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
