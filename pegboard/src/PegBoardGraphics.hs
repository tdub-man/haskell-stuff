module PegBoardGraphics
    ( renderBoard
    , displayBoard
    ) where

import PegBoard
import PegBoardCriticalPoints
import Graphics.Gloss

renderBoard :: Board -> Picture
renderBoard b = b' where
  rs = rows' b
  rs' = map (map snd) rs
  assignCircle c = if c
    then color green circ
    else color red circ
    where
      circ = circleSolid 8
  circs = map (map assignCircle) rs'
  offsets n = [ n*x | x <- [1..] ]
  transCircs x y cs = cs'' where
    offs = zip (offsets x) (offsets y)
    cs' = zip cs offs
    cs'' = map (\(c,(ox,oy)) -> translate ox oy c) cs'
  circs' = map (transCircs 16 0) circs
  circRows = map pictures circs'
  circRows' = transCircs 8 16 (reverse circRows)
  b' = pictures circRows'

displayBoard :: Board -> IO ()
displayBoard b = display
                 (InWindow "PegBoard" (600,600) (20,20))
                 black
                 (renderBoard b)
