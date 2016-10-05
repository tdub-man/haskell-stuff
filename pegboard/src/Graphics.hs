module Graphics
    ( renderBoard
    , renderBoards
    , renderBoardsSquare
    , displayBoard
    , displayBoards
    , displayBoardsSquare
    , displayInteractive
    ) where

import PegBoard
import CriticalPoints
import Helpers.Lists
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

offset :: Float -> Float -> [Picture] -> [Picture]
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
  bsR = rBoards bs
  nRows = fromIntegral . length . rows' . head $ bs
  offN n = fromIntegral (16 * n) * (nRows + 1)
  bsrOff = offsetF (const 0) offN (reverse bsR)
  bs' = pictures bsrOff

renderBoardsSquare :: [Board] -> Picture
renderBoardsSquare [] = blank
renderBoardsSquare bs = bs' where
  sqLen = ceiling . sqrt . fromIntegral . length $ bs
  nRows = fromIntegral . length . rows' . head $ bs
  bsR = rBoards bs
  offN n = (16 * fromIntegral n) * (nRows + 1)
  bsrRow = subDivide sqLen bsR
  bsrRow' = map ( (`zip` [1..]) . reverse) bsrRow
  offsetB (b,n) = translate 0 (offN n) b
  bsrRowOff = map (pictures . offsetF offN (const 0)) bsrRow
  bs' = pictures . offsetF (const 0) offN $ reverse bsrRowOff

-- Animate?

displayBoard :: Board -> IO ()
displayBoard b = display
                 (InWindow "PegBoard" (600,600) (0,0))
                 black
                 (renderBoard b)

displayBoards :: [Board] -> IO ()
displayBoards bs = display
                   (InWindow "PegBoard" (600,600) (0,0))
                   black
                   (renderBoards bs)

displayBoardsSquare :: [Board] -> IO ()
displayBoardsSquare bs = display
                         (InWindow "PegBoard" (600,600) (0,0))
                         black
                         (renderBoardsSquare bs)


type PBZipper = ([Board],[Board])

getPB :: PBZipper -> Picture
getPB ([],[])  = blank
getPB ([],b:_) = renderBoard b
getPB (a:_,_)  = renderBoard a

pbNext :: PBZipper -> PBZipper
pbNext ([],bs)   = ([],bs)
pbNext (x:as,bs) = (as,x:bs)

pbPrev :: PBZipper -> PBZipper
pbPrev (as,[])   = (as,[])
pbPrev (as,x:bs) = (x:as,bs)

handleEvent :: Event -> PBZipper -> PBZipper
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) pbz = pbNext pbz
handleEvent (EventKey (SpecialKey KeyLeft)  Down _ _) pbz = pbPrev pbz
handleEvent _                                         pbz = pbz

displayInteractive :: [Board] -> IO ()
displayInteractive bs =
  play
  (InWindow "PegBoard" (600,600) (0,0))
  black
  100
  (bs,[])
  getPB
  handleEvent
  (\_ x -> x)
