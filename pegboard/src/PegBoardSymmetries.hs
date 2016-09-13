module PegBoardSymmetries
    ( zedFlip
    , posFlip
    , negFlip
    , clockRotate
    , counterClockRotate
    , Symmetries(Positive,Horizontal,Negative,Clockwise,CounterClockwise)
    , posSymmetric
    , zedSymmetric
    , negSymmetric
    , clockSymmetric
    , counterClockSymmetric
    , findSymmetries
    ) where
import PegBoard
import PegBoardCriticalPoints( rows
                             , concentricTrianglesExclusive
                             , topRightRow, bottomRow, topLeftRow)
import Helpers.Lists(middleElem)
import Data.List(nubBy,nub)

boardSize :: Board -> Int
boardSize (Board ps hs) = x where
  (Coord x _) =  maximum $ ps ++ hs

combineBoard :: Board -> Board -> Board
combineBoard (Board p1 h1) (Board p2 h2) = Board ps hs where
  ps = nub $ p1 ++ p2
  hs = nub $ h1 ++ h2

zedFlip :: Board -> Board
zedFlip b@(Board ps hs) = Board ps' hs' where
  isPeg x = (x,x `elem` ps)
  pRows = rows $ ps ++ hs
  isPegRows = map (map isPeg) pRows
  revRows = map reverse isPegRows
  swapCoords (_,p) c = (c,p)
  pshs = concat $ zipWith (zipWith swapCoords) revRows pRows
  extract (c,_) = c
  ps' = map extract . filter snd $ pshs
  hs' = map extract . filter (not . snd) $ pshs
  -- [[ (C1,True) ], [ (C22,False),(C21,True) ]]
  -- [[ C1        ], [ C21,        C22        ]]

-- rotate concentric triangles
clockRotate :: Board -> Board
clockRotate b = b' where
  rings = concentricTrianglesExclusive b
  rotateRing x@(Board ps _) = Board ps' hs' where
    isPeg = (`elem` ps)
    (trRow,btRow,tlRow) = (topRightRow x,bottomRow x,topLeftRow x)
    (trRow',btRow',tlRow') = (map isPeg trRow, map isPeg btRow, map isPeg tlRow)
    (trRow'',btRow'',tlRow'') = (zip trRow tlRow', zip btRow trRow', zip tlRow btRow')
    sameCoords a b = fst a == fst b
    allRows = nubBy sameCoords $ trRow'' ++ btRow'' ++ tlRow''
    ps' = map fst . filter snd $ allRows
    hs' = map fst . filter (not . snd) $ allRows
  rings' = map rotateRing rings
  b' = foldl1 combineBoard rings'

counterClockRotate :: Board -> Board
counterClockRotate = clockRotate . clockRotate

posFlip :: Board -> Board
posFlip = counterClockRotate . zedFlip . clockRotate

negFlip :: Board -> Board
negFlip = clockRotate . zedFlip . counterClockRotate

data Symmetries = Positive
                | Horizontal
                | Negative
                | Clockwise
                | CounterClockwise
                | Not Symmetries deriving (Show)

posSymmetric :: Board -> Symmetries
posSymmetric b =
  if b == posFlip b
  then Positive
  else Not Positive

zedSymmetric :: Board -> Symmetries
zedSymmetric b =
  if b == zedFlip b
  then Horizontal
  else Not Horizontal

negSymmetric :: Board -> Symmetries
negSymmetric b =
  if b == negFlip b
  then Negative
  else Not Negative

clockSymmetric :: Board -> Symmetries
clockSymmetric b =
  if b == clockRotate b
  then Clockwise
  else Not Clockwise

counterClockSymmetric :: Board -> Symmetries
counterClockSymmetric b =
  if b == counterClockRotate b
  then CounterClockwise
  else Not CounterClockwise

findSymmetries :: Board -> [Symmetries]
findSymmetries b = [pSym,zSym,nSym,clSym,ccSym] where
  pSym  = posSymmetric b
  zSym  = zedSymmetric b
  nSym  = negSymmetric b
  clSym = clockSymmetric b
  ccSym = counterClockSymmetric b
