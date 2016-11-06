module Symmetries
    ( zedFlip
    , posFlip
    , negFlip
    , clockRotate
    , counterClockRotate
    , Symmetries(Positive,Horizontal,Negative,Rotational,All,Not)
    , posSymmetric
    , zedSymmetric
    , negSymmetric
    , rotSymmetric
    -- , counterClockSymmetric
    , findSymmetries
    , boardEquals
    ) where
import PegBoard
import CriticalPoints( rows, rowsZ, rowsP, rowsN
                     , BoolRow, brCoord
                     , boolRows, toBoolRows
                     , concentricTrianglesExclusive
                     , topRightRow, bottomRow, topLeftRow
                     , topLeftRowBR', bottomRowBR', topRightRowBR')
import Helpers.Lists(middleElem)
import Data.List(groupBy,sortBy,nubBy,nub,sort)

sortBoard :: Board -> Board
sortBoard (Board ps hs) = Board ps' hs' where
  ps' = sort ps
  hs' = sort hs

boardEquals :: Board -> Board -> Bool
boardEquals a b = sortBoard a == sortBoard b

boardSize :: Board -> Int
boardSize (Board ps hs) = x where
  (Coord x _) =  maximum $ ps ++ hs

combineBoard :: Board -> Board -> Board
combineBoard (Board p1 h1) (Board p2 h2) = Board ps hs where
  ps = nub $ p1 ++ p2
  hs = nub $ h1 ++ h2

swapCoords :: Coord -> (a,Bool) -> (Coord,Bool)
swapCoords c (_,p) = (c,p)

reverseBRs :: [BoolRow] -> Board
reverseBRs brs = Board ps hs where
  normalRows = map brCoord brs
  brFlip = map reverse brs
  newCoords = concat $ zipWith (zipWith swapCoords) normalRows brFlip
  ps = brCoord . filter snd $ newCoords
  hs = brCoord . filter (not . snd) $ newCoords

zedFlip :: Board -> Board
zedFlip = reverseBRs . rowsZ

posFlip :: Board -> Board
posFlip = reverseBRs . rowsN -- Pos direction has neg rows

negFlip :: Board -> Board
negFlip = reverseBRs . rowsP -- Neg direction has pos rows

-- Rotates the outer edges (sides) of a board
rotateRing :: Board -> Board
rotateRing x = Board ps' hs' where
  -- Change board into list of coord-bool for each side
  -- all of these read counter-clockwise except topRight, so we reverse it
  (trRow,btRow,tlRow) = (reverse $ topRightRowBR' x,bottomRowBR' x,topLeftRowBR' x)
  -- Extract the coords
  (trCoord,btCoord,tlCoord) = (map fst trRow,map fst btRow,map fst tlRow)
  -- Swap the coords of the sides (this assigns the "pegged" boolean to the next side)
  zipSwap = zipWith swapCoords
  (trRow',btRow',tlRow') = (zipSwap trCoord tlRow,zipSwap btCoord trRow, zipSwap tlCoord btRow)
  sameCoords a b = fst a == fst b
  -- Remove duplicates at corners
  allRows = nubBy sameCoords $ trRow' ++ btRow' ++ tlRow'
  -- Split the list into pegs and holes
  ps' = map fst . filter snd $ allRows
  hs' = map fst . filter (not . snd) $ allRows

-- rotate concentric triangles
clockRotate :: Board -> Board
clockRotate b = b' where
  rings = concentricTrianglesExclusive b
  rings' = map rotateRing rings
  b' = foldl1 combineBoard rings'

counterClockRotate :: Board -> Board
counterClockRotate = clockRotate . clockRotate

data Symmetries = Positive
                | Horizontal
                | Negative
                | Rotational
                | Not
                | All deriving (Eq,Show)

posSymmetric :: Board -> Symmetries
posSymmetric b =
  if b `boardEquals` posFlip b
  then Positive else Not

zedSymmetric :: Board -> Symmetries
zedSymmetric b =
  if b `boardEquals` zedFlip b
  then Horizontal else Not

negSymmetric :: Board -> Symmetries
negSymmetric b =
  if b `boardEquals` negFlip b
  then Negative else Not

rotSymmetric :: Board -> Symmetries
rotSymmetric b =
  if b `boardEquals` clockRotate b
  then Rotational else Not

findSymmetries :: Board -> Symmetries
findSymmetries b = syms' where
  syms = filter (/= Not)
    [posSymmetric b
    ,zedSymmetric b
    ,negSymmetric b
    ,rotSymmetric b]
  syms' = case syms of
    [] -> Not
    [x] -> x
    [Positive,Horizontal,Negative,Rotational] -> All
