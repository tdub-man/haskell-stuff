module PegBoardCriticalPoints
    ( rows, rows'
    , innerTriangle
    , concentricTriangles
    , concentricTrianglesExclusive
    -- , originShift
    -- , insideShift
    , critPoints
    , topLeftRow, bottomRow, topRightRow
    ) where
import PegBoard
import Data.List(groupBy,sort)
import Helpers.Lists(middle,compR)
import Helpers.Math(ceilDiv)

-- CRITICAL POINTS
-- Assuming full board
-- Minimal unique starting coords,
-- others can be found by roatating/flipping these

rows :: [Coord] -> [[Coord]]
rows cs = groupBy (\(Coord a _) (Coord b _) -> a == b) $ sort cs

rows' :: Board -> [[ (Coord,Bool) ]]
rows' (Board ps hs) = rPsHs' where
  rPsHs = rows $ ps ++ hs
  isPeg x = (x,x `elem` ps)
  rPsHs' = map (map isPeg) rPsHs

innerTriangle :: Board -> Board
innerTriangle b = Board ps hs where
  mids = concatMap middle . middle . rows' $ b
  ps = map fst . filter snd $ mids
  hs = map fst . filter (not . snd) $ mids
-- innerTriangle (Board ps hs) = Board ps' hs' where
--   mids = concatMap middle . middle . rows $ ps ++ hs
--   ps' = filter (`elem` ps) mids
--   hs' = filter (`elem` hs) mids

concentricTriangles' :: (Board,[Board]) -> (Board,[Board])
concentricTriangles' (Board [] [],bs) = (Board [] [],bs)
concentricTriangles' (b,bs) = concentricTriangles' (b',bs') where
  b' = innerTriangle b
  bs' = b:bs

concentricTriangles :: Board -> [Board]
concentricTriangles b = snd . concentricTriangles' $ (b,[])

concentricTrianglesExclusive :: Board -> [Board]
concentricTrianglesExclusive b = concs' where
  concs = concentricTriangles b
  remPsHs (Board p1 h1) (Board p2 h2) = Board ps hs where
    ps = filter (`notElem` p2) p1
    hs = filter (`notElem` h2) h1
  concs' = head concs:compR remPsHs concs

originShiftC :: [Coord] -> [Coord]
originShiftC = map (\(Coord x y) -> Coord (x-2) (y-1))

originShift :: Board -> Board
originShift (Board ps _) = Board ps' [] where
  ps' = originShiftC ps

insideShiftC :: [Coord] -> [Coord]
insideShiftC = map (\(Coord x y) -> Coord (x+2) (y+1))

insideShift :: Board -> Board
insideShift (Board ps _) = Board ps' [] where
  ps' = insideShiftC ps

-- Alter for pegs and holes
topLeftRow :: Board -> [Coord]
topLeftRow (Board [] []) = []
topLeftRow b = cs where
  rs = rows' b
  rs' = map head rs
  cs = map fst rs'
-- topLeftRow (Board ps@(Coord _ y:_) _) = filter (\(Coord _ y') -> y' == y) ps

bottomRow :: Board -> [Coord]
bottomRow (Board [] []) = []
bottomRow b = bottomR' where
  rs = rows' b
  bottomR = last rs
  bottomR' = map fst bottomR
-- bottomRow (Board ps hs) = last . rows $ ps ++ hs

topRightRow :: Board -> [Coord]
topRightRow (Board [] []) = []
topRightRow b = reverse trRow' where
  rs = rows' b
  trRow = map last rs
  trRow' = map fst trRow

critPoints :: Board -> [Coord]
critPoints b = cs where
  inners = reverse . concentricTriangles $ b
  tlRows = map topLeftRow inners
  lenTLRows = map length tlRows
  ns = map (`ceilDiv` 2) lenTLRows
  cs = concat [ take n tlr | (tlr,n) <- zip tlRows ns ]
