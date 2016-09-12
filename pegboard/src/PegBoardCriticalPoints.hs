module PegBoardCriticalPoints
    ( rows
    , concentricTriangles
    , concentricTrianglesExclusive
    -- , originShift
    -- , insideShift
    , critPoints
    ) where
import PegBoard
import Data.List(groupBy,sort)
import Helpers.Lists(middle)
import Helpers.Math(ceilDiv)

-- CRITICAL POINTS
-- Assuming full board
-- Minimal unique starting coords,
-- others can be found by roatating/flipping these

rows :: [Coord] -> [[Coord]]
rows cs = groupBy (\(Coord a _) (Coord b _) -> a == b) $ sort cs

innerTriangle :: Board -> Board
innerTriangle (Board ps hs) = Board ps' hs' where
  mids = concatMap middle . middle . rows $ ps ++ hs
  ps' = filter (`elem` ps) mids
  hs' = filter (`elem` hs) mids

concentricTriangles' :: (Board,[Board]) -> (Board,[Board])
concentricTriangles' (Board [] _,bs) = (Board [] [],bs)
concentricTriangles' (b,bs) = concentricTriangles' (b',bs') where
  b' = innerTriangle b
  bs' = b:bs

concentricTriangles :: Board -> [Board]
concentricTriangles b = snd . concentricTriangles' $ (b,[])

concentricTrianglesExclusive :: Board -> [Board]
concentricTrianglesExclusive b = concs' where
  concs = concentricTriangles b
  remPsHs (Board p1 h1) (Board p2 h2) = Board ps hs where
    ps = filter (`notElem` p1) p2
    hs = filter (`notElem` h1) h2
  concs' = [ exclude n | n <- [0..length concs - 1] ]
  exclude 0 = head concs
  exclude n = remPsHs (concs !! (n-1)) (concs !! n)

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

topLeftRow :: Board -> [Coord]
topLeftRow (Board [] _) = []
topLeftRow (Board ps@(Coord _ y:_) _) = filter (\(Coord _ y') -> y' == y) ps

critPoints :: Board -> [Coord]
critPoints b = cs where
  inners = reverse . concentricTriangles $ b
  tlRows = map topLeftRow inners
  lenTLRows = map length tlRows
  ns = map (`ceilDiv` 2) lenTLRows
  cs = concat [ take n tlr | (tlr,n) <- zip tlRows ns ]
