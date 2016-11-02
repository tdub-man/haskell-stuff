module CriticalPoints
    ( rows
    , toBoolRow, boolRows, toBoolRows
    , innerTriangle
    , concentricTriangles
    , concentricTrianglesExclusive
    , topLeftRow, bottomRow, topRightRow
    , topLeftRowBR, bottomRowBR, topRightRowBR
    , topLeftRowBR', bottomRowBR', topRightRowBR'
    , critPoints
    ) where
import PegBoard
import Data.List(sortBy,groupBy,sort)
import Helpers.Lists(middle,compR)
import Helpers.Math(ceilDiv)

-- CRITICAL POINTS
-- Minimal unique starting coords,
-- others can be found by roatating/flipping these

type BoolRow = [(Coord,Bool)]

rows :: [Coord] -> [[Coord]]
rows cs = groupBy (\(Coord a _) (Coord b _) -> a == b) $ sort cs

toBoolRow :: Board -> BoolRow
toBoolRow (Board ps hs) = pBool ++ hBool where
  pBool = map (\x -> (x,True )) ps
  hBool = map (\x -> (x,False)) hs

boolRows :: BoolRow -> [BoolRow]
boolRows = groupBy (\(Coord a _,_) (Coord b _,_) -> a == b)
         . sortBy (\(c1,_) (c2,_) -> compare c1 c2)

toBoolRows :: Board -> [BoolRow]
toBoolRows = boolRows . toBoolRow

innerTriangle :: Board -> Board
innerTriangle b = Board ps hs where
  mids = concatMap middle . middle . toBoolRows $ b
  ps = map fst . filter snd $ mids
  hs = map fst . filter (not . snd) $ mids

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

topLeftRow :: Board -> [Coord]
topLeftRow (Board [] []) = []
topLeftRow b = cs where
  rs = toBoolRows b
  rs' = map head rs
  cs = map fst rs'

topLeftRowBR :: BoolRow -> BoolRow
topLeftRowBR br = tlBr where
  brs = boolRows br
  tlBr = map head brs

topLeftRowBR' :: Board -> BoolRow
topLeftRowBR' = topLeftRowBR . toBoolRow

bottomRow :: Board -> [Coord]
bottomRow (Board [] []) = []
bottomRow b = bottomR' where
  rs = toBoolRows b
  bottomR = last rs
  bottomR' = map fst bottomR

bottomRowBR :: BoolRow -> BoolRow
bottomRowBR br = bBr where
  brs = boolRows br
  bBr = last brs

bottomRowBR' :: Board -> BoolRow
bottomRowBR' = bottomRowBR . toBoolRow

topRightRow :: Board -> [Coord]
topRightRow (Board [] []) = []
topRightRow b = reverse trRow' where
  rs = toBoolRows b
  trRow = map last rs
  trRow' = map fst trRow

topRightRowBR :: BoolRow -> BoolRow
topRightRowBR br = trBr where
  brs = boolRows br
  trBr = map last brs

topRightRowBR' :: Board -> BoolRow
topRightRowBR' = topRightRowBR . toBoolRow

critPoints :: Board -> [Coord]
critPoints b = cs where
  inners = reverse . concentricTriangles $ b
  tlRows = map topLeftRow inners
  lenTLRows = map length tlRows
  ns = map (`ceilDiv` 2) lenTLRows
  cs = concat [ take n tlr | (tlr,n) <- zip tlRows ns ]
