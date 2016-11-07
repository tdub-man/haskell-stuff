module Symmetries
    ( zedFlip, posFlip, negFlip
    , clockRotate, counterClockRotate
    , Symmetries(Positive,Horizontal,Negative,Rotational,All,Not)
    , posSymmetric, zedSymmetric, negSymmetric, rotSymmetric
    , findSymmetries
    , boardEquals
    ) where
import PegBoard
import CriticalPoints( rows, rowsZ, rowsP, rowsN
                     , BoolRow, brCoord)
import Data.List(nub,sort)

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

clockRotate :: Board -> Board
clockRotate b = Board ps hs where
  (rp,rz,rn) = (rowsP b,rowsZ b,rowsN b) -- BoolRows stuff ([BoolRow],[BoolRow],[BoolRow])
  zipSwap = zipWith (zipWith swapCoords)
  (cp,cz,cn) = (map brCoord rp,map brCoord rz,map brCoord rn) -- Coord stuff, ([[Coord]],[[Coord]],[[Coord]])
  (np,nz,nn) = (zipSwap cp rz,zipSwap cz rn,zipSwap cn rp) -- New BoolRows, ([BoolRow],[BoolRow],[BoolRow])
  mapTail = map tail
  newCoords = concat $ mapTail np ++ mapTail nz ++ mapTail nn -- Removes duplicate corners
  ps = map fst . filter snd $ newCoords
  hs = map fst . filter (not . snd) $ newCoords

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
