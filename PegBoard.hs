module PegBoard
    ( Coord
    , Peg
    , buildBoard
    , triangleNum
    , pegCount
    , nth
    , openBoard
    ) where
import Data.List

type Coord = (Int, Int)
type Peg = (Coord, Bool)
type Board = [[Peg]]

buildBoard :: Int -> Board
buildBoard 0 = [[((0,0),False)]]
buildBoard n = let
  mList = [ (a,b) | a <- [0..n-1], b <- [0..n-1] ]
  matrix = groupBy (\(a,_) (b,_) -> a == b) mList
  triangle = map (filter (uncurry (>=))) matrix
  in map (map (\x -> (x,True))) triangle

pegCount :: Board -> Int
pegCount = triangleNum . length

triangleNum :: Int -> Int
triangleNum n = n * (n+1) `div` 2

nth :: [a] -> Int -> Maybe a
nth [] _ = Nothing
nth (x:_) 0 = Just x
nth (_:xs) n = nth xs (n - 1)

-- function to randomly fill a Board with pegCount-1 pegs

openBoard :: Coord -> Board -> Board
openBoard (row,col) b = let
  pegMatch ((r,c),_) = if r == row && c == col then ((r,c),False) else ((r,c),True)
  in map (map pegMatch) b

-- (0,0)
-- (1,0) (1,1)
-- (2,0) (2,1) (2,2)
-- (3,0) (3,1) (3,2) (3,3)
