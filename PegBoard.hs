module PegBoard
    ( Coord
    , Peg
    , triangleNum
    , pegCount
    , openBoard
    , boardList
    , boardLists
    ) where
import Data.List(groupBy)

type Coord = (Int, Int)
type Peg = (Coord, Bool)
type Board = [[Peg]]

pegCount :: Board -> Int
pegCount = triangleNum . length

triangleNum :: Int -> Int
triangleNum n = n * (n+1) `div` 2

-- function to randomly fill a Board with pegCount-1 pegs
openBoard :: Coord -> Board -> Board
openBoard (row,col) b = let
  pegMatch ((r,c),_) = if r == row && c == col then ((r,c),False) else ((r,c),True)
  in map (map pegMatch) b

boardList :: Int -> [Peg]
boardList n = [ ((x,y),False) | x <- [1..n], y <- [1..x] ]

boardLists :: Int -> Board
boardLists n = groupBy sameRow $ boardList n where
  sameRow ((a,_),_) ((b,_),_) = a == b
