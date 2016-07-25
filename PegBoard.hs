module PegBoard
    ( Coord
    , Peg
    , triangleNum
    , pegCount
    , openBoard
    , boardList
    , boardLists
    , neighbor
    , validMove
    ) where
import Data.List(groupBy)

data Coord = Coord { xCoord :: Int, yCoord :: Int } deriving (Eq)
data Peg = Peg { coord :: Coord, pegged :: Bool } deriving (Eq)

instance Show Coord where
  show (Coord x y) = show (x,y)
instance Show Peg where
  show (Peg c b) = show c ++ "_" ++ show b

-- Pos[Left,Right] = Move along a line of positive slope
-- Zed[Left,Right] = Move along a line of zero slope
-- Neg[Left,Right] = Move along a line of negative slope
data BoardMoves = None
                | PosLeft | PosRight
                | ZedLeft | ZedRight
                | NegLeft | NegRight deriving (Eq,Enum,Show)
-- type Peg = (Coord, Bool)
type PegTriple = (Peg,Peg,Peg)
type Board = [[Peg]]

triangleNum :: Int -> Int
triangleNum n = n * (n+1) `div` 2

pegCount :: Board -> Int
pegCount = triangleNum . length

pegX :: Peg -> Int
pegX = xCoord . coord

pegY :: Peg -> Int
pegY = yCoord . coord

-- function to randomly fill a Board with pegCount-1 pegs
openBoard :: Coord -> Board -> Board
openBoard (Coord x y) b = let
  pegMatch (Peg c _) = if xCoord c == x && yCoord c == y
    then Peg c False
    else Peg c True
  in map (map pegMatch) b

boardList :: Int -> [Peg]
boardList n = [ Peg (Coord x y) False | x <- [1..n], y <- [1..x] ]

boardLists :: Int -> Board
boardLists n = groupBy sameRow $ boardList n where
  sameRow p1 p2 = pegX p1 == pegX p2

togglePeg :: Peg -> Peg
togglePeg (Peg c b) = Peg c $ not b

-- b's relation to a
neighbor :: Peg -> Peg -> BoardMoves
neighbor a b = let
  compX = pegX b - pegX a
  compY = pegY b - pegY a
  in case (compX,compY) of
    (-1,0)  -> PosRight
    (1,0)   -> PosLeft
    (-1,-1) -> NegLeft
    (1,1)   -> NegRight
    (0,-1)  -> ZedLeft
    (0,1)   -> ZedRight
    _       -> None

validMove :: PegTriple -> BoardMoves
validMove (Peg _ False,_,_) = None
validMove (_,Peg _ False,_) = None
validMove (_,_,Peg _ True)  = None
validMove (a,b,c) = let
  neighAB = neighbor a b
  neighBC = neighbor b c
  in if neighAB == neighBC
    then neighAB
    else None
