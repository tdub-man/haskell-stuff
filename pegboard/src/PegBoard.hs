module PegBoard
    ( Coord(Coord)
    , Board(Board)
    -- , compareCoord
    , makeBoard
    , removePeg
    , pegCount
    , showBoard

    , BoardMoves
    , nextMoves
    , movePegs
    , movePegsAll
    ) where
import Data.List(partition,sortBy,intercalate)
import Helpers.Lists(nPerms,moveXTo,groupWithNs,takeThrough)

data Coord = Coord { _xCoord :: Int, _yCoord :: Int } deriving (Eq)
data Board = Board { _pegs :: [Coord], _holes :: [Coord] } deriving (Eq)

instance Show Coord where
  show (Coord x y) = show (x,y)
instance Show Board where
  show (Board ps hs) = "{ Pegs-" ++ show ps ++ " Holes-" ++ show hs ++ " }"
instance Read Coord where
  readsPrec _ input = [(c,rest)] where
    (tup,rest) = takeThrough (/=')') input
    (x,y) = read tup :: (Int,Int)
    c = Coord x y
instance Ord Coord where
  (Coord x1 y1) `compare` (Coord x2 y2) = let
    xComp = x1 `compare` x2
    yComp = y1 `compare` y2
    in xComp `mappend` yComp

-- compareCoord :: Coord -> Coord -> Ordering
-- compareCoord (Coord x1 y1) (Coord x2 y2) = let
--   xComp = x1 `compare` x2
--   yComp = y1 `compare` y2
--   in xComp `mappend` yComp

groupTri :: [a] -> [[a]]
groupTri as = groupWithNs as [1..]

showBoard :: Board -> String
showBoard (Board ps hs) = intercalate "\n" strs where
  ps' = zip ps $ repeat (1 :: Int)
  hs' = zip hs $ repeat (0 :: Int)
  compF (a,_) (b,_) = a `compare` b
  allCoord = sortBy compF $ ps' ++ hs'
  allCoord' = map (map snd) $ groupTri allCoord
  acLength = length allCoord'
  spaces n = concat . replicate n $ " "
  spaceN = [acLength-1,acLength-2..1]
  lastLine = show . last $ allCoord'
  strs = (++ [lastLine]) . zipWith (\n cs -> spaces n ++ show cs) spaceN $ allCoord'

-- Pos = Move along a line of positive slope
-- Zed = Move along a line of zero slope
-- Neg = Move along a line of negative slope
data BoardMoves = None | Pos | Zed | Neg deriving (Eq,Enum,Show)

bmAnd :: BoardMoves -> BoardMoves -> BoardMoves
bmAnd a b = if a == b then a else None

makeBoard :: Int -> Board
makeBoard n = Board ps [] where
  ps = [ Coord x y | x <- [1..n], y <- [1..x] ]

-- Use to open board
removePeg :: Coord -> Board -> Board
removePeg c (Board ps hs) = Board ps' hs' where
  (ps',hs') = moveXTo c (ps,hs)

addPeg :: Coord -> Board -> Board
addPeg c (Board ps hs) = Board ps' hs' where
  (hs',ps') = moveXTo c (hs,ps)

pegCount :: Board -> Int
pegCount = length . _pegs

-- Relation between a and b
-- They are related in the pos direction if they are one row apart and in the same column
-- They are related in the zed direction if they are in the same row and one column apart
-- They are related in the neg direction if they are one row and one column apart
-- Having a negative value for the relation represents
-- coord1 being one row or column higher than coord2
neighbor :: Coord -> Coord -> BoardMoves
neighbor (Coord x1 y1) (Coord x2 y2) = let
  in case (x2-x1,y2-y1) of
    ( 1, 0) -> Pos
    (-1, 0) -> Pos
    ( 0, 1) -> Zed
    ( 0,-1) -> Zed
    ( 1, 1) -> Neg
    (-1,-1) -> Neg
    _       -> None

validMove :: (Coord,Coord,Coord) -> BoardMoves
validMove (a,b,c) = ab `bmAnd` bc where
  ab = neighbor a b
  bc = neighbor b c

-- Creates a 3-tuple of two pegs and one hole
-- Filters these tuples by whether they are valid moves
-- First two coords are some permutation of the pegs
-- The 3-tuple is a cartesian product of the previous permutations and the holes
-- Filters the 3-tuples by whether they are valid moves
nextMoves :: Board -> [(Coord,Coord,Coord)]
nextMoves (Board ps hs) = trips' where
  pPerm = 2 `nPerms` ps
  trips = [ (a,b,c) | [a,b] <- pPerm, c <- hs ]
  trips' = filter (\tri -> validMove tri /= None) trips

-- Takes the 3-tuple of two pegs and one hole
-- Removes the pegs and adds the hole
movePegs :: (Coord,Coord,Coord) -> Board -> Board
movePegs (a,b,c) = removePeg a . removePeg b . addPeg c

movePegsAll :: [(Coord,Coord,Coord)] -> Board -> [Board]
movePegsAll mvs b = [ movePegs mv b | mv <- mvs ]

-- In these definitions of nextMoves and movePegs,
-- the move being made and what coordinates are pegs or holes
-- are not explicitly specified
-- We know from the order the triples are made that the
-- first two coordinates are pegs and the remaining is a hole
-- When filtering for valid moves, what don't keep the move that
-- the triple represents, it is only implied by the data.
-- Therefore, in movePegs, we know that
-- "removePeg a . removePeg b . addPeg c" is a valid move, though
-- a, b, and c aren't explicitly said to be pegs or holes, and their
-- combination isn't explicitly stated as a certain move
