module PegBoard
    ( Coord
    , Peg
    , BoardMoves
    , concatZip
    , combinations
    , nPerms
    -- , removePeg
    , makeBoard
    , neighbor
    -- , validMove
    , nextMoves
    , movePegs
    , movePegsAll
    , play
    , playGame
    ) where
import Data.List(nub,tails,find,delete,partition)
import Control.Monad(replicateM)

data Coord = Coord { xCoord :: Int, yCoord :: Int } deriving (Eq)
data Peg = Peg { coord :: Coord, pegged :: Bool } deriving (Eq)
data Board = Board { pegs :: [Coord], holes :: [Coord] } deriving (Eq)

instance Show Coord where
  show (Coord x y) = show (x,y)
instance Show Peg where
  show (Peg c b) = show c ++ "_" ++ show b
instance Show Board where
  show (Board ps hs) = "{ Pegs-" ++ show ps ++ " Holes-" ++ show hs ++ " }"

-- Pos[Left,Right] = Move along a line of positive slope
-- Zed[Left,Right] = Move along a line of zero slope
-- Neg[Left,Right] = Move along a line of negative slope
data BoardMoves = None
                | PosLeft | PosRight
                | ZedLeft | ZedRight
                | NegLeft | NegRight deriving (Eq,Enum,Show)
type PegTriple = (Peg,Peg,Peg)

bmAnd :: BoardMoves -> BoardMoves -> BoardMoves
bmAnd a b = if a == b then a else None

pegX :: Peg -> Int
pegX = xCoord . coord

pegY :: Peg -> Int
pegY = yCoord . coord

concatZip :: [a] -> [a] -> [a]
concatZip a = concat . zipWith (\x y -> [x,y]) a

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = do
  (x:xs') <- tails xs
  rest <- combinations (n-1) xs'
  return $ x:rest

nPerms :: (Eq a) => Int -> [a] -> [[a]]
nPerms n = filter ((==n).length.nub) . replicateM n

-- remove :: (Eq a) => a -> [a] -> (Maybe a,[a])
-- remove x xs = case find (==x) xs of
--   Nothing -> (Nothing, xs)
--   Just _ -> (Just x, delete x xs)

moveXTo :: (Eq a) => a -> ([a],[a]) -> ([a],[a])
moveXTo x (as,bs) = case find (==x) as of
  Nothing -> (as,bs)
  Just _ -> (delete x as,x:bs)

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

-- b's relation to a
neighbor :: Coord -> Coord -> BoardMoves
neighbor (Coord x1 y1) (Coord x2 y2) = case (x2-x1, y2-y1) of
    (-1,0)  -> PosRight
    (1,0)   -> PosLeft
    (-1,-1) -> NegLeft
    (1,1)   -> NegRight
    (0,-1)  -> ZedLeft
    (0,1)   -> ZedRight
    _       -> None

validMove :: (Coord,Coord,Coord) -> BoardMoves
validMove (a,b,c) = ab `bmAnd` bc where
  ab = neighbor a b
  bc = neighbor b c

nextMoves :: Board -> [(Coord,Coord,Coord)]
nextMoves (Board ps hs) = trips' where
  pPerm = 2 `nPerms` ps
  trips = [ (a,b,c) | [a,b] <- pPerm, c <- hs ]
  trips' = filter (\tri -> validMove tri /= None) trips

movePegs :: (Coord,Coord,Coord) -> Board -> Board
movePegs (a,b,c) = removePeg a . removePeg b . addPeg c

movePegsAll :: [(Coord,Coord,Coord)] -> Board -> [Board]
movePegsAll mvs b = [ movePegs mv b | mv <- mvs ]
-- TODO : Implement logging

play :: ([Board],[Board]) -> ([Board],[Board])
play ([],ended) = ([],ended)
play (playing,ended) = play (playing',ended') where
  boardAndMoves = zip playing $ map nextMoves playing
  (newlyEnded,playable) = partition (\(_,m) -> null m) boardAndMoves
  newlyEnded' = map fst newlyEnded
  ended' = ended ++ newlyEnded'
  playing' = concat [ movePegsAll mvs b | (b,mvs) <- playable ]

playGame :: Board -> [Board]
playGame b = endStates where
  (_,endStates) = play ([b],[])
