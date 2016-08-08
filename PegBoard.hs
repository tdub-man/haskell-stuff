module PegBoard
    ( Coord
    , Board
    , makeBoard
    , playGame
    , showBoard
    ) where
import Data.List(partition,sortBy,intercalate)
import ListHelpers(nPerms,moveXTo,groupWithNs)

data Coord = Coord { _xCoord :: Int, _yCoord :: Int } deriving (Eq)
data Board = Board { _pegs :: [Coord], _holes :: [Coord] } deriving (Eq)

instance Show Coord where
  show (Coord x y) = show (x,y)
instance Show Board where
  show (Board ps hs) = "{ Pegs-" ++ show ps ++ " Holes-" ++ show hs ++ " }"

compareCoord :: Coord -> Coord -> Ordering
compareCoord (Coord x1 y1) (Coord x2 y2) = let
  xComp = x1 `compare` x2
  yComp = y1 `compare` y2
  in xComp `mappend` yComp

groupTri :: [a] -> [[a]]
groupTri as = groupWithNs as [1..]

showBoard :: Board -> String
showBoard (Board ps hs) = intercalate "\n" strs where
  ps' = zip ps $ repeat (1 :: Int)
  hs' = zip hs $ repeat (0 :: Int)
  compF (a,_) (b,_) = a `compareCoord` b
  allCoord = sortBy compF $ ps' ++ hs'
  allCoord' = map (map snd) $ groupTri allCoord
  acLength = length allCoord'
  spaces n = concat . replicate n $ " "
  spaceN = [acLength-1,acLength-2..1]
  strs = zipWith (\n cs -> spaces n ++ show cs) spaceN allCoord'

-- Pos[Left,Right] = Move along a line of positive slope
-- Zed[Left,Right] = Move along a line of zero slope
-- Neg[Left,Right] = Move along a line of negative slope
data BoardMoves = None
                | PosLeft | PosRight
                | ZedLeft | ZedRight
                | NegLeft | NegRight deriving (Eq,Enum,Show)

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
