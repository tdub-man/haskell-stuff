module PegBoard
    ( Coord(Coord)
    , Board(Board)
    -- , compareCoord
    , makeBoard
    , removePeg
    , addPeg
    , pegCount
    , showBoard
    ) where
import Data.List(partition,sortBy,intercalate)
import Helpers.Lists(moveXTo,groupWithNs,takeThrough)

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

-- xs GT && ys GT == GT : Greater zed row
-- xs GT && ys EQ == GT : Greater zed row
-- xs GT && ys LT == GT : Greater zed row
-- xs EQ && ys GT == GT : Same zed row, greater
-- xs EQ && ys EQ == EQ : Same coord
-- xs EQ && ys LT == LT : Same zed row, lesser
-- xs LT && ys GT == LT : Lesser zed row
-- xs LT && ys EQ == LT : Lesser zed row
-- xs LT && ys LT == LT : Lesser zed row
-- Compares in the zed rows
compareZ :: Coord -> Coord -> Ordering
compareZ = compare

-- xs GT && ys GT == LT : Lesser pos row
-- xs EQ && ys GT == LT : Lesser pos row
-- xs LT && ys GT == LT : Lesser pos row
-- xs GT && ys EQ == GT : Same pos row, greater
-- xs EQ && ys EQ == EQ : Same coord
-- xs LT && ys EQ == LT : Same pos row, lesser
-- xs GT && ys LT == GT : Greater pos row
-- xs EQ && ys LT == GT : Greater pos row
-- xs LT && ys LT == GT : Greater pos row
-- Compares in the pos rows
compareP :: Coord -> Coord -> Ordering
compareP (Coord x1 y1) (Coord x2 y2) = let
  xComp = x1 `compare` x2
  yComp = y1 `compare` y2
  in case (yComp,xComp) of
    (GT,_) -> LT
    (EQ,x) -> x
    (LT,_) -> GT

-- xs GT && ys GT == LT : Same neg row, lesser
-- xs GT && ys EQ == LT : Lesser neg row
-- xs GT && ys LT == LT : Lesser neg row
-- xs EQ && ys GT == GT : Greater neg row
-- xs EQ && ys EQ == EQ : Same coord
-- xs EQ && ys LT == LT : Lesser neg row
-- xs LT && ys GT == GT : Greater neg row
-- xs LT && ys EQ == GT : Greater neg row
-- xs LT && ys LT == GT : Same neg row, greater
-- Compares in the neg rows
compareN :: Coord -> Coord -> Ordering
compareN (Coord x1 y1) (Coord x2 y2) = let
  xComp = x1 `compare` x2
  yComp = y1 `compare` y2
  in case (xComp,yComp) of
    (GT,_) -> LT
    (EQ,x) -> x
    (LT,_) -> GT

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
