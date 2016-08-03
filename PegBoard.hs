module PegBoard
    ( Coord
    , Peg
    -- , triangleNum
    , concatZip
    , options
    , boardList
    , openBoard
    , neighbor
    , validMove
    , nextMoves
    , movePegs
    , movePegsChain
    , play
    ) where
import Data.List(partition,break)

data Coord = Coord { xCoord :: Int, yCoord :: Int } deriving (Eq)
data Peg = Peg { coord :: Coord, pegged :: Bool } deriving (Eq)
data Board = Board { width :: Int, pegs :: [Peg] } deriving (Eq)

instance Show Coord where
  show (Coord x y) = show (x,y)
instance Show Peg where
  show (Peg c b) = show c ++ "_" ++ show b
instance Show Board where
  show (Board s ps) = "B-" ++ show s ++ ":" ++ show ps

-- Pos[Left,Right] = Move along a line of positive slope
-- Zed[Left,Right] = Move along a line of zero slope
-- Neg[Left,Right] = Move along a line of negative slope
data BoardMoves = None
                | PosLeft | PosRight
                | ZedLeft | ZedRight
                | NegLeft | NegRight deriving (Eq,Enum,Show)
type PegTriple = (Peg,Peg,Peg)

-- triangleNum :: Int -> Int
-- triangleNum n = n * (n+1) `div` 2

pegX :: Peg -> Int
pegX = xCoord . coord

pegY :: Peg -> Int
pegY = yCoord . coord

concatZip :: [a] -> [a] -> [a]
concatZip a = concat . zipWith (\x y -> [x,y]) a

options :: [a] -> [a] -> [a] -> [(a,a,a)]
options a b c = [ (x,y,z) | x <- a, y <- b, z <- c ]

options' :: [a] -> [(a,a,a)]
options' a = options a a a

boardList :: Int -> [Peg]
boardList n = [ Peg (Coord x y) False | x <- [1..n], y <- [1..x] ]

boardList' :: Int -> Board
boardList' n = Board n ps where
  ps = [ Peg (Coord x y) True | x <- [1..n], y <- [1..x] ]

openBoard :: Coord -> [Peg] -> [Peg]
openBoard (Coord x y) ps = let
  pegMatch (Peg c _) = if xCoord c == x && yCoord c == y
    then Peg c False
    else Peg c True
  in map pegMatch ps

removePeg' :: Coord -> Board -> Board
removePeg' c (Board n ps) = Board n ps' where
  (pre,rest) = break (\(Peg pc _) -> pc == c) ps
  ps' = case rest of
    [] -> ps
    _:xs -> pre ++ xs

addPeg :: Coord -> Board -> Board
addPeg c (Board n ps) = Board n $ Peg c True:ps

onBoard :: Int -> Peg -> Bool
onBoard n (Peg (Coord x y) _) = pos && inWidth where
  pos = x > 0 && y > 0
  inWidth = x <= n && y <= n

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

makeNeighbor :: Peg -> BoardMoves -> Maybe Peg
makeNeighbor _ None     = Nothing
makeNeighbor p PosRight = Just $ Peg (Coord (pegX p - 1) (pegY p)) True
makeNeighbor p PosLeft  = Just $ Peg (Coord (pegX p + 1) (pegY p)) True
makeNeighbor p NegLeft  = Just $ Peg (Coord (pegX p - 1) (pegY p - 1)) True
makeNeighbor p NegRight = Just $ Peg (Coord (pegX p + 1) (pegY p + 1)) True
makeNeighbor p ZedLeft  = Just $ Peg (Coord (pegX p) (pegY p - 1)) True
makeNeighbor p ZedRight = Just $ Peg (Coord (pegX p) (pegY p + 1)) True

findThirdNeighbor :: Int -> Peg -> Peg -> Maybe Peg
findThirdNeighbor s a b = third where
  abRel = neighbor a b
  third = case makeNeighbor b abRel of
    Nothing -> Nothing
    Just p -> if onBoard s p then Just p else Nothing

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

nextMoves :: [Peg] -> [(Peg,Peg,Peg)]
nextMoves ps = ops' where
  ops = options' ps
  validOps = map validMove ops
  ops' = map fst . filter (\(_,mv) -> mv /= None) $ zip ops validOps

elem3Tup :: (Eq a) => a -> (a,a,a) -> Bool
elem3Tup x (a,b,c)
    | x == a = True
    | x == b = True
    | x == c = True
    | otherwise = False

movePegs :: [Peg] -> [[Peg]]
movePegs ps = ps' where
  nMoves = nextMoves ps
  ps' = [ map (\p -> if p `elem3Tup` t then togglePeg p else p) ps | t <- nMoves ]
-- TODO : Implement logging

movePegsChain :: [[Peg]] -> [[Peg]]
movePegsChain lps = let
  nlps = concatMap movePegs lps
  in case nlps of
    [] -> lps
    _ -> movePegsChain nlps

movePegs' :: [Peg] -> [[Peg]]
movePegs' ps = ps' where
  nMoves = nextMoves ps
  ps' = case nMoves of
    [] -> [ps]
    _  -> [ map (\p -> if p `elem3Tup` t then togglePeg p else p) ps | t <- nMoves ]

movePegsChain' :: [[Peg]] -> [[Peg]]
movePegsChain' lps = let
  nlps = concatMap movePegs' lps
  (same,new) = partition (`elem` lps) nlps
  in case nlps of
    [] -> lps
    _ -> movePegsChain' nlps

play :: [Peg] -> [[Peg]]
play ps = movePegsChain [ps]
