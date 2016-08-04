module PegBoard
    ( Coord
    , Peg
    , concatZip
    , combinations
    , nPerms
    , removePeg
    , makeBoard
    , neighbor
    -- , validMove
    , nextMoves
    -- , movePegs
    -- , movePegsChain
    -- , play
    ) where
import Data.List(nub,partition,break,tails,permutations)
import Control.Monad(replicateM)

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

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = do
  (x:xs') <- tails xs
  rest <- combinations (n-1) xs'
  return $ x:rest

nPerms :: (Eq a) => Int -> [a] -> [[a]]
nPerms n = filter ((==n).length) . map nub . replicateM n

makeBoard :: Int -> Board
makeBoard n = Board n ps where
  ps = [ Peg (Coord x y) True | x <- [1..n], y <- [1..x] ]

-- Use to open board
removePeg :: Coord -> Board -> Board
removePeg c (Board n ps) = Board n ps' where
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

-- togglePeg :: Peg -> Peg
-- togglePeg (Peg c b) = Peg c $ not b

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

iterCoord :: Coord -> BoardMoves -> Maybe Coord
iterCoord _ None = Nothing
iterCoord (Coord x y) PosRight = Just $ Coord (x-1) y
iterCoord (Coord x y) PosLeft  = Just $ Coord (x+1) y
iterCoord (Coord x y) NegLeft  = Just $ Coord (x-1) (y-1)
iterCoord (Coord x y) NegRight = Just $ Coord (x+1) (y+1)
iterCoord (Coord x y) ZedLeft  = Just $ Coord x (y-1)
iterCoord (Coord x y) ZedRight = Just $ Coord x (y+1)

makeNeighbor :: Peg -> BoardMoves -> Maybe Peg
makeNeighbor _ None       = Nothing
makeNeighbor (Peg c _) mv = Just $ Peg c' True where
  (Just c') = iterCoord c mv

findThirdNeighbor :: Int -> Peg -> Peg -> Maybe Peg
findThirdNeighbor s a b = third where
  abRel = neighbor a b
  third = case makeNeighbor b abRel of
    Nothing -> Nothing
    Just p -> if onBoard s p then Just p else Nothing

nextMoves :: Board -> [[Peg]]
nextMoves (Board w ps) = let
  pPerm = 2 `nPerms` ps
  triple [a,b] = case findThirdNeighbor w a b of
    Nothing -> Nothing
    Just c -> Just [a,b,c]
  pTrips = filter (/= Nothing) . map triple $ pPerm
  in [ x | (Just x) <- pTrips ]

-- nextMoves :: [Peg] -> [(Peg,Peg,Peg)]
-- nextMoves ps = ops' where
--   ops = options' ps
--   validOps = map validMove ops
--   ops' = map fst . filter (\(_,mv) -> mv /= None) $ zip ops validOps

-- elem3Tup :: (Eq a) => a -> (a,a,a) -> Bool
-- elem3Tup x (a,b,c)
--     | x == a = True
--     | x == b = True
--     | x == c = True
--     | otherwise = False

-- movePegs :: [Peg] -> [[Peg]]
-- movePegs ps = ps' where
--   nMoves = nextMoves ps
--   ps' = [ map (\p -> if p `elem3Tup` t then togglePeg p else p) ps | t <- nMoves ]
-- -- TODO : Implement logging
--
-- movePegsChain :: [[Peg]] -> [[Peg]]
-- movePegsChain lps = let
--   nlps = concatMap movePegs lps
--   in case nlps of
--     [] -> lps
--     _ -> movePegsChain nlps
--
-- movePegs' :: [Peg] -> [[Peg]]
-- movePegs' ps = ps' where
--   nMoves = nextMoves ps
--   ps' = case nMoves of
--     [] -> [ps]
--     _  -> [ map (\p -> if p `elem3Tup` t then togglePeg p else p) ps | t <- nMoves ]
--
-- movePegsChain' :: [[Peg]] -> [[Peg]]
-- movePegsChain' lps = let
--   nlps = concatMap movePegs' lps
--   (same,new) = partition (`elem` lps) nlps
--   in case nlps of
--     [] -> lps
--     _ -> movePegsChain' nlps
--
-- play :: [Peg] -> [[Peg]]
-- play ps = movePegsChain [ps]
