module Matrix
    ( Matrix
    , longestShow
    , singleCell
    , addNorth, addEast, addSouth, addWest
    , maxLocalNorth, maxLocalEast, maxLocalSouth, maxLocalWest
    , levelsLocalNorth, levelsLocalEast, levelsLocalSouth, levelsLocalWest
    , getNorth, getEast, getSouth, getWest
    ) where
import Control.Applicative

data Matrix a = Border | Cell { value :: a
                              , north :: Matrix a
                              , east :: Matrix a
                              , south :: Matrix a
                              , west :: Matrix a
                              } deriving (Eq)

instance (Show a) => Show (Matrix a) where
  show Border = "\t"
  show x = "\t"          ++ show (north x) ++                  "\n" ++
           show (west x) ++ show (value x) ++ show (east x) ++ "\n" ++
           "\t"          ++ show (south x)

instance Functor Matrix where
  fmap _ Border = Border
  fmap f (Cell a n e s w) = Cell a' n' e' s' w' where
    a' = f a
    n' = fmap f n
    e' = fmap f e
    s' = fmap f s
    w' = fmap f w

instance Applicative Matrix where
  pure = singleCell
  Border <*> _ = Border
  (Cell f n e s w) <*> x = Cell a' n' e' s' w' where
    a' = value . fmap f $ x
    n' = n <*> x
    e' = e <*> x
    s' = s <*> x
    w' = w <*> x

longestShow :: (Show a) => Matrix a -> Int
longestShow Border = 0
longestShow (Cell a n e s w) = maximum [a', n', e', s', w'] where
  a' = length . show $ a
  n' = longestShow n
  e' = longestShow e
  s' = longestShow s
  w' = longestShow w

singleCell :: a -> Matrix a
singleCell x = Cell x Border Border Border Border

addNorth :: Matrix a -> Matrix a -> Matrix a
addNorth (Cell a _ e s w) n = Cell a n e s w

addEast :: Matrix a -> Matrix a -> Matrix a
addEast (Cell a n _ s w) e = Cell a n e s w

addSouth :: Matrix a -> Matrix a -> Matrix a
addSouth (Cell a n e _ w) s = Cell a n e s w

addWest :: Matrix a -> Matrix a -> Matrix a
addWest (Cell a n e s _) w = Cell a n e s w

maxLocalNorth :: Matrix a -> Matrix a
maxLocalNorth Border = Border
maxLocalNorth (Cell a Border e s w) = Cell a Border e s w
maxLocalNorth (Cell _ n _ _ _) = maxLocalNorth n

maxLocalEast :: Matrix a -> Matrix a
maxLocalEast Border = Border
maxLocalEast (Cell a n Border s w) = Cell a n Border s w
maxLocalEast (Cell _ _ e _ _) = maxLocalEast e

maxLocalSouth :: Matrix a -> Matrix a
maxLocalSouth Border = Border
maxLocalSouth (Cell a n e Border w) = Cell a n e Border w
maxLocalSouth (Cell _ _ _ s _) = maxLocalSouth s

maxLocalWest :: Matrix a -> Matrix a
maxLocalWest Border = Border
maxLocalWest (Cell a n e s Border) = Cell a n e s Border
maxLocalWest (Cell _ _ _ _ w) = maxLocalWest w

-- How many levels are directly above this cell
levelsLocalNorth :: Matrix a -> Int
levelsLocalNorth Border = 0
levelsLocalNorth x = 1 + rest where
  rest = levelsLocalNorth $ north x

levelsLocalEast :: Matrix a -> Int
levelsLocalEast Border = 0
levelsLocalEast x = 1 + rest where
  rest = levelsLocalEast $ east x

levelsLocalSouth :: Matrix a -> Int
levelsLocalSouth Border = 0
levelsLocalSouth x = 1 + rest where
  rest = levelsLocalSouth $ south x

levelsLocalWest :: Matrix a -> Int
levelsLocalWest Border = 0
levelsLocalWest x = 1 + rest where
  rest = levelsLocalWest $ west x

-- How many levels are above this cell across the matrix
-- Map levelsLocalNorth
-- levelsGlobalNorth :: Matrix a -> Maybe Int
-- levelsGlobalNorth Border = Nothing
-- levelsGlobalNorth (Cell _ n e s w) = let
--   n' = expression
--   in expression

getNorth :: Matrix a -> Int -> Matrix a
getNorth Border _ = Border
getNorth x 0 = north x
getNorth x n = getNorth (north x) (n-1)

getEast :: Matrix a -> Int -> Matrix a
getEast Border _ = Border
getEast x 0 = east x
getEast x n = getEast (east x) (n-1)

getSouth :: Matrix a -> Int -> Matrix a
getSouth Border _ = Border
getSouth x 0 = south x
getSouth x n = getSouth (south x) (n-1)

getWest :: Matrix a -> Int -> Matrix a
getWest Border _ = Border
getWest x 0 = west x
getWest x n = getWest (west x) (n-1)

-- foldMatrix :: (a -> a -> a) -> a -> Matrix a -> b
-- foldMatrix f st (Cell a n e s w) = foldr1 [a, n', e', s', w'] where
--   n' = foldMatrix n
--   e' = foldMatrix e
--   s' = foldMatrix s
--   w' = foldMatrix w
--   cs = [c | c <- [n,e,s,w], c /= Border]

-- CANNOT SOLVE CYCLING PROBLEM, MAYBE GRAPH THEORY HELPS
