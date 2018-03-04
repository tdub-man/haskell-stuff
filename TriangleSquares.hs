module TriangleSquares
    ( triangleNum
    , isPerfectSquare
    , isTriangleNumber
    , isTriangleSquare
    , triSqrs
    , nTriSqrs
    , nextTriangleSquare
    , nthTriangleSquare
    , calcTriangleSquares
    , calcTriangleSquaresFrom
    ) where
import Helpers.Math(squareRoot)

sqr :: Integer -> Integer
sqr n = n * n

binarySearch :: Integer -> Integer -> Integer -> (Integer -> Integer) -> Bool
binarySearch low high term func
    | low > high = False
    | term == midFunc = True
    | term < midFunc = binarySearch low (mid - 1) term func
    | otherwise = binarySearch (mid + 1) high term func
  where
    mid = (low + high) `div` 2
    midFunc = func mid

binarySearch' :: Integer -> (Integer -> Integer) -> Bool
binarySearch' n = binarySearch 1 n n

triangleNum :: Integer -> Integer
triangleNum n = (n + 1) * n `div` 2

isPerfectSquare :: Integer -> Bool
isPerfectSquare n = binarySearch' n sqr

isTriangleNumber :: Integer -> Bool
isTriangleNumber n = binarySearch' n triangleNum

isTriangleSquare :: Integer -> Bool
isTriangleSquare n = isPerfectSquare n && isTriangleNumber n

triSqrs :: [Integer]
triSqrs = filter isTriangleNumber [ sqr x | x <- [1..] ]

nTriSqrs :: Int -> [Integer]
nTriSqrs n = take n triSqrs

nextTriangleSquare :: Integer -> Integer
nextTriangleSquare n
  | isTriangleSquare n = 1 + 17*n + 6*squareRoot (n + 8*sqr n)
  | otherwise = n

nthTriangleSquare :: Integer -> Integer -> Integer
nthTriangleSquare 0 n = n
nthTriangleSquare 1 n = nextTriangleSquare n
nthTriangleSquare n x = nthTriangleSquare (n-1) (nextTriangleSquare x)

calcTriangleSquares :: Int -> [Integer]
calcTriangleSquares 0 = []
calcTriangleSquares 1 = [1]
calcTriangleSquares n = num:prev where
  prev = calcTriangleSquares (n-1)
  num = nextTriangleSquare . head $ prev

calcTriangleSquaresFrom :: Integer -> Integer -> [Integer] -> [Integer]
calcTriangleSquaresFrom 0 _ xs = xs
calcTriangleSquaresFrom n x xs = calcTriangleSquaresFrom (n-1) next xs' where
  next = nextTriangleSquare x
  xs' = xs ++ [next]
