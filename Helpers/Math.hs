module Helpers.Math
    ( evens
    , odds
    , squares
    , triangles
    , squareRoot
    , ceilDiv
    , dist
    , divTimes
    ) where

(.^) :: Num a => a -> Int -> a
(.^) x n = x^n

evens :: [Integer]
evens = map (2*) [1..]

odds :: [Integer]
odds = map (\x -> 2*x - 1) [1..]

squares :: [Integer]
squares = map (\x -> x*x) [1..]

-- Squares are repeated additions of consecutive odd numbers
squaresOdds :: [Integer]
squaresOdds = scanl1 (+) odds

squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (.^2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r.^2 <= n && n < (r+1).^2
  in  head $ dropWhile (not . isRoot) iters

triangles :: [Integer]
triangles = scanl1 (+) [1..]

ceilDiv :: (Integral a) => a -> a -> a
ceilDiv a b = if a `mod` b == 0
  then a `div` b
  else (a `div` b) + 1

dist :: (Num a) => a -> a -> a
dist a b = abs $ a-b

divTimes :: (Integral a) => a -> a -> a
divTimes x = fst . divMod x
