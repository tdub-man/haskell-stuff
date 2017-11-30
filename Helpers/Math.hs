module Helpers.Math
    ( (.^)
    , evens
    , odds
    , squares
    , triangles
    , squareRoot
    , ceilDiv
    , dist
    , divTimes
    , factorial
    , intSize
    ) where

(.^) :: Num a => a -> Integer -> a
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

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = fst . factHelp $ (1,n) where
  factHelp (a,0) = (a,0)
  factHelp (a,b) = factHelp (a*b,b-1)

intSize :: (Integral a) => a -> a
intSize 0 = 1
intSize n = fst . sizeHelp $ (0,n) where
  sizeHelp (s,0) = (s,0)
  sizeHelp (s,x) = sizeHelp (s+1,x `div` 10)
