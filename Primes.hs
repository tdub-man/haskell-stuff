module Primes
    ( sieveEras
    , sieveForm
    , primeForm
    , isPrime
    , isPrime'
    , nPrimes
    ) where
import Helpers.Math(squareRoot)
import Data.List

type PrimeRec = ([Integer],[Integer])

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

positive :: (Num a,Ord a) => a -> Bool
positive x = x > 0

negative :: (Num a,Ord a) => a -> Bool
negative x = x < 0

mult :: Integer -> Integer -> Bool
a `mult` b = a `mod` b == 0

mult' :: Integer -> Integer -> Bool
mult' a b = min a b == gcd a b

nonMult :: Integer -> Integer -> Bool
a `nonMult` b = a `mod` b > 0

primeForm :: [Integer]
primeForm = concatMap form [1..] where
  form k = [6*k-1,6*k+1]

erasth :: PrimeRec -> [Integer]
erasth (ps,[]) = ps
erasth (ps,p:xs) = let
  xs' = filter (`nonMult` p) xs
  in erasth (ps ++ [p],xs')

sieveEras :: Integer -> [Integer]
sieveEras n = erasth ([],[2..n])

sieveForm :: Integer -> [Integer]
sieveForm n = erasth ([],xs) where
  xs = 2:3:takeWhile (<= n) primeForm

isPrime :: Integer -> Bool
isPrime 1        = False
isPrime 2        = True
isPrime 3        = True
isPrime n
    | even n     = False
    | n `mult` 3 = False
    | otherwise  = let
      xs = [2..squareRoot n]
      xs' = filter odd xs
      xs'' = filter (n `mult`) xs'
      in isEmpty xs''

isPrime' :: Integer -> Bool
isPrime' n
    | even n     = False
    | n `mult` 3 = False
    | otherwise  = let
      xs = [ x | x <- [2..squareRoot n], odd x ]
      findMult = find (\i -> n `mult` i) xs
      in findMult == Nothing

nPrimes :: Int -> [Integer]
nPrimes 1 = [2]
nPrimes 2 = [2,3]
nPrimes n = 2:3:rest where
  rest = take (n-2) . filter isPrime $ primeForm

-- isPrime :: Integer -> Bool
-- isPrime n = let
--   xs = [2..squareRoot n]
--   xs' = filter odd
--   xs' = filter (n `mult`) xs
--   in isEmpty xs'
