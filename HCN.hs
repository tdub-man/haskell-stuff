module HCN
    ( coprime
    , totient
    , primeCount
    , primorialN
    , primorials
    , primorialsN
    , divisors
    , numDivisors
    , factors
    , smallestNDivs
    ) where
import Primes
import Data.List
-- import ListHelpers(compR)

length' :: [a] -> Integer
length' = foldr (\_ -> (+) 1) 0

coprime :: (Integral a) => a -> a -> Bool
coprime a b = gcd a b == 1

totient :: (Integral a) => a -> Int
totient n = length . filter (coprime n) $ [1..n]

primeCount :: Integer -> Int
primeCount = length . sieveEras

-- Primorial for natural numbers
primorialN :: Integer -> Integer
primorialN = product . sieveEras

primorials :: [Integer]
primorials = nub . map primorialN $ [1..]

-- Cumulative product of primes up to n
primorialsN :: Integer -> [Integer]
primorialsN = map product . inits . sieveEras

divisors :: Integer -> [Integer]
divisors n = filter dividesN [1..n] where
  dividesN x = n `mod` x == 0

numDivisors :: Integer -> Integer
numDivisors = length' . divisors

factors :: Integer -> [Integer]
factors = filter isPrime . divisors

-- smallestBigDivs :: [Integer]
-- smallestBigDivs = [ let (Just n) = find (\i -> numDivisors i == x) [1..] in n | x <- [1..] ]

smallestNDivs :: [Integer]
smallestNDivs = do
  x <- [1..]
  let (Just n) = if isPrime x
        then Just $ 2 ^ (x-1)
        else find (\i -> numDivisors i == x) [1..]
  return n
