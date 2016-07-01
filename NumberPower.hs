module NumberPower
    ( numberToArr
    , arrToNumber
    , numberToArr'
    , arrToNumber'
    , digitsToSelf
    , digitsToRevSelf
    , wrapShift
    , digitsToShiftSelf
    ) where
import Control.Applicative()

numberToArr :: Integer -> [Int]
numberToArr = map (read . return) . show

numberToArr' :: Integer -> [Integer]
numberToArr' 0 = []
numberToArr' n = numberToArr' (n `div` 10) ++ [n `mod` 10]

arrToNumber :: [Int] -> Integer
arrToNumber = (\x -> read x :: Integer) . concatMap show

arrToNumber' :: [Integer] -> Integer
arrToNumber' = foldr (\y x -> (10 * x) + y) 0

digitsToSelf :: Integer -> Integer
digitsToSelf x = let
  numArr = numberToArr' x
  powArr = map (\n -> n ^ n) numArr
  in sum powArr

digitsToRevSelf :: Integer -> Integer
digitsToRevSelf x = let
  numArr = numberToArr' x
  powArr = zipWith (^) numArr $ reverse numArr
  in sum powArr

wrapShift :: [a] -> Int -> [a]
wrapShift [] _ = []
wrapShift x 0 = x
wrapShift (x:xs) n = wrapShift (xs ++ [x]) (n - 1)

digitsToShiftSelf :: Integer -> Int -> Integer
digitsToShiftSelf x n = let
  numArr = numberToArr' x
  powArr = zipWith (^) numArr $ wrapShift numArr n
  in sum powArr
