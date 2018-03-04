module UpArrow
( upArrow
, upArrow'
) where

import Data.List

upArrow :: Integer -> Integer -> Integer -> Integer
upArrow a n b
    | n < 0 = error "What are negative up-arrows?"
    | n == 0 = a*b  -- 0 arrow is multiplication
    | b == 0 = 1    -- x to the power of 0 is 1
    | n == 1 = a^b
    | otherwise = let invPow = flip (^)
                      baseCount = fromIntegral (n + b - 2)
                      baseList = replicate baseCount a
                  in foldl' invPow 1 baseList
    -- | otherwise = upArrow a (n-1) (upArrow a n (b-1))

upArrow' :: Integer -> Integer -> Integer -> [Integer]
upArrow' a n b = scanl (flip (^)) 1 (replicate (fromIntegral (n+b-2)) a)
