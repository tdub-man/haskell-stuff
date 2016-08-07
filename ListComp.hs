module ListComp
    ( fizzBuzz
    ) where

fbCond :: (Integral a,Show a) => a -> String
fbCond i
    | i `mod` 15 == 0 = "FIZZBUZZ"
    | i `mod` 3 == 0 = "FIZZ"
    | i `mod` 5 == 0 = "BUZZ"
    | otherwise = show i

fizzBuzz :: [String]
fizzBuzz = [fbCond i | i <- [1..]]
