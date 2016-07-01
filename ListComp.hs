module Main where

main :: IO ()
main = putStrLn "Hello World"

fbCond :: Integral a => a -> String
fbCond i
    | i `mod` 15 == 0 = "FIZZBUZZ"
    | i `mod` 3 == 0 = "FIZZ"
    | otherwise = "BUZZ"

fizzBuzz :: [String]
fizzBuzz = [fbCond i | i <- [1..], (i `mod` 3 == 0) || (i `mod` 5 == 0)]

infBool :: [Bool]
infBool = [even i | i <- [1..]]
