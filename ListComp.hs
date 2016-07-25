module ListComp
    ( fizzBuzz
    , nth
    , subList
    ) where

fbCond :: (Integral a,Show a) => a -> String
fbCond i
    | i `mod` 15 == 0 = "FIZZBUZZ"
    | i `mod` 3 == 0 = "FIZZ"
    | i `mod` 5 == 0 = "BUZZ"
    | otherwise = show i

fizzBuzz :: [String]
fizzBuzz = [fbCond i | i <- [1..]]

nth :: [a] -> Int -> Maybe a
nth [] _ = Nothing
nth (x:_) 0 = Just x
nth (_:xs) n = nth xs (n - 1)

subList :: Int -> [a] -> Maybe ([a],[a])
subList _ [] = Nothing
subList n xs
  | length xs >= n = let
      taken = take n xs
      rest = drop n xs
      in Just (taken,rest)
  | otherwise = Just ([],xs)

concatZip :: [a] -> [a] -> [a]
concatZip a = concat . zipWith (\x y -> [x,y]) a
