module ListHelpers
    ( nth
    , subList
    , concatZip
    , combinations
    , nPerms
    , moveXTo
    , groupWithNs
    , longest
    ) where
import Data.List(tails,nub,find,delete)
import Control.Monad(replicateM)

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

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = do
  (x:xs') <- tails xs
  rest <- combinations (n-1) xs'
  return $ x:rest

nPerms :: (Eq a) => Int -> [a] -> [[a]]
nPerms n = filter ((==n).length.nub) . replicateM n

moveXTo :: (Eq a) => a -> ([a],[a]) -> ([a],[a])
moveXTo x (as,bs) = case find (==x) as of
  Nothing -> (as,bs)
  Just _ -> (delete x as,x:bs)

groupWithNs :: [a] -> [Int] -> [[a]]
groupWithNs [] _ = []
groupWithNs xs [] = [xs]
groupWithNs xs (n:ns) = pre:suf' where
  (pre,suf) = splitAt n xs
  suf' = groupWithNs suf ns

longest :: [[a]] -> Int
longest xs = maximum $ map length xs
