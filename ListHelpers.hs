module ListHelpers
    ( nth
    , subList
    , concatZip
    , combinations
    , nPerms
    , moveXTo
    , groupWithNs
    , longest
    , compR
    , compL
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

compR :: (a -> a -> a) -> [a] -> [a]
compR f as = snd . compRTup $ (as,[]) where
  compRTup ([],ys)  = ([],ys)
  compRTup ([_],ys) = ([],ys)
  compRTup (xs,ys)  = compRTup (xs',y:ys) where
    xs' = init xs
    l = last xs
    l2 = last xs'
    y = f l l2

compL :: (a -> a -> a) -> [a] -> [a]
compL f as = snd . compLTup $ (as,[]) where
  compLTup ([],ys)  = ([],ys)
  compLTup ([_],ys) = ([],ys)
  compLTup (xs,ys)  = compLTup (xs',y:ys) where
    xs' = tail xs
    h = head xs
    h2 = head xs'
    y = f h h2
