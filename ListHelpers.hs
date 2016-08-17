module ListHelpers
    ( lengthIntegral
    , nth
    , subList
    , concatZip
    , combinations
    , nPerms
    , moveXTo
    , groupWithNs
    , shorter
    , shortest
    , longer
    , longest
    , compR
    , compL
    , middle
    , iterateN
    , takeThrough
    , distMinMax
    , numMid
    ) where
import Data.List(tails,nub,find,delete)
import Control.Monad(replicateM)
import MathHelpers(dist)

lengthIntegral :: (Integral b) => [a] -> b
lengthIntegral = foldr (\_ -> (+) 1) 0

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

shorter :: [a] -> [a] -> [a]
shorter a b = if length a <= length b then a else b

shortest :: [[a]] -> [a]
shortest = foldr1 shorter

longer :: [a] -> [a] -> [a]
longer a b = if length a >= length b then a else b

longest :: [[a]] -> [a]
longest = foldr1 longer

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

middle :: [a] -> [a]
middle xs
    | length xs == 1 || length xs == 2 = []
    | otherwise = init . tail $ xs

iterateN :: (a -> a) -> a -> Int -> a
iterateN f x n = iterate f x !! n

takeThrough :: (a -> Bool) -> [a] -> ([a],[a])
takeThrough f xs = tWhile f ([],xs) where
  tWhile _ (as,[]) = (as,[])
  tWhile g (as,b:bs) = if f b
    then tWhile g (as ++ [b],bs)
    else (as ++ [b],bs)
  -- (_,ys) = tWhile f (xs,[])

-- Multiplies the distance of a number
-- from the minimum and maximum of the list
distMinMax :: (Num a, Ord a) => [a] -> (a -> a)
distMinMax xs = \x -> distMin x * distMax x where
  distMin = dist $ minimum xs
  distMax = dist $ maximum xs

-- Doesn't necessarily find the middle element,
-- just the numerically middle-most element
-- between the minimum and maximum
numMid :: (Num a, Ord a) => [a] -> a
numMid x = foldr1 maxDist x where
  dist' = distMinMax x
  maxDist a b = if max da db == da then a else b where
    da = dist' a
    db = dist' b
