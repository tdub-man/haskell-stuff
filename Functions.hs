module Function
( notSumCubes
, exclusiveSubset
, take'
)
where

import Data.List
-- import qualified Data.Map as Map


-- find an exclusive subset given a superset and a subset
exclusiveSubset :: [Integer] -> [Integer] -> [Integer]
exclusiveSubset sups subs = sups \\ subs
-- exclusiveSubset sups subs
--     | all (`elem` sups) subs = let notSubs = not . (`elem` subs)
--                                in filter notSubs sups
--     | otherwise = error "Provided subset is not a subset of provided superset"

-- reverse Collatz, a tree (binary tree for even/odd), starts with 1, describes all numbers

take' :: Int -> ([a],[b]) -> ([a],[b])
take' n (as,bs) = (take n as, take n bs)

notSumCubes :: Int -> [Int]
notSumCubes n = take n $ concatMap (\x -> [9*x+4,9*x+5]) [0..]
