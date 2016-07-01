module Collatz
( Ctree
, treeBuild
, collatzTree
, collatzTreeTrim
, traverseTree
, drawTree
, collatz
, longCollatz
, missingCollatz
) where

import Data.List
import qualified Data.Set as Set

data Ctree = NullTree | Node Integer Ctree Ctree deriving (Show, Eq)

type Depth = Integer

singletonC :: Integer -> Ctree
singletonC x = Node x NullTree NullTree

treeBuild :: Ctree -> Depth -> Ctree
treeBuild t 0 = t
treeBuild NullTree x = treeBuild (singletonC 1) (x-1)
treeBuild (Node 0 _ _) _ = NullTree
treeBuild (Node n _ _) x = Node n lTree rTree where
                             num = if (n - 1) `mod` 3 == 0
                               then (n - 1) `div` 3
                               else 0
                             sL = singletonC num
                             sR = singletonC (2*n)
                             lTree = treeBuild sL (x-1)
                             rTree = treeBuild sR (x-1)

-- Build a tree without duplicating branches (i.e. loop at 1,2,4,1...)
treeBuildTrim :: Set.Set Integer -> Ctree -> Depth -> Ctree
treeBuildTrim _ t 0 = t
treeBuildTrim _ NullTree x = treeBuildTrim (Set.singleton 1) (singletonC 1) (x-1)
treeBuildTrim _ (Node 0 _ _) _ = NullTree
treeBuildTrim s (Node n _ _) x = Node n lTree rTree where
                                   num = if (n - 1) `mod` 3 == 0
                                     then (n - 1) `div` 3
                                     else 0
                                   sL = if Set.member num s
                                     then NullTree
                                     else singletonC num
                                   sR = if Set.member (2*n) s
                                     then NullTree
                                     else singletonC (2*n)
                                   s' = Set.insert num $ Set.insert (2*n) s
                                   lTree = treeBuildTrim s' sL (x-1)
                                   rTree = treeBuildTrim s' sR (x-1)

-- Convenience function
collatzTree :: Depth -> Ctree
collatzTree = treeBuild NullTree

-- Convenience function
collatzTreeTrim :: Depth -> Ctree
collatzTreeTrim = treeBuildTrim Set.empty NullTree

traverseTree :: Ctree -> String -> [Integer]
traverseTree NullTree _ = []
traverseTree (Node x _ _) [] = [x]
traverseTree (Node x l r) (b:bs)
    | b == 'L' = x : traverseTree l bs
    | b == 'R' = x : traverseTree r bs

drawTree :: Ctree -> String
drawTree  = unlines . draw

draw :: Ctree -> [String]
draw (Node x l r) = show x : drawSubTrees [l,r] where
  drawSubTrees [] = []
  drawSubTrees [t] =
    "|" : shift "`- " "   " (draw t)
  drawSubTrees (t:ts) =
    "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
  shift first other = zipWith (++) (first : repeat other)

collatz :: (Integral a) => a -> [a]
collatz 0 = []
collatz 1 = [1]
collatz a
    | even a = a:collatz (a `div` 2)
    | odd a = a:collatz (3 * a + 1)
    | otherwise = error "Wat?"

longCollatz :: Integer -> [[Integer]]
longCollatz a = [collatz i | i <- [1..(2^a)], toInteger (length (collatz i)) == a]

-- find how how many numbers are missing from a collatz sequence, i.e. [1,2,4] missing [3]
missingCollatz :: Integer -> [Integer]
missingCollatz x = let col = sort . collatz $ x
                       l = last col
                   in [1..l] \\ col
