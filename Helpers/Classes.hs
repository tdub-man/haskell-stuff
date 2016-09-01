{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module Helpers.Classes
    ( DistA(distA,multA,distMinMaxA,midElemA)
    , DistB(distB,multB,distMinMaxB,midElemB)
    ) where

-- For datatype a of kind *
class (Ord a, Ord n) => DistA a n | a -> n, n -> a where
  distA :: a -> a -> n
  -- Some form of multiplication
  multA :: n -> n -> n
  distMinMaxA :: (Foldable t) => t a -> (a -> n)
  distMinMaxA xs = \x -> distMin x `multA` distMax x where
    distMin = distA $ minimum xs
    distMax = distA $ maximum xs
  midElemA :: (Foldable t) => t a -> a
  midElemA x = foldr1 maxDist x where
    dist' = distMinMaxA x
    maxDist a b = if max da db == da then a else b where
      da = dist' a
      db = dist' b

-- For datatype a of kind * -> *
class (Ord (a n), Ord n) => DistB (a :: * -> *) n | a n -> n, n -> a n where
  distB :: a n -> a n -> n
  multB :: n -> n -> n
  distMinMaxB :: (Foldable t) => t (a n) -> (a n -> n)
  distMinMaxB xs = \x -> distMin x `multB` distMax x where
    distMin = distB $ minimum xs
    distMax = distB $ maximum xs
  midElemB :: (Foldable t) => t (a n) -> a n
  midElemB x = foldr1 maxDist x where
    dist' = distMinMaxB x
    maxDist a b = if max da db == da then a else b where
      da = dist' a
      db = dist' b

-- EXAMPLES

-- instance DistA (Maybe Int) Int where
--   distA Nothing _ = 0
--   distA _ Nothing = 0
--   distA (Just a) (Just b) = abs $ a - b
--
-- instance DistB Maybe Int where
--   distB = distA
--
-- instance DistA (Maybe Double) Double where
--   distA Nothing _ = 0
--   distA _ Nothing = 0
--   distA (Just a) (Just b) = abs $ a - b
--
-- instance DistB Maybe Double where
--   distB = distA
--
-- d1 = [ Just x | x <- [1..5] :: [Int] ]
-- d2 = [ Just x | x <- [1..5] :: [Double] ]
