{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module ClassHelpers
    ( Distance(dist,distMinMax,midElem)
    ) where

class (Ord n,Num n,Ord (a n)) => Distance a n where
  dist :: a n -> a n -> n
  distMinMax :: (Foldable t) => t (a n) -> a n -> n
  distMinMax xs = \x -> distMin x * distMax x where
    distMin = dist $ minimum xs
    distMax = dist $ maximum xs
  -- Finds middle-most element by distance
  midElem :: (Foldable t) => t (a n) -> a n
  midElem x = foldr1 maxDist x where
    dist' = distMinMax x
    maxDist a b = if max da db == da then a else b where
      da = dist' a
      db = dist' b
