module ClassHelpers
    ( Distance(dist,distMinMax,midElem)
    ) where

class (Ord a) => Distance a where
  dist :: (Num b) => a -> a -> b
  distMinMax :: (Foldable t, Num b) => t a -> a -> b
  distMinMax xs = \x -> distMin x * distMax x where
    distMin = dist $ minimum xs
    distMax = dist $ maximum xs
  -- Finds middle-most element by distance
  midElem :: (Foldable t) => t a -> a
  midElem x = foldr1 maxDist x where
    dist' = distMinMax x
    maxDist a b = if max da db == da then a else b where
      da = dist' a
      db = dist' b
