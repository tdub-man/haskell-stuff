module Types
( Skittle
, isFavoriteFlavor
, colorCount
, sortSkittleCount
, searchColor
) where

import Data.List (sort,group,sortBy)

data Skittle = Purple | Red | Orange | Yellow | Green deriving (Eq,Ord,Show)
type SkittleCount = (Skittle,Int)

isFavoriteFlavor :: Skittle -> Bool
isFavoriteFlavor = (Orange == )

colorCount :: [Skittle] -> [SkittleCount]
colorCount = map colorLength . group . sort

sortSkittleCount :: [SkittleCount] -> [SkittleCount]
sortSkittleCount = sortBy compareSkittleCount

compareSkittleCount :: SkittleCount -> SkittleCount -> Ordering
compareSkittleCount a b = snd a `compare` snd b

-- Assumes homogenous list of Skittles
colorLength :: [Skittle] -> SkittleCount
colorLength ss = (head ss, length ss)

-- Search for skittles of a color
searchColor :: Skittle -> [Skittle] -> Either String SkittleCount
searchColor c ss =
    case filter (c == ) ss of
        [] -> Left $ "No " ++ show c ++ " skittles found"
        cs -> Right (c,length cs)
