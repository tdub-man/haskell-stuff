{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module YTSort
    ( Time
    -- , qLess
    -- , qEither, qCompare
    -- , queueSort
    , secs, fTime, tAdd, tDiff, tComp
    ) where
-- import Queue
import Helpers.Classes
import Helpers.Math(divTimes)
import Helpers.Lists(compMapL)

data MoveOp = Top | Leave | Bottom deriving (Eq,Ord,Show)

type Time = (Int,Int,Int)
type Times = [Time]
type TimeOp = (Time,MoveOp)
type TimeOps = [(Time,MoveOp)]

instance DistA Time Int where
  multA = (*)
  distA (h1,m1,s1) (h2,m2,s2) = abs $ t1 - t2 where
    t1 = (3600 * h1) + (60 * m1) + s1
    t2 = (3600 * h2) + (60 * m2) + s2

secs :: Time -> Int
secs (h,m,s) = (3600 * h) + (60 * m) + s

fTime :: Int -> Time
fTime x = (h,m,s) where
  (h,r) = divMod x 3600
  (m,s) = divMod r 60

tAdd :: Time -> Time -> Time
tAdd x y = fTime $ s1 + s2 where
  s1 = secs x
  s2 = secs y

tDiff :: Time -> Time -> Time
tDiff x = fTime . distA x

tComp :: Time -> Time -> Ordering
tComp (h1,m1,s1) (h2,m2,s2) = mappend h . mappend m $ s where
  h = h1 `compare` h2
  m = m1 `compare` m2
  s = s1 `compare` s2

tMax :: Time -> Time -> Time
tMax a b = if a `tComp` b == LT then b else a

tMin :: Time -> Time -> Time
tMin a b = if a `tComp` b == GT then b else a

timesSorted :: Times -> Bool
timesSorted = all (==LT) . compMapL tComp

tMove :: Time -> Time -> TimeOp
tMove a b = case a `tComp` b of
  LT -> (a,Top)
  EQ -> (a,Leave)
  GT -> (a,Bottom)

startSort :: Times -> (TimeOps,Time)
startSort ts = (tsOps,mid) where
  mid = midElemA ts
  tsOps = map (tMove mid) ts

sortTop :: Times -> TimeOps
sortTop ts = tsOps where
  bot = foldl1 tMax ts
  tsOps = map (`tMove` bot) ts

sortBot :: Times -> TimeOps
sortBot ts = tsOps where
  top = foldl1 tMin ts
  tsOps = map (`tMove` top) ts

evalOps :: TimeOps -> Times
evalOps tsOps = ts where
  tops = filter (\(_,op) -> op == Top) tsOps
  leaves = filter (\(_,op) -> op == Leave) tsOps
  bots = filter (\(_,op) -> op == Bottom) tsOps
  tsOps' = tops ++ leaves ++ bots
  ts = map fst tsOps'

sortTimes :: Times -> Times
sortTimes ts
  | timesSorted ts = ts
  | otherwise = fst . sortHelp $ ([],ts) where
      sortHelp (s,[]) = (s,[])
      sortHelp (s,us) = sortHelp (s',us') where
        xs = evalOps . sortBot $ us
        us' = tail xs
        s' = s ++ [head xs]

-- Collect log of all ops required to sort the list
