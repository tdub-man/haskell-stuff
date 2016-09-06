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

data MoveOp = Top | Leave | Bottom deriving (Eq,Ord)

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
  LT -> (b,Top)
  EQ -> (b,Leave)
  GT -> (b,Bottom)

startSort :: Times -> (TimeOps,Time)
startSort ts = (tsOps,mid) where
  mid = midElemA ts
  tsOps = map (tMove mid) ts

sortTop :: Times -> TimeOps
sortTop ts = tsOps where
  bot = foldl1 tMax ts
  tsOps = map (tMove bot) ts

sortBot :: Times -> TimeOps
sortBot ts = tsOps where
  top = foldl1 tMin ts
  tsOps = map (tMove top) ts

-- evalOps :: TimeOps -> Times
-- evalOps tsOps = where
--   mv (x:xs) = case snd x of
--     Top -> mv $ (fst x,Leave):xs
--     Leave -> mv

-- closer :: (Num a, Ord a) => a -> (a,a) -> (a,a)
-- closer a (b,x) = if dist a x < dist b x then (a,x) else (b,x)

-- qLess :: (Ord a) => Queue a -> Queue a
-- qLess (Queue [] []) = emptyQueue
-- qLess q = takeQueueWhile (< e) q where
--   (Just e,_) = pop q
--
-- type QueueTriple x = (Queue x, Queue x, Queue x)
--
-- emptyQueueTri :: QueueTriple a
-- emptyQueueTri = (emptyQueue,emptyQueue,emptyQueue)
--
-- qCompare :: (x -> Ordering) -> Queue x -> QueueTriple x -> QueueTriple x
-- qCompare _ (Queue [] []) qtri = qtri
-- qCompare f qx (qa,qb,qc) = let
--   (Just x,qx') = pop qx
--   qtri' = case f x of
--     LT -> (push x qa,qb,qc)
--     EQ -> (qa,push x qb,qc)
--     GT -> (qa,qb,push x qc)
--   in qCompare f qx' qtri'
--
-- queueSort :: (Ord x) => Queue x -> Queue x
-- queueSort (Queue [] []) = emptyQueue
-- queueSort qx = chainQueues [qa',qb,qc'] where
--   (Just x,_) = pop qx
--   comp = (`compare` x)
--   (qa,qb,qc) = qCompare comp qx emptyQueueTri
--   qa' = queueSort qa
--   qc' = queueSort qc
