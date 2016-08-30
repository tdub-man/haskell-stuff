{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module YTSort
    ( Time
    , qLess
    -- , qEither, qCompare
    , queueSort
    , secs, fTime, tAdd, tDiff
    ) where
import Queue
import Helpers.Classes
import Helpers.Math(divTimes)

type Time = (Int,Int,Int)
type QueueTime = Queue Time

instance DistA Time Int where
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

-- closer :: (Num a, Ord a) => a -> (a,a) -> (a,a)
-- closer a (b,x) = if dist a x < dist b x then (a,x) else (b,x)

qLess :: (Ord a) => Queue a -> Queue a
qLess (Queue [] []) = emptyQueue
qLess q = takeQueueWhile (< e) q where
  (Just e,_) = pop q

type QueueTriple x = (Queue x, Queue x, Queue x)

emptyQueueTri :: QueueTriple a
emptyQueueTri = (emptyQueue,emptyQueue,emptyQueue)

qCompare :: (x -> Ordering) -> Queue x -> QueueTriple x -> QueueTriple x
qCompare _ (Queue [] []) qtri = qtri
qCompare f qx (qa,qb,qc) = let
  (Just x,qx') = pop qx
  qtri' = case f x of
    LT -> (push x qa,qb,qc)
    EQ -> (qa,push x qb,qc)
    GT -> (qa,qb,push x qc)
  in qCompare f qx' qtri'

queueSort :: (Ord x) => Queue x -> Queue x
queueSort (Queue [] []) = emptyQueue
queueSort qx = chainQueues [qa',qb,qc'] where
  (Just x,_) = pop qx
  comp = (`compare` x)
  (qa,qb,qc) = qCompare comp qx emptyQueueTri
  qa' = queueSort qa
  qc' = queueSort qc
