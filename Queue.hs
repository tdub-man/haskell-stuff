module Queue
    ( Queue(Queue), emptyQueue
    , push, pop, peek
    , QueueState, pushQueue, popQueue
    , fillQueue, flushQueue
    , chainQueue, chainQueues, chainQueueWhile, chainQueueMap, takeQueueWhile
    , zipQueue
    , elem'
    ) where
import Control.Applicative
import Data.Monoid
import Control.Monad.State

data Queue a = Queue { inbox :: [a], outbox :: [a] } deriving (Eq)

emptyQueue :: Queue a
emptyQueue = Queue [] []

instance (Show a) => Show (Queue a) where
  show (Queue inb out) = show $ out ++ reverse inb

instance Functor Queue where
  fmap _ (Queue [] []) = emptyQueue
  fmap f q = chainQueueMap f q emptyQueue

instance Applicative Queue where
  pure e = push e emptyQueue
  (Queue [] []) <*> _ = emptyQueue
  qf <*> qa = let
    (Just f,qf') = pop qf
    qb = fmap f qa
    in chainQueue qb (qf' <*> qa)

instance Monoid (Queue m) where
  mempty = emptyQueue
  mappend = chainQueue
  mconcat = chainQueues

instance Monad Queue where
  return = pure
  (Queue [] []) >>= _ = mempty
  qa >>= f = let
    qb = fmap f qa
    in queueFoldl mappend mempty qb

push :: a -> Queue a -> Queue a
push e (Queue inb out) = Queue (e:inb) out

pop :: Queue a -> (Maybe a, Queue a)
pop q@(Queue [] [])     = (Nothing, q)
pop (Queue inb [])      = pop $ Queue [] (reverse inb)
pop (Queue inb (o:out)) = (Just o, Queue inb out)

peek :: Queue a -> (Maybe a, Queue a)
peek q@(Queue [] [])   = (Nothing, q)
peek (Queue inb [])    = peek $ Queue [] (reverse inb)
peek q@(Queue _ (o:_)) = (Just o, q)

type QueueState a = State (Queue a)

pushQueue :: a -> QueueState a ()
pushQueue e = state $ \q -> ((),push e q)

popQueue :: QueueState a (Maybe a)
popQueue = state $ \q -> pop q

fillQueue :: [a] -> Queue a
fillQueue = Queue []

flushQueue :: Queue a -> [a]
flushQueue q = case pop q of
  (Nothing,_) -> []
  (Just x,q') -> x:flushQueue q'

chainQueue :: Queue a -> Queue a -> Queue a
chainQueue q (Queue [] []) = q
chainQueue q1 q2 = chainQueue q1' q2' where
  (Just e,q2') = pop q2
  q1' = push e q1

chainQueues :: [Queue x] -> Queue x
chainQueues [] = emptyQueue
chainQueues x = foldr1 chainQueue x

chainQueueWhile :: (a -> Bool) -> Queue a -> Queue a -> Queue a
chainQueueWhile _ q1 (Queue [] []) = q1
chainQueueWhile f q1 q2 = if f e
  then chainQueueWhile f (push e q1) q2' else q1
  where
    (Just e,q2') = pop q2

takeQueueWhile :: (a -> Bool) -> Queue a -> Queue a
takeQueueWhile f = chainQueueWhile f emptyQueue

chainQueueMap :: (a -> b) -> Queue a -> Queue b -> Queue b
chainQueueMap _ (Queue [] []) qb = qb
chainQueueMap f qa qb = chainQueueMap f qa' qb' where
  (Just e,qa') = pop qa
  qb' = push (f e) qb

queueFoldl :: (b -> a -> b) -> b -> Queue a -> b
queueFoldl _ x (Queue [] []) = x
queueFoldl f b qa = let
  (Just a,qa') = pop qa
  b' = f b a
  in queueFoldl f b' qa'

queueFoldl1 :: (a -> a -> a) -> Queue a -> Maybe a
queueFoldl1 f qa = case pop qa of
  (Nothing,_) -> Nothing
  (Just x,qa') -> case queueFoldl1 f qa' of
    Nothing -> Just x
    Just y -> Just $ f x y

zipQueue :: (a -> b -> c) -> Queue a -> Queue b -> Queue c -> Queue c
zipQueue _ (Queue [] []) _ qc = qc
zipQueue _ _ (Queue [] []) qc = qc
zipQueue f qa qb qc = zipQueue f qa' qb' qc' where
  (Just ea,qa') = pop qa
  (Just eb,qb') = pop qb
  qc' = push (f ea eb) qc

elem' :: (Eq a) => a -> Queue a -> Bool
elem' _ (Queue [] []) = False
elem' x q = let
  (Just a,q') = pop q
  in (x == a) || elem' x q'
