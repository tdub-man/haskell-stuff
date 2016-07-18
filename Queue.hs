module Queue
    ( Queue(Queue), emptyQueue
    , push, pop, peek
    , QueueState, pushQueue, popQueue
    , fillQueue, flushQueue
    , chainQueue, chainQueueWhile, chainQueueMap, takeQueueWhile
    , zipQueue
    , elem'
    ) where
import Control.Applicative
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

-- instance Monad Queue where
--   return = pure
--   (Queue [] []) >>= _ = emptyQueue
--   qa >>= f = where
--     definitions

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
