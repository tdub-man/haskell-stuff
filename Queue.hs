module Queue
    ( Queue
    , emptyQueue
    , push
    , pop
    , peek
    , QueueState
    , pushQueue
    , popQueue
    , fillQueue
    , flushQueue
    ) where
import Control.Monad.State

data Queue a = Queue { inbox :: [a], outbox :: [a] } deriving (Eq, Show)

emptyQueue :: Queue a
emptyQueue = Queue [] []

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
