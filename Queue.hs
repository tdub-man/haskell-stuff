module Queue
    ( Queue
    , push
    , pop
    , peek
    , QueueState
    , pushQueue
    , popQueue
    , arrToQueue
    ) where
import Control.Monad.State

data Queue a = Queue { inbox :: [a], outbox :: [a] } deriving (Eq, Show)

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

arrToQueue :: [a] -> Queue a
arrToQueue [] = Queue [] []
arrToQueue x = Queue (reverse x) []
