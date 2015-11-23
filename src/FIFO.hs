module FIFO where
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe

data FIFO a = FIFO [a] [a]
              deriving Show

empty :: FIFO a
empty = FIFO [] []

isEmpty :: FIFO a -> Bool
isEmpty (FIFO [] []) = True
isEmpty _ = False

enqueue :: a -> FIFO a -> FIFO a
enqueue x (FIFO front back) = FIFO front (x:back)

-- | Remove the head off the queue.  My type's different from yours
-- because I use Maybe to handle the case where somebody tries to
-- dequeue off an empty FIFO.
dequeue :: FIFO a -> Maybe (a, FIFO a)
dequeue queue = case queue of
                  FIFO [] [] -> Nothing
                  FIFO (x:f) b -> Just (x, FIFO f b)
                  otherwise -> dequeue (rotate queue)
    where rotate (FIFO [] back) = FIFO (reverse back) []


-- | Elements exit the queue in the order they appear in the list.
fromList :: [a] -> FIFO a
fromList xs = FIFO xs []

-- | Elements appear in the result list in the order they exit the queue.
toList :: FIFO a -> [a]
toList = unfoldr dequeue

-- | Enqueue multiple elements.  Elements exit the queue in the order
-- they appear in xs.
add :: [a] -> FIFO a -> FIFO a
add xs q = foldl' (flip enqueue) q xs


-- | Remove n elements from the queue.  My result type is different
-- from yours, again, because I handle the empty FIFO case.  If you
-- try to remove too many elements, you get a bunch of Nothings at
-- the end of your list.
remove :: Int -> FIFO a -> ([Maybe a], FIFO a)
remove n q = runState (removeM n) q

-- | State monad action to dequeue n elements from the state queue.
removeM :: Int -> State (FIFO a) [Maybe a]
removeM n = replicateM n dequeueM

-- | State monad action to dequeue an element from the state queue.
dequeueM :: State (FIFO a) (Maybe a)
dequeueM = do q <- get
              case dequeue q of
                Just (x, q') -> put q' >> return (Just x)
                Nothing -> return Nothing
