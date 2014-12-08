module Queue (
	Queue,
	empty,
	enqueue,
	dequeue
) where

data Queue a = Queue [a]
	deriving Show

empty :: Queue a
empty = Queue []

isEmpty :: Queue a -> Bool
isEmpty (Queue xs) = null xs

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [])       = Nothing
dequeue (Queue (x : xs)) = Just (x, Queue xs)

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs) = Queue (xs ++ [x])

toList :: Queue a -> [a]
toList (Queue xs) = xs
