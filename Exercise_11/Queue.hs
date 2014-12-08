module Queue (
	-- Queue only exports the data type Queue but not the constructor Queue
	-- which means the user who implements this module cannot create its own Queue without using the function empty
	-- this guarantees that the user cannot abuse the implemented functions by misusing them with own created queues
	Queue,
	empty,
	dequeue,
	enqueue,
	toList
) where

-- new type which defines a queue (FIFO buffer)
newtype Queue a = Queue [a]
	deriving Show

-- interface
empty :: Queue a
isEmpty :: Queue a -> Bool
dequeue :: Queue a -> Maybe (a, Queue a)
enqueue :: a -> Queue a -> Queue a
toList :: Queue a -> [a]

-- implementation of the interface
-- returns an empty queue
empty = Queue []

-- returns wether this Queue is empty or not
isEmpty (Queue []) = True
isEmpty (Queue _) = False

-- returns the bottommost element on the queue and the new queue without this element
-- returns nothing if the passed queue is empty
dequeue (Queue []) = Nothing
dequeue (Queue (x:xs)) = Just (x, Queue xs)

-- inserts a new element at the end of the queue (after the topmost element)
enqueue x (Queue xs) = Queue (xs ++ [x])

-- converts a queue to a list
toList (Queue xs) = xs