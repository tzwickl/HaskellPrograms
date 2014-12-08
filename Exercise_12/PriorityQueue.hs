module PriorityQueue (
	PrioQueue,
	empty,
	isEmpty,
	insert,
	extractMin,
	union
) where

----------------------------------- Module Description -----------------------------------
{-
This priority queue is implemented as a skew heap.
A skew heap is defined as following:

A skew heap (or self-adjusting heap) is a heap data structure implemented as a binary tree.
Skew heaps are advantageous because of their ability to merge more quickly than binary heaps. 
In contrast with binary heaps, there are no structural constraints, so there is no guarantee 
that the height of the tree is logarithmic. Only two conditions must be satisfied:

	- The general heap-order must be enforced
	- Every operation (insert, extractMin) on two skew heaps must be done using a 
	  special skew heap merge which is in this case implemented as union
-}
--------------------------------- Data Type definitions ----------------------------------

-- the internal data representation of the priority queue
data PrioQueue a = Empty | Node a (PrioQueue a) (PrioQueue a)
	deriving Show

--------------------------------------- Interface ----------------------------------------

empty      :: Ord a => PrioQueue a
isEmpty    :: Ord a => PrioQueue a -> Bool
insert     :: Ord a => a -> PrioQueue a -> PrioQueue a
extractMin :: Ord a => PrioQueue a -> (a, PrioQueue a)
union      :: Ord a => PrioQueue a -> PrioQueue a -> PrioQueue a

-------------------------- Implementation of the Interface -------------------------------

-- the function empty returns an empty priority queue
empty = Empty

-- isEmpty checks if the passed priority queue is empty or not and returns the result
isEmpty Empty 	= True
isEmpty _ 		= False

-- insert inserts a new element in the priority queue
insert elem pq = union (Node elem Empty Empty) pq

-- extractMin returns the minimum element on the priority queue and the new priority queue
-- the min element is always on the topmost position of the priority queue
extractMin (Node val l r) = (val, union l r)

-- union merges two priority queues according to the rule of the priority queue (skew heap)
-- so that after the merge the new priority queue doesn't lose its property of a heap
union Empty  pq    = pq
union pq     Empty = pq
union (pq1 @ (Node val1 l1 r1)) (pq2 @ (Node val2 l2 r2))
	| val1 <= val2 = Node val1 (union pq2 r1) l1
	| otherwise    = Node val2 (union pq1 r2) l2
	
------------------------ End of implementation of the Interface --------------------------