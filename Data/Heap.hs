{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | 
-- An efficent implementation of min-, max- or custom-priority heaps
-- based on the leftist-heaps from Chris Okasaki's book \"Purely Functional Data
-- Structures\", Cambridge University Press, 1998, chapter 3.1.
module Data.Heap (
	-- * Heap type
	Heap, MinHeap, MaxHeap,
	HeapPolicy(..), MinPolicy, MaxPolicy,
	-- * Query
	null, isEmpty, size, findHead,
	-- * Construction
	empty, singleton,
	insert, deleteHead, deleteFindHead,
	removeWhile,
	-- * Combine
	union, unions,
	-- * Conversion
	-- ** Lists
	fromList, toList, elems,
	-- ** Ordered lists
	fromAscList, toAscList,
	-- * Debugging
	check
) where

import Data.List (foldl')
import Data.Monoid
import Data.Ord
import Prelude hiding (null)

-- |
-- The basic 'Heap' type.
data Heap p a
	= Empty
	| Tree !Int a !(Heap p a) !(Heap p a)

-- |
-- A 'Heap' which will always extract the minimum first.
type MinHeap a = Heap MinPolicy a

-- |
-- A 'Heap' with inverted order: The maximum will be extracted first.
type MaxHeap a = Heap MaxPolicy a

instance (Show a) => Show (Heap p a) where
	show h = "fromList " ++ (show . toList) h

instance (HeapPolicy p a) => Eq (Heap p a) where
	h1 == h2 = EQ == compare h1 h2

instance (HeapPolicy p a) => Ord (Heap p a) where
	compare h1 h2 = compare' (toAscList h1) (toAscList h2)
		where	compare' [] [] = EQ
			compare' [] _  = LT
			compare' _  [] = GT
			compare' (x:xs) (y:ys) = case heapCompare h1 x y of
				EQ -> compare' xs ys
				c  -> c

instance (HeapPolicy p a) => Monoid (Heap p a) where
	mempty  = empty
	mappend = union
	mconcat = unions

-- |
-- The 'HeapPolicy' class defines an order on the elements contained within
-- a 'Heap'.
class HeapPolicy p a where
	-- |
	-- Compare two elements, just like 'compare' of the 'Ord' class.
	-- /The first parameter must be ignored by the implementation!/
	heapCompare :: Heap p a -> a -> a -> Ordering

-- |
-- Policy type for a 'MinHeap'.
data MinPolicy = MinPolicy

instance (Ord a) => HeapPolicy MinPolicy a where
	heapCompare = const compare

-- |
-- Policy type for a 'MaxHeap'
data MaxPolicy = MaxPolicy

instance (Ord a) => HeapPolicy MaxPolicy a where
	heapCompare = const (flip compare)

-- |
-- /O(1)/. Is the 'Heap' empty?
null :: Heap p a -> Bool
null Empty = True
null _     = False

-- |
-- /O(1)/. Is the 'Heap' empty?
isEmpty :: Heap p a -> Bool
isEmpty = null

-- |
-- /O(1)/. Calculate the rank of a 'Heap'.
rank :: Heap p a -> Int
rank Empty = 0
rank (Tree r _ _ _) = r

-- |
-- /O(n)/. The number of elements in the 'Heap'.
size :: (Num n) => Heap p a -> n
size Empty = 0
size (Tree _ _ a b) = 1 + size a + size b

-- |
-- /O(1)/. Finds the minimum (depending on the 'HeapPolicy') of the 'Heap'.
findHead :: (HeapPolicy p a) => Heap p a -> a
findHead = fst . deleteFindHead

-- |
-- /O(1)/. Constructs an empty 'Heap'.
empty :: Heap p a
empty = Empty

-- |
-- /O(1)/. Create a singleton 'Heap'.
singleton :: a -> Heap p a
singleton x = Tree 1 x empty empty

-- |
-- /O(log n)/. Insert an element in the 'Heap'.
insert :: (HeapPolicy p a) => a -> Heap p a -> Heap p a
insert x h = union h (singleton x)

-- |
-- /O(log n)/. Delete the minimum (depending on the 'HeapPolicy')
-- from the 'Heap'.
deleteHead :: (HeapPolicy p a) => Heap p a -> Heap p a
deleteHead = snd . deleteFindHead

-- |
-- /O(log n)/. Find the minimum (depending on the 'HeapPolicy') and
-- delete it from the 'Heap'.
deleteFindHead :: (HeapPolicy p a) => Heap p a -> (a, Heap p a)
deleteFindHead Empty = (error "Heap is empty", Empty)
deleteFindHead (Tree _ x a b) = (x, union a b)

-- |
-- Removes and returns results as long as the supplied function
-- returns 'Just'.
removeWhile :: (HeapPolicy p a) => (a -> Maybe b) -> Heap p a -> (Heap p a, [b])
removeWhile f heap
	| null heap = (heap, [])
	| otherwise = let (ak, nheap) = deleteFindHead heap in case f ak of
		Nothing  -> (heap, [])
		Just rem -> let (nnheap, rest) = removeWhile f nheap in (nnheap, rem:rest)

-- |
-- /O(log max(n, m))/. The union of two 'Heap's.
union :: (HeapPolicy p a) => Heap p a -> Heap p a -> Heap p a
union h Empty = h
union Empty h = h
union heap1@(Tree _ x l1 r1) heap2@(Tree _ y l2 r2) = if LT == heapCompare heap1 x y
	then makeT x l1 (union r1 heap2) -- keep smallest number on top and merge the other
	else makeT y l2 (union r2 heap1) -- heap into the right branch, it's shorter

-- |
-- Combines a value x and two 'Heaps' to one 'Heap'. Therefore, x has to
-- be less or equal the minima (depending on the 'HeapPolicy') of both
-- 'Heap' parameters. /The precondition is not checked/.
makeT :: a -> Heap p a -> Heap p a -> Heap p a
makeT x a b = let
		ra = rank a
		rb = rank b
	in if ra > rb
		then Tree (rb + 1) x a b
		else Tree (ra + 1) x b a

-- |
-- Builds the union over all given 'Heap's.
unions :: (HeapPolicy p a) => [Heap p a] -> Heap p a
unions = foldl' union empty

-- |
-- Builds a 'Heap' from the given elements. If you have a sorted list,
-- use 'fromAscList', which is much more efficient.
fromList :: (HeapPolicy p a) => [a] -> Heap p a
fromList = unions . (map singleton)

-- |
-- /O(n)/. Lists elements of the 'Heap' in no specific order.
toList :: Heap p a -> [a]
toList Empty = []
toList (Tree _ x a b) = x : toList a ++ toList b

-- |
-- /O(n)/. Lists elements of the 'Heap' in no specific order.
elems :: Heap p a -> [a]
elems = toList

-- |
-- /O(n)/. Creates a 'Heap' from an ascending list. Note that the list
-- has to be ascending corresponding to the 'HeapPolicy', not to it's
-- 'Ord' instance declaration (if there is one).
-- /The precondition is not checked./
fromAscList :: (HeapPolicy p a) => [a] -> Heap p a
fromAscList []     = Empty
fromAscList (x:xs) = Tree 1 x (fromAscList xs) Empty

-- |
-- /O(n)/. Lists elements of the 'Heap' in ascending order (corresponding
-- to the 'HeapPolicy').
toAscList :: (HeapPolicy p a) => Heap p a -> [a]
toAscList Empty          = []
toAscList h@(Tree _ x a b) = x : mergeLists (toAscList a) (toAscList b)
	where	mergeLists [] ys = ys
		mergeLists xs [] = xs
		mergeLists xs@(x:xs') ys@(y:ys') = if LT == heapCompare h x y
	      		then x : mergeLists xs' ys
			else y : mergeLists xs  ys'

-- |
-- Sanity checks for debugging. This includes checking the ranks and
-- the heap and leftist (the left rank is at least the right rank) properties.
check :: (HeapPolicy p a) => Heap p a -> Bool
check Empty = True
check h@(Tree r x left right) = let
		leftRank  = rank left
		rightRank = rank right
	in (isEmpty left || LT /= heapCompare h (findHead left) x)
		&& (isEmpty right || LT /= heapCompare h (findHead right) x)
		&& r == 1 + rightRank
		&& leftRank >= rightRank
		&& check left
		&& check right

