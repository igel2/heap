{-# LANGUAGE CPP, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | 
-- A flexible implementation of min-, max- or custom-priority heaps
-- based on the leftist-heaps from Chris Okasaki's book \"Purely Functional Data
-- Structures\", Cambridge University Press, 1998, chapter 3.1.
--
-- If you need a minimum or maximum heap, use 'MinHeap' resp. 'MaxHeap'. If
-- you want to define a custom order of the heap elements implement a
-- 'HeapPolicy'.
--
-- This module is best imported @qualified@ in order to prevent name clashes
-- with other modules.
module Data.Heap
	(
	-- * Heap type
	  Heap, MinHeap, MaxHeap
	, HeapPolicy(..), MinPolicy, MaxPolicy
	-- * Query
	, null, isEmpty, size, head, tail, extractHead
	-- * Construction
	, empty, singleton, insert
	-- * Union
	, union, unions
	-- * Filter
	, filter, partition
	-- * Subranges
	, take, drop, splitAt
	, takeWhile, span, break
	-- * Conversion
	-- ** List
	, fromList, toList, elems
	-- ** Ordered list
	, fromAscList, toAscList
	-- * Debugging
	, check
	) where

import Data.Foldable (Foldable(foldMap))
import Data.List (foldl')
import Data.Monoid
import Prelude hiding (break, drop, filter, head, null, tail, span, splitAt, take, takeWhile)
import Text.Read

-- |
-- The basic 'Heap' type.
data Heap p a
	= Empty
	| Tree {-# UNPACK #-} !Int a !(Heap p a) !(Heap p a)

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
		where
		compare' [] [] = EQ
		compare' [] _  = LT
		compare' _  [] = GT
		compare' (x:xs) (y:ys) = case heapCompare (policy h1) x y of
			EQ -> compare' xs ys
			c  -> c

instance (HeapPolicy p a) => Monoid (Heap p a) where
	mempty  = empty
	mappend = union
	mconcat = unions

instance Foldable (Heap p) where
	foldMap _ Empty          = mempty
	foldMap f (Tree _ x l r) = foldMap f l `mappend` f x `mappend` foldMap f r

instance (HeapPolicy p a, Read a) => Read (Heap p a) where
#ifdef __GLASGOW_HASKELL__
	readPrec = parens $ prec 10 $ do
		Ident "fromList" <- lexP
		xs <- readPrec
		return (fromList xs)
	readListPrec = readListPrecDefault
#else
	readsPrec p = readParen (p > 10) $ \r -> do
		("fromList", s) <- lex r
		(xs, t) <- reads s
		return (fromList xs, t)
#endif

-- |
-- The 'HeapPolicy' class defines an order on the elements contained within
-- a 'Heap'.
class HeapPolicy p a where
	-- |
	-- Compare two elements, just like 'compare' of the 'Ord' class,
	-- so this function has to define a mathematical ordering.
	-- When using a 'HeapPolicy' for a 'Heap', the minimal value
	-- (defined by this order) will be the 'head' of the 'Heap'.
	heapCompare :: p    -- ^ /Must not be evaluated/.
		-> a        -- ^ Must be compared to 3rd parameter.
		-> a        -- ^ Must be compared to 2nd parameter.
		-> Ordering -- ^ Result of the comparison.

-- |
-- Policy type for a 'MinHeap'.
data MinPolicy

instance (Ord a) => HeapPolicy MinPolicy a where
	heapCompare = const compare

-- |
-- Policy type for a 'MaxHeap'
data MaxPolicy

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
rank Empty          = 0
rank (Tree r _ _ _) = r

-- |
-- Gets the default policy instance for a 'Heap' that can be the first
-- parameter of 'heapCompare'. This function always returns 'undefined'.
policy :: Heap p a -> p
policy = const undefined

-- |
-- /O(n)/. The number of elements in the 'Heap'.
size :: (Num n) => Heap p a -> n
size Empty          = 0
size (Tree _ _ l r) = 1 + size l + size r

-- |
-- /O(1)/. Finds the minimum (depending on the 'HeapPolicy') of the 'Heap'.
head :: (HeapPolicy p a) => Heap p a -> a
head = fst . extractHead

-- |
-- /O(log n)/. Delete the minimum (depending on the 'HeapPolicy')
-- from the 'Heap'.
tail :: (HeapPolicy p a) => Heap p a -> Heap p a
tail = snd . extractHead

-- |
-- /O(log n)/. Find the minimum (depending on the 'HeapPolicy') and
-- delete it from the 'Heap'. This function is undefined for an
-- empty 'Heap'.
extractHead :: (HeapPolicy p a) => Heap p a -> (a, Heap p a)
extractHead Empty          = error "empty Heap"
extractHead (Tree _ x l r) = (x, union l r)

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
-- Take the lowest @n@ elements in ascending order of the
-- 'Heap' (according to the 'HeapPolicy').
take :: (HeapPolicy p a) => Int -> Heap p a -> [a]
take n = fst . (splitAt n)

-- |
-- Remove the lowest (according to the 'HeapPolicy') @n@ elements
-- from the 'Heap'.
drop :: (HeapPolicy p a) => Int -> Heap p a -> Heap p a
drop n = snd . (splitAt n)

-- |
-- @'splitAt' n h@ returns an ascending list of the lowest @n@
-- elements of @h@ (according to its 'HeapPolicy') and a 'Heap'
-- like @h@, lacking those elements.
splitAt :: (HeapPolicy p a) => Int -> Heap p a -> ([a], Heap p a)
splitAt _ Empty     = ([], empty)
splitAt n heap@(Tree _ x l r)
	| n > 0     = let (xs, heap') = splitAt (n-1) (union l r) in (x:xs, heap')
	| otherwise = ([], heap)

-- |
-- @'takeWhile' p h@ lists the longest prefix of elements in ascending
-- order (according to its 'HeapPolicy') of @h@ that satisfy @p@.
takeWhile :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> [a]
takeWhile p = fst . (span p)

-- |
-- @'span' p h@ returns the longest prefix of elements in ascending
-- order (according to its 'HeapPolicy') of @h@ that satisfy @p@ and
-- a 'Heap' like @h@, lacking those elements.
span :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> ([a], Heap p a)
span _ Empty        = ([], empty)
span p heap@(Tree _ x l r)
	| p x       = let (xs, heap') = span p (union l r) in (x:xs, heap')
	| otherwise = ([], heap)
-- |
-- @'break' p h@ returns the longest prefix of elements in ascending
-- order (according to its 'HeapPolicy') of @h@ that do /not/ satisfy @p@
-- and a 'Heap' like @h@, lacking those elements.
break :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> ([a], Heap p a)
break p = span (not . p)

-- |
-- /O(log max(n, m))/. The union of two 'Heap's.
union :: (HeapPolicy p a) => Heap p a -> Heap p a -> Heap p a
union h Empty = h
union Empty h = h
union heap1@(Tree _ x l1 r1) heap2@(Tree _ y l2 r2) = if LT == heapCompare (policy heap1) x y
	then makeT x l1 (union r1 heap2) -- keep smallest number on top and merge the other
	else makeT y l2 (union r2 heap1) -- heap into the right branch, it's shorter

-- |
-- Combines a value @x@ and two 'Heap's to one 'Heap'. Therefore, @x@ has to
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
-- Removes all elements from a given 'Heap' that do not fulfil the
-- predicate.
filter :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> Heap p a
filter p = fst . (partition p)

{-# RULES
	"filter/filter" forall p1 p2 h. filter p2 (filter p1 h) = filter (\x -> p1 x && p2 x) h
  #-}

-- |
-- Partition the 'Heap' into two. @'partition' p h = (h1, h2)@:
-- All elements in @h1@ fulfil the predicate @p@, those in @h2@ don't.
-- @'union' h1 h2 = h@.
partition :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> (Heap p a, Heap p a)
partition _ Empty   = (empty, empty)
partition p (Tree _ x l r)
	| p x       = (makeT x l1 r1, union l2 r2)
	| otherwise = (union l1 r1, makeT x l2 r2)
	where
	(l1, l2) = partition p l
	(r1, r2) = partition p r

-- |
-- Builds a 'Heap' from the given elements.
-- You may want to use 'fromAscList', if you have a sorted list.
fromList :: (HeapPolicy p a) => [a] -> Heap p a
fromList = unions . (map singleton)

-- |
-- /O(n)/. Lists elements of the 'Heap' in no specific order.
toList :: Heap p a -> [a]
toList Empty          = []
toList (Tree _ x l r) = x : toList l ++ toList r

-- |
-- /O(n)/. Lists elements of the 'Heap' in no specific order.
elems :: Heap p a -> [a]
elems = toList

-- |
-- /O(n)/. Creates a 'Heap' from an ascending list. Note that the list
-- has to be ascending corresponding to the 'HeapPolicy', not to its
-- 'Ord' instance declaration (if there is one).
-- /The precondition is not checked/.
fromAscList :: (HeapPolicy p a) => [a] -> Heap p a
--fromAscList []     = empty
--fromAscList (x:xs) = Tree 1 x (fromAscList xs) empty
fromAscList = fromList -- Just as fast, but needs less memory. Why?

-- |
-- /O(n)/. Lists elements of the 'Heap' in ascending order (corresponding
-- to the 'HeapPolicy').
toAscList :: (HeapPolicy p a) => Heap p a -> [a]
toAscList Empty            = []
toAscList h@(Tree _ e l r) = e : mergeLists (toAscList l) (toAscList r)
	where
	mergeLists [] ys = ys
	mergeLists xs [] = xs
	mergeLists xs@(x:xs') ys@(y:ys') = if LT == heapCompare (policy h) x y
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
	in (null left || LT /= heapCompare (policy h) (head left) x) -- heap property
		&& (null right || LT /= heapCompare (policy h) (head right) x) -- dito
		&& r == 1 + rightRank    -- rank == length of right spine
		&& leftRank >= rightRank -- leftist property
		&& check left
		&& check right

