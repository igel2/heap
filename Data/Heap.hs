{-# LANGUAGE CPP, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | A flexible implementation of min-, max- or custom-priority heaps based on
-- the leftist-heaps from Chris Okasaki's book \"Purely Functional Data
-- Structures\", Cambridge University Press, 1998, chapter 3.1.
--
-- Choose 'MinHeap' or 'MaxHeap' if you need a simple minimum or maximum heap.
-- If you want to manually associate a priority with a value, use 'MinPrioHeap'
-- or 'MaxPrioHeap'. They manage @(priority, value)@ tuples so that only the
-- priority (and not the value) influences the order of elements. If you still
-- need something different, define a custom order for the heap elements by
-- implementing a 'HeapPolicy' and let the maintainer know, what's missing.
--
-- This module is best imported @qualified@ in order to prevent name clashes
-- with other modules.
module Data.Heap
  ( -- * Heap type
    Heap, MinHeap, MaxHeap, MinPrioHeap, MaxPrioHeap
  , HeapPolicy(..), MinPolicy, MaxPolicy, FstMinPolicy, FstMaxPolicy
    -- * Query
  , null, isEmpty, size, head, tail, view, extractHead
    -- * Construction
  , empty, singleton, insert
    -- * Union
  , union, unions
    -- * Filter
  , filter, partition
    -- * Subranges
  , take, drop, splitAt
  , takeWhile, dropWhile, span, break
    -- * Conversion
    -- ** List
  , fromList, toList, elems
    -- ** Ordered list
  , fromAscList, toAscList
    -- * Debugging
  , check
  ) where

import Data.Foldable ( foldl', Foldable(foldMap) )
import qualified Data.Foldable as Foldable ( toList )
import Data.Monoid
import Data.Ord
import Prelude hiding ( break, drop, dropWhile, filter, head, null, tail, span
                      , splitAt, take, takeWhile )
import Text.Read

-- | The basic 'Heap' type.
data Heap p a
  = Empty
  | Tree {-# UNPACK #-} !Int a !(Heap p a) !(Heap p a)

-- | A 'Heap' which will always extract the minimum first.
type MinHeap a = Heap MinPolicy a

-- | A 'Heap' with inverted order: The maximum will be extracted first.
type MaxHeap a = Heap MaxPolicy a

-- | A 'Heap' storing priority-value-associations. It only regards the priority
-- for determining the order of elements, the tuple with minimal 'fst' value
-- (i. e. priority) will always be the head of the 'Heap'.
type MinPrioHeap priority value = Heap FstMinPolicy (priority, value)

-- | A 'Heap' storing priority-value-associations. It only regards the priority
-- for determining the order of elements, the tuple with maximal 'fst' value
-- (i. e. priority) will always be the head of the 'Heap'.
type MaxPrioHeap priority value = Heap FstMaxPolicy (priority, value)

instance (Show a) => Show (Heap p a) where
  show = ("fromList " ++) . show . toList

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
    xs               <- readPrec
    return (fromList xs)
  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \r -> do
    ("fromList", s) <- lex r
    (xs, t)         <- reads s
    return (fromList xs, t)
#endif

-- | The 'HeapPolicy' class defines an order on the elements contained within
-- a 'Heap'.
class HeapPolicy p a where
  -- | Compare two elements, just like 'compare' of the 'Ord' class, so this
  -- function has to define a mathematical ordering. When using a 'HeapPolicy'
  -- for a 'Heap', the minimal value (defined by this order) will be the head
  -- of the 'Heap'.
  heapCompare :: p -- ^ /Must not be evaluated/.
    -> a           -- ^ Must be compared to 3rd parameter.
    -> a           -- ^ Must be compared to 2nd parameter.
    -> Ordering    -- ^ Result of the comparison.

-- | Policy type for a 'MinHeap'.
data MinPolicy

instance (Ord a) => HeapPolicy MinPolicy a where
  heapCompare = const compare

-- | Policy type for a 'MaxHeap'.
data MaxPolicy

instance (Ord a) => HeapPolicy MaxPolicy a where
  heapCompare = const (flip compare)

-- | Policy type for a @(priority, value)@ 'MinPrioHeap'.
data FstMinPolicy

instance (Ord priority) => HeapPolicy FstMinPolicy (priority, value) where
  heapCompare = const (comparing fst)

-- | Policy type for a @(priority, value)@ 'MaxPrioHeap'.
data FstMaxPolicy

instance (Ord priority) => HeapPolicy FstMaxPolicy (priority, value) where
  heapCompare = const (flip (comparing fst))

-- | /O(1)/. Is the 'Heap' empty?
null :: Heap p a -> Bool
null Empty = True
null _     = False

-- | /O(1)/. Is the 'Heap' empty?
isEmpty :: Heap p a -> Bool
isEmpty = null

-- | /O(1)/. Calculate the rank of a 'Heap'.
rank :: Heap p a -> Int
rank Empty          = 0
rank (Tree r _ _ _) = r

-- | Gets the default policy instance for a 'Heap' that can be the first
-- parameter of 'heapCompare'. This function always returns 'undefined'.
policy :: Heap p a -> p
policy = undefined

-- | /O(n)/. The number of elements in the 'Heap'.
size :: (Num n) => Heap p a -> n
size Empty          = 0
size (Tree _ _ l r) = 1 + size l + size r

-- | /Deprecated/. Please use the 'view' function instead, it's not partial.
head :: (HeapPolicy p a) => Heap p a -> a
head = fst . extractHead

-- | /Deprecated/. Please use the 'view' function instead, it's not partial.
tail :: (HeapPolicy p a) => Heap p a -> Heap p a
tail = snd . extractHead

-- | /O(log n)/ for the tail, /O(1)/ for the head. Find the minimum (depending
-- on the 'HeapPolicy') and delete it from the 'Heap' (i. e. find head and tail
-- of a heap) if it is not empty. Otherwise, 'Nothing' is returned.
view :: (HeapPolicy p a) => Heap p a -> Maybe (a, Heap p a)
view Empty          = Nothing
view (Tree _ x l r) = Just (x, union l r)

{-# INLINE view #-}

-- | /Deprecated/. Please use the 'view' function instead, it's not partial.
extractHead :: (HeapPolicy p a) => Heap p a -> (a, Heap p a)
extractHead heap = maybe (error "empty heap") id (view heap)

{-# DEPRECATED head, tail, extractHead
  "Please use the view function instead, it's not partial"
  #-}

-- | /O(1)/. Constructs an empty 'Heap'.
empty :: Heap p a
empty = Empty

-- | /O(1)/. Create a singleton 'Heap'.
singleton :: a -> Heap p a
singleton x = Tree 1 x empty empty

-- | /O(log n)/. Insert an element in the 'Heap'.
insert :: (HeapPolicy p a) => a -> Heap p a -> Heap p a
insert x h = union h (singleton x)

-- | Take the lowest @n@ elements in ascending order of the 'Heap' (according
-- to the 'HeapPolicy').
take :: (HeapPolicy p a) => Int -> Heap p a -> [a]
take n = fst . (splitAt n)

-- | Remove the lowest (according to the 'HeapPolicy') @n@ elements
-- from the 'Heap'.
drop :: (HeapPolicy p a) => Int -> Heap p a -> Heap p a
drop n = snd . (splitAt n)

-- | @'splitAt' n h@ returns an ascending list of the lowest @n@ elements of @h@
-- (according to its 'HeapPolicy') and a 'Heap' like @h@, lacking those elements.
splitAt :: (HeapPolicy p a) => Int -> Heap p a -> ([a], Heap p a)
splitAt n heap
  | n > 0     = case view heap of
    Nothing      -> ([], empty)
    Just (h, hs) -> let (xs, heap') = splitAt (n-1) hs in (h:xs, heap')
  | otherwise = ([], heap)

-- | @'takeWhile' p h@ lists the longest prefix of elements in ascending order
-- (according to its 'HeapPolicy') of @h@ that satisfy @p@.
takeWhile :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> [a]
takeWhile p = fst . (span p)

-- | @'dropWhile' p h@ removes the longest prefix of elements from @h@ that
-- satisfy @p@.
dropWhile :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> Heap p a
dropWhile p = snd . (span p)

-- | @'span' p h@ returns the longest prefix of elements in ascending order
-- (according to its 'HeapPolicy') of @h@ that satisfy @p@ and a 'Heap' like
-- @h@, with those elements removed.
span :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> ([a], Heap p a)
span p heap = case view heap of
  Nothing      -> ([], empty)
  Just (h, hs) -> if p h
    then let (xs, heap') = span p hs in (h:xs, heap')
    else ([], heap)

-- | @'break' p h@ returns the longest prefix of elements in ascending order
-- (according to its 'HeapPolicy') of @h@ that do /not/ satisfy @p@ and a 'Heap'
-- like @h@, with those elements removed.
break :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> ([a], Heap p a)
break p = span (not . p)

-- | /O(log max(n, m))/. The union of two 'Heap's.
union :: (HeapPolicy p a) => Heap p a -> Heap p a -> Heap p a
union h Empty = h
union Empty h = h
union heap1@(Tree _ x l1 r1) heap2@(Tree _ y l2 r2) =
  if LT == heapCompare (policy heap1) x y
    then makeT x l1 (union r1 heap2) -- keep smallest number on top and merge the other
    else makeT y l2 (union r2 heap1) -- heap into the right branch, it's shorter

-- | Combines a value @x@ and two 'Heap's to one 'Heap'. Therefore, @x@ has to
-- be less or equal the minima (depending on the 'HeapPolicy') of both 'Heap'
-- parameters. /The precondition is not checked/.
makeT :: a -> Heap p a -> Heap p a -> Heap p a
makeT x a b = let
  ra = rank a
  rb = rank b
  in if ra > rb
    then Tree (rb + 1) x a b
    else Tree (ra + 1) x b a

-- | Builds the union over all given 'Heap's.
unions :: (HeapPolicy p a) => [Heap p a] -> Heap p a
unions = foldl' union empty

-- | Removes all elements from a given 'Heap' that do not fulfil the predicate.
filter :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> Heap p a
filter p = fst . (partition p)

{-# RULES
  "filter/filter" forall p1 p2 h. filter p2 (filter p1 h) = filter (\x -> p1 x && p2 x) h
  #-}

-- | Partition the 'Heap' into two. @'partition' p h = (h1, h2)@: All elements
-- in @h1@ fulfil the predicate @p@, those in @h2@ don't. @'union' h1 h2 = h@.
partition :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> (Heap p a, Heap p a)
partition _ Empty = (empty, empty)
partition p (Tree _ x l r)
  | p x       = (makeT x l1 r1, union l2 r2)
  | otherwise = (union l1 r1, makeT x l2 r2)
  where
  (l1, l2) = partition p l
  (r1, r2) = partition p r

-- | Builds a 'Heap' from the given elements. You may want to use 'fromAscList',
-- if you have a sorted list.
fromList :: (HeapPolicy p a) => [a] -> Heap p a
fromList = unions . (map singleton)

-- | /O(n)/. Lists elements of the 'Heap' in no specific order.
toList :: Heap p a -> [a]
toList = Foldable.toList

-- | /O(n)/. Lists elements of the 'Heap' in no specific order.
elems :: Heap p a -> [a]
elems = toList

-- | /O(n)/. Creates a 'Heap' from an ascending list. Note that the list has to
-- be ascending corresponding to the 'HeapPolicy', not to its 'Ord' instance
-- declaration (if there is one). /The precondition is not checked/.
fromAscList :: (HeapPolicy p a) => [a] -> Heap p a
fromAscList = fromList

-- | /O(n)/. Lists elements of the 'Heap' in ascending order (corresponding to
-- the 'HeapPolicy').
toAscList :: (HeapPolicy p a) => Heap p a -> [a]
toAscList = takeWhile (const True)

-- | Sanity checks for debugging. This includes checking the ranks and the heap
-- and leftist (the left rank is at least the right rank) properties.
check :: (HeapPolicy p a) => Heap p a -> Bool
check Empty                   = True
check h@(Tree r x left right) = let
  leftRank  = rank left
  rightRank = rank right
  in
  (maybe True (\(lHead, _) -> LT /= heapCompare (policy h) lHead x) (view left))
    && (maybe True (\(rHead, _) -> LT /= heapCompare (policy h) rHead x) (view right))
    && r == 1 + rightRank    -- rank == length of right spine
    && leftRank >= rightRank -- leftist property
    && check left && check right

