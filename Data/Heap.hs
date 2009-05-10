--{-# LANGUAGE CPP, DeriveDataTypeable, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
--
---- | A flexible implementation of min-, max- and custom-priority heaps based on
---- the leftist-heaps from Chris Okasaki's book \"Purely Functional Data
---- Structures\", Cambridge University Press, 1998, chapter 3.1.
----
---- There are different flavours of 'Heap's, each of them following a different
---- strategy when ordering its elements:
----
----  * Choose 'MinHeap' or 'MaxHeap' if you need a simple minimum or maximum heap
----    (which always keeps the minimum/maximum element at the head of the 'Heap').
----
----  * If you wish to manually annotate a value with a priority, e. g. an @IO ()@
----    action with an 'Int' use 'MinPrioHeap' or 'MaxPrioHeap'. They manage
----    @(priority, value)@ tuples so that only the priority (and not the value)
----    influences the order of elements.
----
----  * If you still need something different, define a custom order for the heap
----    elements by implementing a 'HeapPolicy' and let the maintainer know,
----    what's missing.
module Data.Heap
    ( -- * Types
      -- ** Various heap flavours
      Heap, ManagedHeap
    , MinHeap, MaxHeap, MinPrioHeap, MaxPrioHeap
      -- ** Ordering strategies
    , HeapItem(..), MinPolicy, MaxPolicy, FstMinPolicy, FstMaxPolicy
      -- * Query
    , I.isEmpty, null, I.size
      -- * Construction
    , I.empty, singleton, insert, I.union, I.unions
      -- * Deconstruction
    , view, viewHead, viewTail
      -- * Filter
    , filter, partition
      -- * Subranges
    , take, drop, splitAt
    , takeWhile, dropWhile, span, break
      -- * Conversion
      -- ** Foldable
    , fromFoldable, fromAscFoldable, fromDescFoldable
      -- ** List
      -- | Note that there are no @fromList@ functions, because they're implied
      -- by 'fromFoldable' and friends (@instance Foldable []@).
    , toList, toAscList, toDescList
    ) where

import Data.Foldable as Foldable ( Foldable )
import qualified Data.Foldable as Foldable ( toList )
import Data.Heap.Item
import Data.Heap.Internal ( Heap )
import qualified Data.Heap.Internal as I
import Prelude hiding
    ( break, drop, dropWhile, filter, null, span, splitAt, take, takeWhile )

-- | /O(1)/. Is the 'Heap' empty?
null :: Heap prio val -> Bool
null = I.isEmpty

-- | /O(1)/. Create a singleton 'Heap'.
singleton :: (HeapItem pol item) => item -> ManagedHeap pol item
singleton = (uncurry I.singleton) . split

-- | /O(log n)/. Insert a single item into the 'Heap'.
insert :: (HeapItem pol item) => item -> ManagedHeap pol item
       -> ManagedHeap pol item
insert = I.union . singleton

-- | /O(1)/ for the head, /O(log n)/ for the tail. Find the item with minimal
-- associated priority and remove it from the 'Heap' (i. e. find head and tail
-- of the heap) if it is not empty. Otherwise, 'Nothing' is returned.
view :: (HeapItem pol item) => ManagedHeap pol item
     -> Maybe (item, ManagedHeap pol item)
view = fmap (\(p, v, h) -> (merge (p, v), h)) . I.view

-- | /O(1)/. Find the item with minimal associated priority on the 'Heap' (i. e.
-- its head) if it is not empty. Otherwise, 'Nothing' is returned.
viewHead :: (HeapItem pol item) => ManagedHeap pol item -> Maybe item
viewHead = fmap fst . view

-- | /O(log n)/. Remove the item with minimal associated priority and from the
-- 'Heap' (i. e. its tail) if it is not empty. Otherwise, 'Nothing' is returned.
viewTail :: (HeapItem pol item) => ManagedHeap pol item
         -> Maybe (ManagedHeap pol item)
viewTail = fmap snd . view

-- | Remove all items from a 'Heap' not fulfilling a predicate.
filter :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item
       -> ManagedHeap pol item
filter p = fst . (partition p)

-- | Partition the 'Heap' into two. @'partition' p h = (h1, h2)@: All items in
-- @h1@ fulfil the predicate @p@, those in @h2@ don't. @'union' h1 h2 = h@.
partition :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item
          -> (ManagedHeap pol item, ManagedHeap pol item)
partition = I.partition . splitF

-- | Take the first @n@ items from the 'Heap'.
take :: (HeapItem pol item) => Int -> ManagedHeap pol item -> [item]
take n = fst . splitAt n

-- | Remove first @n@ items from the 'Heap'.
drop :: (HeapItem pol item) => Int -> ManagedHeap pol item -> ManagedHeap pol item
drop n = snd . splitAt n

-- | @'splitAt' n h@: Return a list of the first @n@ items of @h@ and @h@, with
-- those elements removed.
splitAt :: (HeapItem pol item) => Int -> ManagedHeap pol item
        -> ([item], ManagedHeap pol item)
splitAt n heap = let (xs, heap') = I.splitAt n heap in (fmap merge xs, heap')

-- | @'takeWhile' p h@: List the longest prefix of items in @h@ that satisfy @p@.
takeWhile :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item -> [item]
takeWhile p = fst . (span p)

-- | @'dropWhile' p h@: Remove the longest prefix of items in @h@ that satisfy
-- @p@.
dropWhile :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item
          -> ManagedHeap pol item
dropWhile p = snd . (span p)

-- | @'span' p h@: Return the longest prefix of items in @h@ that satisfy @p@ and
-- @h@, with those elements removed.
span :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item
     -> ([item], ManagedHeap pol item)
span p heap = let (xs, heap') = I.span (splitF p) heap in (fmap merge xs, heap')

-- | @'break' p h@: The longest prefix of items in @h@ that do /not/ satisfy @p@
-- and @h@, with those elements removed.
break :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item
      -> ([item], ManagedHeap pol item)
break p = span (not . p)

-- | /O(n log n)/. Build a 'Heap' from the given items. Assuming you have a
-- sorted 'Foldable', you probably want to use 'fromDescFoldable' or
-- 'fromAscFoldable', they are faster than this function.
fromFoldable :: (Foldable f, Functor f, HeapItem pol item) => f item
             -> ManagedHeap pol item
fromFoldable = I.fromFoldable . fmap split
{-# SPECIALISE fromFoldable :: (HeapItem pol item) => [item] -> ManagedHeap pol item #-}

-- | /O(n)/. Create a 'Heap' from a 'Foldable' providing its items in ascending
-- order of priority (i. e. in the same order they will be removed from the
-- 'Heap'). This function is faster than 'fromFoldable' but not as fast as
-- 'fromDescFoldable'.
--
-- /The precondition is not checked/.
fromAscFoldable :: (Foldable f, HeapItem pol item) => f item
                -> ManagedHeap pol item
fromAscFoldable = fromDescFoldable . reverse . Foldable.toList
{-# SPECIALISE fromAscFoldable :: (HeapItem pol item) => [item] -> ManagedHeap pol item #-}

-- | /O(n)/. Create a 'Heap' from a 'Foldable' providing its items in descending
-- order of priority (i. e. they will be removed inversely from the 'Heap').
-- Prefer this function over 'fromFoldable' and 'fromAscFoldable', as it's faster.
--
-- /The precondition is not checked/.
fromDescFoldable :: (Foldable f, Functor f, HeapItem pol item) => f item
                 -> ManagedHeap pol item
fromDescFoldable = I.fromDescFoldable . fmap split
{-# SPECIALISE fromDescFoldable :: (HeapItem pol item) => [item] -> ManagedHeap pol item #-}

-- | /O(n log n)/. List all items of the 'Heap' in no specific order.
toList :: (HeapItem pol item) => ManagedHeap pol item -> [item]
toList = fmap merge . I.toList

-- | /O(n log n)/. List the items of the 'Heap' in ascending order of priority.
toAscList :: (HeapItem pol item) => ManagedHeap pol item -> [item]
toAscList = fmap merge . I.toAscList

-- | /O(n log n)/. List the items of the 'Heap' in descending order of priority.
-- Note that this function is not especially efficient (it is implemented in
-- terms of 'reverse' and 'toAscList'), it is provided as a counterpart of the
-- efficient 'fromDescFoldable' function.
toDescList :: (HeapItem pol item) => ManagedHeap pol item -> [item]
toDescList = reverse . toAscList