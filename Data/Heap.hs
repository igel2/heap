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
--TODO rename? :
      -- ** Ordering policies
    , HeapItem(..), MinPolicy, MaxPolicy, FstMinPolicy, FstMaxPolicy
      -- * Query
    , I.isEmpty, I.size
#ifdef __DEBUG__
    , rank
#endif
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
      -- by 'fromFoldable' and friends (we have @instance Foldable []@).
    , toList, toAscList, toDescList
    ) where

import Data.Foldable as Foldable ( Foldable )
import qualified Data.Foldable as Foldable ( toList )
import Data.Heap.Item
import Data.Heap.Internal ( Heap )
import qualified Data.Heap.Internal as I
import Prelude hiding
    ( break, drop, dropWhile, filter, span, splitAt, take, takeWhile )

singleton :: (HeapItem pol item) => item -> ManagedHeap pol item
singleton = (uncurry I.singleton) . split

insert :: (HeapItem pol item) => item -> ManagedHeap pol item
       -> ManagedHeap pol item
insert h = I.union (singleton h)

view :: (HeapItem pol item) => ManagedHeap pol item
     -> Maybe (item, ManagedHeap pol item)
view = fmap (\(p, v, h) -> (merge p v, h)) . I.view

viewHead :: (HeapItem pol item) => ManagedHeap pol item -> Maybe item
viewHead = fmap fst . view

viewTail :: (HeapItem pol item) => ManagedHeap pol item
         -> Maybe (ManagedHeap pol item)
viewTail = fmap snd . view

-- TODO: rename: liftXY, morph, whatever
-- TODO: move to D.H.Item
translate :: (HeapItem pol item) => (item -> a) -> Prio pol item -> Val pol item -> a
translate f p v = let x = merge p v in f x
{-# INLINE translate #-}

---- | Removes all priority-value pairs from a 'Heap' not fulfilling a predicate.
filter :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item
       -> ManagedHeap pol item
filter p = fst . (partition p)

--- | Partition the 'Heap' into two. @'partition' p h = (h1, h2)@: All
--- priority-value pairs in @h1@ fulfil the predicate @p@, those in @h2@ don't.
--- @'union' h1 h2 = h@.
partition :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item
          -> (ManagedHeap pol item, ManagedHeap pol item)
partition p = I.partition (translate p)

--- | Take the lowest @n@ priority-value pairs, in ascending order of priority,
--- from the 'Heap'.
take :: (HeapItem pol item) => Int -> ManagedHeap pol item -> [item]
take n = fst . splitAt n

--- | Remove the lowest @n@ priority-value pairs, in ascending order of priority,
--- from the 'Heap'.
drop :: (HeapItem pol item) => Int -> ManagedHeap pol item -> ManagedHeap pol item
drop n = snd . splitAt n

--- | @'splitAt' n h@ returns a list of the lowest @n@ priority-value pairs of @h@,
--- in  ascending order of priority, and @h@, with those elements removed.
splitAt :: (HeapItem pol item) => Int -> ManagedHeap pol item
        -> ([item], ManagedHeap pol item)
splitAt n heap = let (xs, heap') = I.splitAt n heap in (fmap merge2 xs, heap')

--- | @'takeWhile' p h@ lists the longest prefix of priority-value pairs of @h@,
--- in ascending order of priority, that satisfy @p@.
takeWhile :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item -> [item]
takeWhile p = fst . (span p)

--- | @'dropWhile' p h@ removes the longest prefix of priority-value pairs of @h@
--- in ascending order of priority that satisfy @p@.
dropWhile :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item
          -> ManagedHeap pol item
dropWhile p = snd . (span p)

--- | @'span' p h@ returns the longest prefix of priority-value pairs of @h@, in
--- ascending order of priority, that satisfy @p@ and @h@, with those elements
--- removed.
span :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item
     -> ([item], ManagedHeap pol item)
span p heap = let (xs, heap') = I.span (translate p) heap in (fmap merge2 xs, heap')

--- | @'break' p h@ returns the longest prefix of priority-value pairs of @h@, in
--- ascending order of priority, that do /not/ satisfy @p@ and @h@, with those
--- elements removed.
break :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item
      -> ([item], ManagedHeap pol item)
break p = span (not . p)

--- | /O(n log n)/. Builds a 'Heap' from the given priority-value pairs. Assuming
--- you have a sorted 'Foldable', you probably want to use 'fromDescFoldable' or
--- 'fromAscFoldable', they are faster than this function.
fromFoldable :: (Foldable f, Functor f, HeapItem pol item) => f item
             -> ManagedHeap pol item
fromFoldable = I.fromFoldable . fmap split
{-# SPECIALISE fromFoldable :: (HeapItem pol item) => [item] -> ManagedHeap pol item #-}

--- | /O(n)/. Lists all priority-value pairs of the 'Heap' in no specific order.
toList :: (HeapItem pol item) => ManagedHeap pol item -> [item]
toList = fmap merge2 . I.toList

--- | /O(n)/. Creates a 'Heap' from a 'Foldable' providing its priority-value
--- pairs in ascending sorted order of priority. This function is faster than
--- 'fromFoldable' but not as fast as 'fromDescFoldable'.
---
--- /The precondition is not checked/.
fromAscFoldable :: (Foldable f, HeapItem pol item) => f item
                -> ManagedHeap pol item
fromAscFoldable = fromDescFoldable . reverse . Foldable.toList
{-# SPECIALISE fromAscFoldable :: (HeapItem pol item) => [item] -> ManagedHeap pol item #-}

--- | /O(n)/. Lists priority-value pairs of the 'Heap' in ascending order of
--- priority.
toAscList :: (HeapItem pol item) => ManagedHeap pol item -> [item]
toAscList = fmap merge2 . I.toAscList

--- | /O(n)/. Create a 'Heap' from a 'Foldable' providing its priority-value pairs
--- in descending order of priority. Prefer this function over 'fromFoldable' and
--- 'fromAscFoldable', as it is faster.
---
--- /The precondition is not checked/.
fromDescFoldable :: (Foldable f, Functor f, HeapItem pol item) => f item
                 -> ManagedHeap pol item
fromDescFoldable = I.fromDescFoldable . fmap split
{-# SPECIALISE fromDescFoldable :: (HeapItem pol item) => [item] -> ManagedHeap pol item #-}

--- | /O(n)/. Lists the priority-value pairs of the 'Heap' in descending order of
--- priority. Note that this function is not especially efficient (it is
--- implemented as @'reverse' . 'toAscList'@), it is provided as a counterpart of
--- the efficient 'fromDescFoldable' function.
toDescList :: (HeapItem pol item) => ManagedHeap pol item -> [item]
toDescList = reverse . toAscList