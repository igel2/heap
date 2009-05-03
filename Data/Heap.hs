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
    ( -- * Heap flavours
      Heap, MinHeap, MaxHeap, MinPrioHeap, MaxPrioHeap
    , HeapItem(..), MinPolicy, MaxPolicy, FstMinPolicy, FstMaxPolicy
      -- * Construction
    , I.empty, singleton, insert, I.union, I.unions
      -- * Query
    , I.isEmpty, I.size, view, viewHead, viewTail
      -- * Filter
    , filter, partition
      -- * Subranges
    , take, drop, splitAt
    , takeWhile, dropWhile, span, break
      -- * Conversion
      -- ** Foldable
--    , fromFoldable, fromAscFoldable, fromDescFoldable
--      -- ** List
--      -- | Note that there are no @fromList@ functions, because they're implied
--      -- by 'fromFoldable' and friends (we have @instance Foldable []@).
--    , toList, toAscList, toDescList
    ) where

import Data.Heap.Item
import Data.Heap.Internal ( Heap )
import qualified Data.Heap.Internal as I
import Prelude hiding ( splitAt )

singleton :: (HeapItem pol item) => item -> Heap (Prio pol item) (Val pol item)
singleton = (uncurry I.singleton) . split

insert :: (HeapItem pol item) => item -> Heap (Prio pol item) (Val pol item)
       -> Heap (Prio pol item) (Val pol item)
insert h = I.union (singleton h)

view :: (HeapItem pol item) => Heap (Prio pol item) (Val pol item)
     -> Maybe (item, Heap (Prio pol item) (Val pol item))
view = fmap (\(p, v, h) -> (merge p v, h)) . I.view

viewHead :: (HeapItem pol item) => Heap (Prio pol item) (Val pol item)
         -> Maybe item
viewHead = fmap fst . view

viewTail :: (HeapItem pol item) => Heap (Prio pol item) (Val pol item)
         -> Maybe (Heap (Prio pol item) (Val pol item))
viewTail = fmap snd . view

splitAt :: (HeapItem pol item) => Int -> Heap (Prio pol item) (Val pol item)
        -> ([item], Heap (Prio pol item) (Val pol item))
splitAt n heap = let (xs, heap') = I.splitAt n heap in (fmap merge2 xs, heap')

-- TODO: rename: liftXY, morph, whatever
translate :: (HeapItem pol item) => (item -> a) -> Prio pol item -> Val pol item -> a
translate f p v = let x = merge p v in f x
{-# INLINE translate #-}

partition :: (HeapItem pol item) => (item -> Bool) -> Heap (Prio pol item) (Val pol item)
          -> (Heap (Prio pol item) (Val pol item), Heap (Prio pol item) (Val pol item))
partition p = I.partition (translate p)