{-# LANGUAGE FlexibleContexts #-}

module Test.Heap
    ( runTests
    ) where

import Data.Char
import Data.Heap
import Data.List hiding ( break, null, partition, span, splitAt, union )
import Data.Ord
import Prelude hiding ( break, null, span, splitAt )
import Test.Heap.Common
import Test.Heap.Internal hiding ( runTests )
import Test.Heap.Item hiding ( runTests )

runTests :: IO ()
runTests = do
    qc "list conversions for MinHeap" (listProperty :: MinHeap Int -> Bool)
    qc "list conversions for MaxHeap" (listProperty :: MaxHeap Int -> Bool)
    qc "list conversions for MinPrioHeap" (listProperty :: MinPrioHeap Int Char -> Bool)
    qc "list conversions for MaxPrioHeap" (listProperty :: MaxPrioHeap Int Char -> Bool)

    qc "view for MinHeap" (headTailViewProperty :: MinHeap Int -> Bool)
    qc "view for MaxHeap" (headTailViewProperty :: MaxHeap Int -> Bool)
    qc "view for MinPrioHeap" (headTailViewProperty :: MinPrioHeap Int Char -> Bool)
    qc "view for MaxPrioHeap" (headTailViewProperty :: MaxPrioHeap Int Char -> Bool)

    qc "partition for MinHeap" (partitionProperty even :: MinHeap Int -> Bool)
    qc "partition for MaxHeap" (partitionProperty even :: MaxHeap Int -> Bool)
    qc "partition for MinPrioHeap" (partitionProperty testProp :: MinPrioHeap Int Char -> Bool)
    qc "partition for MaxPrioHeap" (partitionProperty testProp :: MaxPrioHeap Int Char -> Bool)

    qc "splitAt for MinHeap" (splitAtProperty :: Int -> MinHeap Int -> Bool)
    qc "splitAt for MaxHeap" (splitAtProperty :: Int -> MaxHeap Int -> Bool)
    qc "splitAt for MinPrioHeap" (splitAtProperty :: Int -> MinPrioHeap Int Char -> Bool)
    qc "splitAt for MaxPrioHeap" (splitAtProperty :: Int -> MaxPrioHeap Int Char -> Bool)

    qc "span for MinHeap" (spanProperty even :: MinHeap Int -> Bool)
    qc "span for MaxHeap" (spanProperty even :: MaxHeap Int -> Bool)
    qc "span for MinPrioHeap" (spanProperty testProp :: MinPrioHeap Int Char -> Bool)
    qc "span for MaxPrioHeap" (spanProperty testProp :: MaxPrioHeap Int Char -> Bool)
    where
    testProp :: (Int, Char) -> Bool
    testProp (i, c) = even i /= isLetter c

listProperty :: (HeapItem pol item, Ord (Val pol item)) => ManagedHeap pol item -> Bool
listProperty heap = let
    pairs = toList heap
    asc   = toAscList heap
    desc  = toDescList heap
    heap2 = fromFoldable pairs
    heap3 = fromAscFoldable asc
    heap4 = fromDescFoldable desc
    in and (fmap leftistHeapProperty [heap2, heap3, heap4])
        && heap == heap2
        && heap == heap3
        && heap == heap4

headTailViewProperty :: (HeapItem pol item, Eq item, Ord (Val pol item))
    => ManagedHeap pol item -> Bool
headTailViewProperty heap = if null heap
    then isEmpty heap
        && Nothing == view heap
        && Nothing == viewHead heap
        && Nothing == viewTail heap
    else case view heap of
        Just (h, heap') -> viewHead heap == Just h && viewTail heap == Just heap'
        Nothing         -> False

partitionProperty :: (HeapItem pol item, Ord (Val pol item))
    => (item -> Bool) -> ManagedHeap pol item -> Bool
partitionProperty p heap = let
    (yes, no) = partition p heap
    in and (fmap p (toList yes))
        && and (fmap (not . p) (toList no))
        && heap == yes `union` no

splitAtProperty :: (HeapItem pol item, Ord (Val pol item))
    => Int -> ManagedHeap pol item -> Bool
splitAtProperty n heap = let
    (before, after) = splitAt n heap
    in n < 0 || length before == n || isEmpty after
        && heap == fromAscFoldable before `union` after

spanProperty :: (HeapItem pol item) => (item -> Bool) -> ManagedHeap pol item -> Bool
spanProperty p heap = let
    (yes, heap') = span p heap
    (no, heap'') = break p heap
    in and (fmap p yes)
        && and (fmap (not . p) no)
        && maybe True (not . p) (viewHead heap')
        && maybe True p (viewHead heap'')
