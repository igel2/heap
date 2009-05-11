{-# LANGUAGE FlexibleContexts #-}

module Test.Heap
    ( runTests
    ) where

import Data.Heap
import Data.List
import Data.Ord
import Test.Heap.Common
import Test.Heap.Internal hiding ( runTests )
import Test.Heap.Item hiding ( runTests )

runTests :: IO ()
runTests = do
    qc "list conversions MinHeap" (listProperty :: MinHeap Int -> Bool)
    qc "list conversions MaxHeap" (listProperty :: MaxHeap Int -> Bool)
    qc "list conversions MinPrioHeap" (listProperty :: MinPrioHeap Int Char -> Bool)
    qc "list conversions MaxPrioHeap" (listProperty :: MaxPrioHeap Int Char -> Bool)

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
