{-# LANGUAGE FlexibleContexts #-}

module Test.Heap
    ( runTests
    ) where

import Data.Heap
import Data.List hiding ( null )
import Data.Ord
import Prelude hiding ( null )
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
