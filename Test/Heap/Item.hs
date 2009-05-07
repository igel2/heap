{-# LANGUAGE FlexibleContexts #-}

module Test.Heap.Item
    ( runTests
    ) where

import Data.Heap.Item
import Test.Heap.Common
import Test.QuickCheck

runTests :: IO ()
runTests = do
    qc "Eq property for MinPolicy" (eqProperty
        :: Prio MinPolicy Int -> Prio MinPolicy Int -> Prio MinPolicy Int -> Bool)
    qc "Eq property for MaxPolicy" (eqProperty
        :: Prio MaxPolicy Int -> Prio MaxPolicy Int -> Prio MaxPolicy Int -> Bool)
    qc "Eq property for FstMinPolicy" (eqProperty
        :: Prio FstMinPolicy (Int, Char) -> Prio FstMinPolicy (Int, Char)
        -> Prio FstMinPolicy (Int, Char) -> Bool)
    qc "Eq property for FstMaxPolicy" (eqProperty
        :: Prio FstMaxPolicy (Int, Char) -> Prio FstMaxPolicy (Int, Char)
        -> Prio FstMaxPolicy (Int, Char) -> Bool)
    qc "Ord property for MinPolicy" (ordProperty
        :: Prio MinPolicy Int -> Prio MinPolicy Int -> Prio MinPolicy Int -> Bool)
    qc "Ord property for MaxPolicy" (ordProperty
        :: Prio MaxPolicy Int -> Prio MaxPolicy Int -> Prio MaxPolicy Int -> Bool)
    qc "Ord property for FstMinPolicy" (ordProperty
        :: Prio FstMinPolicy (Int, Char) -> Prio FstMinPolicy (Int, Char)
        -> Prio FstMinPolicy (Int, Char) -> Bool)
    qc "Ord property for FstMaxPolicy" (ordProperty
        :: Prio FstMaxPolicy (Int, Char) -> Prio FstMaxPolicy (Int, Char)
        -> Prio FstMaxPolicy (Int, Char) -> Bool)

    qc "read/show property for MinPolicy" (readShowProperty
        :: Prio MinPolicy Int -> Bool)
    qc "read/show property for MaxPolicy" (readShowProperty
        :: Prio MaxPolicy Int -> Bool)
    qc "read/show property for FstMinPolicy" (readShowProperty
        :: Prio FstMinPolicy (Int, Char) -> Bool)
    qc "read/show property for FstMaxPolicy" (readShowProperty
        :: Prio FstMaxPolicy (Int, Char) -> Bool)

    qc "Binary property for MinPolicy" (binaryProperty
        :: Prio MinPolicy Int -> Bool)
    qc "Binary property for MaxPolicy" (binaryProperty
        :: Prio MaxPolicy Int -> Bool)
    qc "Binary property for FstMinPolicy" (binaryProperty
        :: Prio FstMinPolicy (Int, Char) -> Bool)
    qc "Binary property for FstMaxPolicy" (binaryProperty
        :: Prio FstMaxPolicy (Int, Char) -> Bool)

    qc "split/merge for MinPolicy" (splitMergeProperty
        :: Prio MinPolicy Int -> Val MinPolicy Int -> Bool)
    qc "split/merge for MaxPolicy" (splitMergeProperty
        :: Prio MaxPolicy Int -> Val MaxPolicy Int -> Bool)
    qc "split/merge for FstMinPolicy" (splitMergeProperty
        :: Prio FstMinPolicy (Int, Char) -> Val FstMinPolicy (Int, Char) -> Bool)
    qc "split/merge for FstMaxPolicy" (splitMergeProperty
        :: Prio FstMaxPolicy (Int, Char) -> Val FstMaxPolicy (Int, Char) -> Bool)

    qc "priority/value property for MinPolicy" (priorityValueProperty succ
        :: Prio MinPolicy Int -> Val MinPolicy Int -> Bool)
    qc "priority/value property for MaxPolicy" (priorityValueProperty succ
        :: Prio MaxPolicy Int -> Val MaxPolicy Int -> Bool)
    qc "priority/value property for FstMinPolicy" (priorityValueProperty succ
        :: Prio FstMinPolicy (Int, Char) -> Val FstMinPolicy (Int, Char) -> Bool)
    qc "priority/value property for FstMaxPolicy" (priorityValueProperty succ
        :: Prio FstMaxPolicy (Int, Char) -> Val FstMaxPolicy (Int, Char) -> Bool)

    qc "splitF property for MinPolicy" (splitFProperty (\x -> 4 * x - 7)
        :: Prio MinPolicy Int -> Val MinPolicy Int -> Bool)
    qc "splitF property for MaxPolicy" (splitFProperty (\x -> 4 * x - 7)
        :: Prio MaxPolicy Int -> Val MaxPolicy Int -> Bool)
    qc "splitF property for FstMinPolicy" (splitFProperty (\(x, y) -> (pred x, succ y))
        :: Prio FstMinPolicy (Int, Char) -> Val FstMinPolicy (Int, Char) -> Bool)
    qc "splitF property for FstMaxPolicy" (splitFProperty (\(x, y) -> (pred x, succ y))
        :: Prio FstMaxPolicy (Int, Char) -> Val FstMaxPolicy (Int, Char) -> Bool)

instance (Arbitrary item, HeapItem pol item) => Arbitrary (Prio pol item) where
    arbitrary = fmap (fst . split) arbitrary

splitMergeProperty :: (HeapItem pol item, Eq (Prio pol item), Eq (Val pol item))
    => Prio pol item -> Val pol item -> Bool
splitMergeProperty p v = (p, v) == split (merge (p, v))

priorityValueProperty :: (HeapItem pol item, Eq (Prio pol item))
    => (Val pol item -> Val pol item) -> Prio pol item -> Val pol item -> Bool
priorityValueProperty f p v = p == fst (split (merge (p, v)))
    && p == fst (split (merge (p, f v)))

splitFProperty :: (HeapItem pol item, Eq a)
    => (item -> a) -> Prio pol item -> Val pol item -> Bool
splitFProperty f p v = f (merge (p, v)) == splitF f p v
