module Test.Heap
    ( testHeap
    ) where

import Data.Heap as Heap
import Data.List as List
import Test.QuickCheck

testHeap :: IO ()
testHeap = do
    putStr "Leftist property of MinHeap Int: "
    quickCheck (leftistHeapProperty :: MinHeap Int -> Bool)
    putStr "Leftist property of MaxHeap Int: "
    quickCheck (leftistHeapProperty :: MaxHeap Int -> Bool)
    putStr "Size property:                   "
    quickCheck sizeProperty
    putStr "Order property:                  "
    quickCheck orderProperty
    putStr "head/tail property:              "
    quickCheck headTailProperty
    putStr "take/drop/splitAt                "
    quickCheck (takeDropSplitAtProperty :: Int -> MinHeap Int -> Bool)
    putStr "takeWhile/span/break             "
    quickCheck takeWhileSpanBreakProperty
    putStr "read . show === id               "
    quickCheck (readShowProperty :: MinHeap Int -> Bool)
    putStr "fromList vs. fromAscList         "
    quickCheck (fromListProperty :: [Int] -> Bool)
    putStr "toList === elems                 "
    quickCheck (toListProperty :: MaxHeap Int -> Bool)
    putStr "partition and filter             "
    quickCheck (partitionFilterProperty (\x -> x `mod` 2 == 0) :: MinHeap Int -> Bool)
    putStr "ordering property                "
    quickCheck (orderingProperty :: MinHeap Int -> MinHeap Int -> Bool)

instance (Arbitrary a, HeapPolicy p a) => Arbitrary (Heap p a) where
    arbitrary = do
        len  <- choose (0, 100)
        list <- vector len
        return (Heap.fromList list)

leftistHeapProperty :: (HeapPolicy p a) => Heap p a -> Bool
leftistHeapProperty Empty                     = True
leftistHeapProperty h@(Tree r s x left right) = let
    leftRank  = rank left
    rightRank = rank right
    in
    (maybe True (\(lHead, _) -> LT /= heapCompare (policy h) lHead x) (view left))
        && (maybe True (\(rHead, _) -> LT /= heapCompare (policy h) rHead x) (view right))
        && r == 1 + rightRank              -- rank == length of right spine
        && leftRank >= rightRank           -- leftist property
        && s == 1 + size left + size right -- check size
        && leftistHeapProperty left
        && leftistHeapProperty right

sizeProperty :: Int -> Bool
sizeProperty n = let
    n' = abs n `mod` 100
    h  = Heap.fromList [1..n'] :: MaxHeap Int
    in
    Heap.size h == n' && (if n' == 0 then Heap.isEmpty h && Heap.null h else True)

orderProperty :: Int -> [Int] -> Bool
orderProperty n list = let
    n'          = signum n * (n `mod` 100)
    heap        = Heap.fromList list :: MaxHeap Int
    (a,  b)     = List.splitAt n' (sortBy (heapCompare (policy heap)) list)
    (a', heap') = Heap.splitAt n' heap
    in
    (Heap.fromList b == heap') && equal heap a a'
    where
    equal _ [] [] = True
    equal _ _  [] = False
    equal _ [] _  = False
    equal h (x:xs) (y:ys) = EQ == heapCompare (policy h) x y && equal h xs ys

headTailProperty :: [Int] -> Bool
headTailProperty []   = True
headTailProperty list = let
    heap  = fromList list :: MaxHeap Int
    list' = sortBy (heapCompare (policy heap)) list
    in case view heap of
        Nothing      -> False -- list is not empty
        Just (h, hs) -> h == List.head list' && hs == (fromAscList (List.tail list'))

takeDropSplitAtProperty :: (Ord a) => Int -> MinHeap a -> Bool
takeDropSplitAtProperty n heap = let
    n'           = signum n * (n `mod` 100)
    (begin, end) = Heap.splitAt n heap
    begin'       = Heap.take n heap
    end'         = Heap.drop n heap
    in
    begin == begin' && end == end'

takeWhileSpanBreakProperty :: Int -> Int -> Bool
takeWhileSpanBreakProperty len index = let
    length'      = abs (len `mod` 100)
    index'       = abs (index `mod` 100)
    xs           = [1..(max length' index')]
    heap         = Heap.fromAscList xs :: MinHeap Int
    p1 x         = x <= index'
    p2 x         = x > index'
    (xs', heap') = Heap.span p1 heap
    in
    xs' == Heap.takeWhile p1 heap
        && heap' == Heap.dropWhile p1 heap
        && (xs', heap') == Heap.break p2 heap

readShowProperty :: (HeapPolicy p a, Show a, Read a) => Heap p a -> Bool
readShowProperty heap = heap == read (show heap)

fromListProperty :: [Int] -> Bool
fromListProperty xs = let
    xs' = sort xs
    in
    (fromList xs' :: MinHeap Int) == (fromAscList xs' :: MinHeap Int)

toListProperty :: (HeapPolicy p a, Eq a) => Heap p a -> Bool
toListProperty heap = toList heap == elems heap

partitionFilterProperty :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> Bool
partitionFilterProperty p heap = let
    (yes,  no)  = Heap.partition p heap
    (yes', no') = List.partition p (toList heap)
    in
    yes == fromList yes'
        && no == fromList no'
        && (Heap.filter p heap) == fromList yes'

orderingProperty :: (Ord a) => MinHeap a -> MinHeap a -> Bool
orderingProperty heap1 heap2 = let
    list1 = toAscList heap1
    list2 = toAscList heap2
    in
    compare heap1 heap2 == compare list1 list2

