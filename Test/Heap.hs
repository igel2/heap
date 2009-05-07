module Test.Heap
    ( runTests
    ) where

runTests :: IO ()
runTests = return ()
{-
import Data.Binary
import Data.Heap as Heap
import Data.List as List
import Data.Monoid
import Test.QuickCheck

testHeap :: IO ()
testHeap = do
    qc "Leftist property of MinHeap Int" (leftistHeapProperty :: MinHeap Int -> Bool)
    qc "Leftist property of MaxHeap Int" (leftistHeapProperty :: MaxHeap Int -> Bool)
    qc "read . show === id" (readShowProperty :: MinHeap Int -> Bool)
    qc "decode . encode === id" (binaryProperty :: MinHeap Int -> Bool)
    qc "monoid property" (monoidProperty :: MinHeap Int -> MinHeap Int -> MinHeap Int -> Bool)
    qc "Size property" sizeProperty
    qc "Order property" orderProperty
    qc "head/tail property" headTailProperty
    qc "take/drop/splitAt" (takeDropSplitAtProperty :: Int -> MinHeap Int -> Bool)
    qc "takeWhile/span/break" takeWhileSpanBreakProperty
    qc "from{,Asc,Desc}Foldable, to{,Asc,Desc}List" (listProperty :: [Int] -> Bool)
    qc "partition and filter" (partitionFilterProperty testProperty :: MinHeap Int -> Bool)
    qc "ordering property" (orderingProperty :: MinHeap Int -> MinHeap Int -> Bool)
    where
    testProperty x = x `mod` 2 == 0

orderProperty :: Int -> [Int] -> Bool
orderProperty n list = let
    n'          = signum n * (n `mod` 100)
    heap        = fromFoldable list :: MaxHeap Int
    (a,  b)     = List.splitAt n' (sortBy (heapCompare (policy heap)) list)
    (a', heap') = Heap.splitAt n' heap
    in
    (fromFoldable b == heap') && equal heap a a'
    where
    equal _ [] [] = True
    equal _ _  [] = False
    equal _ [] _  = False
    equal h (x:xs) (y:ys) = EQ == heapCompare (policy h) x y && equal h xs ys

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
    heap         = Heap.fromAscFoldable xs :: MinHeap Int
    p1 x         = x <= index'
    p2 x         = x > index'
    (xs', heap') = Heap.span p1 heap
    in
    xs' == Heap.takeWhile p1 heap
        && heap' == Heap.dropWhile p1 heap
        && (xs', heap') == Heap.break p2 heap

listProperty :: [Int] -> Bool
listProperty xs = let
    xsAsc  = sort xs
    xsDesc = reverse xsAsc
    h1     = fromFoldable xs         :: MinHeap Int
    h2     = fromAscFoldable xsAsc   :: MinHeap Int
    h3     = fromDescFoldable xsDesc :: MinHeap Int
    in
    (h1 == h2) && (h2 == h3)
        && (and (map leftistHeapProperty [h1, h2, h3]))
        && (and (map ((== xsAsc) . toAscList) [h1, h2, h3]))
        && (and (map ((== xsDesc) . toDescList) [h1, h2, h3]))

partitionFilterProperty :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> Bool
partitionFilterProperty p heap = let
    (yes,  no)  = Heap.partition p heap
    (yes', no') = List.partition p (toList heap)
    in
    yes == fromFoldable yes'
        && no == fromFoldable no'
        && (Heap.filter p heap) == fromFoldable yes'
        && yes `Heap.union` no == heap -- nothing gets lost

orderingProperty :: (Ord a) => MinHeap a -> MinHeap a -> Bool
orderingProperty heap1 heap2 = let
    list1 = toAscList heap1
    list2 = toAscList heap2
    in
    compare heap1 heap2 == compare list1 list2-}
