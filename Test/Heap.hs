module Test.Heap
    ( testHeap
    ) where

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
    qc "{from,to}{,Asc,Desc}List" (listProperty :: [Int] -> Bool)
    qc "toList === elems" (toListProperty :: MaxHeap Int -> Bool)
    qc "partition and filter" (partitionFilterProperty testProperty :: MinHeap Int -> Bool)
    qc "ordering property" (orderingProperty :: MinHeap Int -> MinHeap Int -> Bool)
    where
    testProperty x = x `mod` 2 == 0

qc :: (Testable prop) => String -> prop -> IO ()
qc msg prop = quickCheck
    $ whenFail (putStrLn msg)
    $ label msg prop

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

readShowProperty :: (HeapPolicy p a, Show a, Read a) => Heap p a -> Bool
readShowProperty heap = heap == read (show heap)

binaryProperty :: (HeapPolicy p a, Eq a, Binary a) => Heap p a -> Bool
binaryProperty heap = let
    heap' = decode (encode heap)
    in leftistHeapProperty heap' && heap' == heap

monoidProperty :: (Monoid m, Eq m) => m -> m -> m -> Bool
monoidProperty m1 m2 m3 = let
    result = mconcat [m1, m2, m3]
    in
    result == (m1 `mappend` m2) `mappend` m3
        && result == m1 `mappend` (m2 `mappend` m3)
        && result == mempty `mappend` result
        && result == result `mappend` mempty

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
            && h == unsafeHead heap && hs == unsafeTail heap && (h, hs) == unsafeUncons heap

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

listProperty :: [Int] -> Bool
listProperty xs = let
    xsAsc  = sort xs
    xsDesc = reverse xsAsc
    h1     = fromList xs         :: MinHeap Int
    h2     = fromAscList xsAsc   :: MinHeap Int
    h3     = fromDescList xsDesc :: MinHeap Int
    in
    (h1 == h2) && (h2 == h3)
        && (and (map leftistHeapProperty [h1, h2, h3]))
        && (and (map ((== xsAsc) . toAscList) [h1, h2, h3]))
        && (and (map ((== xsDesc) . toDescList) [h1, h2, h3]))

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

