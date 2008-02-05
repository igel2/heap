module Test.Heap (
	testHeap
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
	putStr "Head property:                   "
	quickCheck headProperty

instance (Arbitrary a, HeapPolicy p a) => Arbitrary (Heap p a) where
	arbitrary = do
		length <- choose (0, 100)
		list <- vector length
		return (Heap.fromList list)
	coarbitrary heap = variant (Heap.size heap)

leftistHeapProperty :: (HeapPolicy p a) => Heap p a -> Bool
leftistHeapProperty = Heap.check

sizeProperty :: Int -> Bool
sizeProperty n = let n' = abs n in Heap.size (Heap.fromList [1..n'] :: MaxHeap Int) == n'

orderProperty :: Int -> [Int] -> Bool
orderProperty n xs = let
		heap        = Heap.fromList xs :: MaxHeap Int
		(a,  b)     = List.splitAt n (sortBy (heapCompare (policy heap)) xs)
		(a', heap') = Heap.splitAt n heap
	in (Heap.fromList b == heap') && equal heap a a'
	where	equal _ [] [] = True
		equal _ _  [] = False
		equal _ [] _  = False
		equal h (x:xs) (y:ys) = EQ == heapCompare (policy h) x y

policy :: Heap p a -> p
policy = const undefined

headProperty :: [Int] -> Bool
headProperty [] = True
headProperty xs = let
		heap = fromList xs :: MaxHeap Int
	in Heap.head heap == List.head (sortBy (heapCompare (policy heap)) xs)

