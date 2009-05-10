module Test.Heap.Internal
    ( runTests
    ) where

import Data.Char
import Data.Heap.Internal as Heap
import qualified Data.List as List
import Test.Heap.Common
import Test.QuickCheck

runTests :: IO ()
runTests = do
    qc "Eq" (eqProperty :: Heap Int Char -> Heap Int Char -> Heap Int Char -> Bool)
    qc "Ord" (ordProperty :: Heap Int Char -> Heap Int Char -> Heap Int Char -> Bool)
    qc "leftist heap" (leftistHeapProperty :: Heap Int Char -> Bool)
    qc "read/show" (readShowProperty :: Heap Int Char -> Bool)
    qc "Binary" (binaryProperty :: Heap Int Char -> Bool)
    qc "Monoid" (monoidProperty :: Heap Int Char -> Heap Int Char -> Heap Int Char -> Bool)
    qc "union" (unionProperty :: Heap Int Char -> Heap Int Char -> Bool)
    qc "Functor" (functorProperty (subtract 1000) (*42) :: Heap Char Int -> Bool)
    qc "fmap" (fmapProperty (subtract 1000) :: Heap Char Int -> Bool)
    qc "Foldable" (foldableProperty :: Heap Char Int -> Bool)
    qc "size" sizeProperty
    qc "view" viewProperty
    qc "singleton" (singletonProperty :: Char -> Int -> Bool)
    qc "partition" (partitionProperty testProp :: Heap Char Int -> Bool)
    qc "splitAt" splitAtProperty
    qc "span" spanProperty
    qc "fromFoldable/toList" (listProperty :: [Char] -> Bool)
    qc "fromDescFoldable/toAscList" (sortedListProperty :: [Char] -> Bool)
    where
    testProp :: Char -> Int -> Bool
    testProp c i = even i && isLetter c

instance (Arbitrary prio, Arbitrary val, Ord prio) => Arbitrary (Heap prio val) where
    arbitrary = fmap (fromFoldable . take 100) arbitrary
    shrink    = fmap fromFoldable . shrink . toList

leftistHeapProperty :: (Ord prio) => Heap prio val -> Bool
leftistHeapProperty Empty = True
leftistHeapProperty heap  =
    (maybe True (\(p, _, _) -> p >= _priority heap) (view (_left heap)))
        && (maybe True (\(p, _, _) -> p >= _priority heap) (view (_right heap)))
        && _rank heap == 1 + rank (_right heap)    -- rank == length of right spine
        && rank (_left heap) >= rank (_right heap) -- leftist property
        && _size heap == 1 + size (_left heap) + size (_right heap)
        && leftistHeapProperty (_left heap)
        && leftistHeapProperty (_right heap)

unionProperty :: (Ord prio, Ord val) => Heap prio val -> Heap prio val -> Bool
unionProperty a b = let ab = a `union` b
    in leftistHeapProperty ab && size ab == size a + size b
        && ab == ab `union` empty
        && ab == empty `union` ab
        && a == unions (fmap (uncurry singleton) (toList a))

fmapProperty :: (Ord prio) => (val -> val) -> Heap prio val -> Bool
fmapProperty f = leftistHeapProperty . fmap f

sizeProperty :: Int -> Bool
sizeProperty n = let
    n' = abs n `mod` 100
    h  = fromFoldable (zip [1..n'] (repeat ())) :: Heap Int ()
    in
    size h == n' && if n' == 0 then isEmpty h else not (isEmpty h)

viewProperty :: [Int] -> Bool
viewProperty []   = True
viewProperty list = let
    heap = fromFoldable (zip list (repeat ()))
    m    = minimum list
    in case view heap of
        Nothing          -> False -- list is not empty
        Just (p, (), hs) -> p == m
            && heap == union (singleton p ()) hs
            && viewProperty (tail list)

singletonProperty :: (Ord prio, Ord val) => prio -> val -> Bool
singletonProperty p v = let
    heap = singleton p v
    in
    leftistHeapProperty heap && size heap == 1 && view heap == Just (p, v, empty)

partitionProperty :: (Ord prio, Ord val) => (prio -> val -> Bool) -> Heap prio val -> Bool
partitionProperty p heap = let
    (yes,  no)  = partition p heap
    (yes', no') = List.partition (uncurry p) (toList heap)
    in
    (heap, empty) == partition (\_ _ -> True) heap
        && (empty, heap) == partition (\_ _ -> False) heap
        && yes == fromFoldable yes'
        && no == fromFoldable no'
        && yes `union` no == heap -- nothing gets lost

splitAtProperty :: Int -> Int -> Bool
splitAtProperty i n = let
    i'     = i `mod` 100
    n'     = n `mod` 100
    ab     = [1..n']
    (a, b) = List.splitAt i' ab
    heap   = fromFoldable $ zip ab (repeat ())
    in
    Heap.splitAt i' heap == (zip a (repeat ()), fromFoldable (zip b (repeat ())))

spanProperty :: Int -> Int -> Bool
spanProperty i n = let
    i'      = i `mod` 100
    n'      = n `mod` 100
    ab      = [1..n']
    (a, b)  = List.span (<= i') ab
    (a', h) = Heap.span (\x _ -> x <= i') $ fromFoldable (zip ab (repeat ()))
    in
    a == (fmap fst a') && h == fromFoldable (zip b (repeat ()))

listProperty :: (Ord prio) => [prio] -> Bool
listProperty xs = let
    list = List.sort xs
    heap = fromFoldable (zip xs [1..])
    in
    list == fmap fst (List.sort (toList heap))

sortedListProperty :: (Ord prio) => [prio] -> Bool
sortedListProperty xs = let
    list = List.sort xs
    heap = fromDescFoldable (zip (reverse list) [1..])
    in
    list == fmap fst (toAscList heap)
