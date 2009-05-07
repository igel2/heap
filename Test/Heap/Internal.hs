module Test.Heap.Internal
    ( runTests
    ) where

import Data.Heap.Internal
import Test.Heap.Common
import Test.QuickCheck

runTests :: IO ()
runTests = do
    qc "Eq property" (eqProperty :: Heap Int Char -> Heap Int Char -> Heap Int Char -> Bool)
    qc "Ord property" (ordProperty :: Heap Int Char -> Heap Int Char -> Heap Int Char -> Bool)
    qc "leftist heap property" (leftistHeapProperty :: Heap Int Char -> Bool)
    qc "read/show property" (readShowProperty :: Heap Int Char -> Bool)
    qc "Binary property" (binaryProperty :: Heap Int Char -> Bool)
    qc "Monoid property" (monoidProperty :: Heap Int Char -> Heap Int Char -> Heap Int Char -> Bool)
    qc "Functor property" (functorProperty (subtract 1000) (*42) :: Heap Char Int -> Bool)
    qc "Foldable property" (foldableProperty :: Heap Char Int -> Bool)
    qc "size property" sizeProperty
    qc "view property" viewProperty
    qc "singleton property" (singletonProperty :: Char -> Int -> Bool)

instance (Arbitrary prio, Arbitrary val, Ord prio) => Arbitrary (Heap prio val) where
    arbitrary = do
        len  <- choose (0, 100)
        fmap fromFoldable $ vector len

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
        Just (p, (), hs) -> p == m && viewProperty (tail list)

singletonProperty :: (Ord prio, Ord val) => prio -> val -> Bool
singletonProperty p v = let
    heap = singleton p v
    in
    leftistHeapProperty heap && size heap == 1 && view heap == Just (p, v, empty)
