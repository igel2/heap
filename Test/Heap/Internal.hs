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
    qc "decode/encode property" (binaryProperty :: Heap Int Char -> Bool)
    qc "monoid property" (monoidProperty :: Heap Int Char -> Heap Int Char -> Heap Int Char -> Bool)
    qc "functor property" (functorProperty (subtract 1000) (*42) :: Heap Char Int -> Bool)
    qc "size property" sizeProperty
    qc "view property" viewProperty

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
    size h == n' && (n' > 0 || isEmpty h)

viewProperty :: [Int] -> Bool
viewProperty []   = True
viewProperty list = let
    heap = fromFoldable (zip list (repeat ()))
    m    = minimum list
    in case view heap of
        Nothing          -> False -- list is not empty
        Just (p, (), hs) -> p == m && viewProperty (tail list)
