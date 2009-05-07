module Test.Heap.Common
    ( qc
    , eqProperty, ordProperty
    , readShowProperty
    , binaryProperty
    , monoidProperty
    , functorProperty
    ) where

import Data.Binary
import Data.Monoid
import Test.QuickCheck

qc :: (Testable prop) => String -> prop -> IO ()
qc msg prop = quickCheck
    $ whenFail (putStrLn msg)
    $ label msg prop

eqProperty :: (Eq a) => a -> a -> a -> Bool
eqProperty x y z = (x == y) == (y == x)
    && ((not (x == y && y == z)) || x == z)

ordProperty :: (Ord a) => a -> a -> a -> Bool
ordProperty x y z = let
    _min = minimum [x, y, z]
    _max = maximum [x, y, z]
    in case compare x y of
            LT -> x < y && x <= y && not (x > y) && not (x >= y)
            EQ -> x == y && x <= y && x >= y && not (x < y) && not (x > y)
            GT -> x > y && x >= y && not (x < y) && not (x <= y)
        && _min <= x && _min <= y && _min <= z
        && _max >= x && _max >= y && _max >= z

readShowProperty :: (Read a, Show a, Eq a) => a -> Bool
readShowProperty x = x == read (show x)

binaryProperty :: (Binary a, Eq a) => a -> Bool
binaryProperty x = x == decode (encode x)

monoidProperty :: (Monoid m, Eq m) => m -> m -> m -> Bool
monoidProperty m1 m2 m3 = let
    result = mconcat [m1, m2, m3]
    in
    result == (m1 `mappend` m2) `mappend` m3
        && result == m1 `mappend` (m2 `mappend` m3)
        && m1 == mempty `mappend` m1
        && m1 == m1 `mappend` mempty

functorProperty :: (Functor f, Eq (f a), Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
functorProperty f g fun = fun == fmap id fun
    && fmap (f . g) fun == fmap f (fmap g fun)
