module Test.Heap.Common
    ( qc
    , readShowProperty
    , binaryProperty
    ) where

import Data.Binary
import Test.QuickCheck

qc :: (Testable prop) => String -> prop -> IO ()
qc msg prop = quickCheck
    $ whenFail (putStrLn msg)
    $ label msg prop

readShowProperty :: (Read a, Show a, Eq a) => a -> Bool
readShowProperty x = x == read (show x)

binaryProperty :: (Binary a, Eq a) => a -> Bool
binaryProperty x = x == decode (encode x)
