#! /usr/bin/runghc -D__DEBUG__

>
> module Main where
>
> import Control.Exception ( assert )
> import qualified Test.Heap as Heap
> import qualified Test.Heap.Internal as Internal
> import qualified Test.Heap.Item as Item
> import Test.QuickCheck
>
> main :: IO ()
> main = do
>     putStrLn "Ensuring assertions are not ignored:"
>     quickCheckWith (Args Nothing 1 1 1) $ expectFailure (assert False True)
>     putStrLn ""
>
>     putStrLn "Tests for Data.HeapT.Internal:" >> Internal.runTests >> putStrLn ""
>     putStrLn "Tests for Data.HeapT.Item:"     >> Item.runTests     >> putStrLn ""
>     putStrLn "Tests for Data.HeapT:"          >> Heap.runTests
>

