#! /usr/bin/runghc -D__DEBUG__ -Wall -fno-ignore-asserts

>
> module Main where
>
> import qualified Test.Heap as Heap
> import qualified Test.Heap.Internal as Internal
> import qualified Test.Heap.Item as Item
>
> main :: IO ()
> main = do
>     putStrLn "Tests for Data.Heap.Item:"     >> Item.runTests     >> putStrLn ""
>     putStrLn "Tests for Data.Heap.Internal:" >> Internal.runTests >> putStrLn ""
>     putStrLn "Tests for Data.Heap:"          >> Heap.runTests
>

