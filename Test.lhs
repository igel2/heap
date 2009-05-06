#! /usr/bin/runghc -D__DEBUG__ -Wall

>
> module Main where
>
> import qualified Test.Heap as Heap
> import qualified Test.Heap.Internal as Internal
> import qualified Test.Heap.Item as Item
>
> main :: IO ()
> main = do Internal.runTests
>           Item.runTests
>           Heap.runTests
>

