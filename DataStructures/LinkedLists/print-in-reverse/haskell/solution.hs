{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

data SinglyLinkedListNode = SinglyLinkedListNode {
    nodeData :: Int,
    next :: SinglyLinkedListNode
} | SinglyLinkedListNodeNil

createSinglyLinkedList :: [Int] -> SinglyLinkedListNode
createSinglyLinkedList [] = SinglyLinkedListNodeNil
createSinglyLinkedList (x:xs) = SinglyLinkedListNode {
    nodeData = x,
    next = createSinglyLinkedList xs
}

instance {-# OVERLAPPING #-} Show (SinglyLinkedListNode, String) where
    show (SinglyLinkedListNodeNil, _) = ""
    show (SinglyLinkedListNode x SinglyLinkedListNodeNil, _) = show x
    show (SinglyLinkedListNode x xs, sep) = show x ++ sep ++ show(xs, sep)

-- Complete the reversePrint function below.

--
-- For your reference:
--
-- SinglyLinkedListNode {
--     nodeData :: Int
--     next :: SinglyLinkedListNode
-- }
--
--

reversePrint :: SinglyLinkedListNode -> IO ()
reversePrint SinglyLinkedListNodeNil = return ()
reversePrint llist = do
    reversePrint (next llist)
    putStrLn (show (nodeData llist))


readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    tests <- readLn :: IO Int

    forM_ [1..tests] $ \tests_itr -> do
        llistTempCount <- readLn :: IO Int

        llistTempTemp <- readMultipleLinesAsStringArray llistTempCount
        let llistTemp = Data.List.map (read :: String -> Int) llistTempTemp

        let llist = createSinglyLinkedList llistTemp

        reversePrint llist
