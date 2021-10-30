module Main where

-- Which starting number, under one million, produces the longest collatz chain?


-- Version: 1, Runtime: 3.09s

import Prelude hiding (lookup)
import Data.IntMap.Strict (IntMap,empty,lookup,insert)

type Table = IntMap Int

iTable :: Table
iTable = insert 1 1 empty

collatzF :: Int -> Int
collatzF x
  | x == 1    = 1
  | even x    = div x 2
  | otherwise = 3*x + 1

collatzSeq :: Int -> [Int]
collatzSeq 1 = [1]
collatzSeq x = x : collatzSeq (collatzF x)

lCollatzSeq :: Int -> Table -> (Int, Table)
lCollatzSeq 1 table = (1,table)
lCollatzSeq x table = case lookup x table of
                        Nothing -> ((l+1),insert x (l+1) t)
                        Just l  -> (l,table)
                        where
                          (l,t) = lCollatzSeq (collatzF x) table

cmp :: (Int,Table) -> Int -> (Int,Table)
cmp (l,t) n = (max l l', t')
  where
    (l',t') = lCollatzSeq n t

sol = foldl cmp (1,iTable) [1..1000000]

main :: IO()
main = print $ fst sol

