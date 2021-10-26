module Main where

-- Find the sum of all the primes below two million.

-- Following paper: https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
-- NOTE: Runtime 1.66s

{-
NOTE: This Implementation doesn't work.

import Data.IntPSQ (IntPSQ, empty, alterMin, findMin, insert)

type Table = IntPSQ Int [Int]

minKey :: Table -> Int
minKey table = case findMin table of
                 Just (k,_,_) -> k
                 Nothing -> undefined

minVal :: Table -> [Int]
minVal table = case findMin table of
                 Just (_,_,v) -> v
                 Nothing -> undefined

modifyMin :: Int -> Table -> Table
modifyMin mKey table
  | mKey == minKey table = modifyMin mKey (snd $ alterMin f table)
  | otherwise            = table
  where
    f (Just (_, _, m':ms))   = (True, Just (m',m',ms))

insertTable :: Int -> Int -> Table -> Table
insertTable k v = case lookup k table of
                    Nothing    -> insert k k [v] table
                    Just (_,vs) -> vs ++ [v]

sieve' [] table = []
sieve' (y:ys) table
  | minKey table <= y   = sieve' ys (adjust table)
  | otherwise           = y : sieve' ys (insertTable y ys table)
    where
      adjust table
        | minKey table <= y  = modifyMin (minKey table) table
        | otherwise          = table

sieve [] = []
sieve (x:xs) = x : sieve' xs (insertTable x xs empty)
primes' = sieve [2..30]
x = take 10 primes'
-}

import Prelude hiding (lookup)
import Data.IntMap (insert, empty, insertWith, lookup, delete)

primes = sieve [2..]

sieve xs = sieve' xs empty
  where
    sieve' [] table = []
    sieve' (x:xs) table =
      case  lookup x table of
        Nothing    -> x : sieve' xs (insert (x*x) [x] table)
        Just facts -> sieve' xs (foldl reinsert (delete x table) facts)
      where
        reinsert table prime = insertWith (++) (x+prime) [prime] table

sol = sum $ takeWhile (<2000000) primes

main :: IO()
main = print sol
