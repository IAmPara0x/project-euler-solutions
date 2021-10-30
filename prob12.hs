module Main where

-- What is the value of the first triangle number to have over five hundred divisors?

import Data.List (group)

triangleNums :: [Int]
triangleNums = 1 : zipWith (+) triangleNums [2..]

wheel :: [Int]
wheel = 2:4:wheel

spin :: [Int]
spin = 5 : zipWith (+) wheel spin

primes :: [Int]
primes = 2:3: filter isPrime spin

isPrime :: Int -> Bool
isPrime x = go x primes
  where
    go x (n:ns)
      | n*n > x         = True
      | x `mod` n == 0  = False
      | otherwise       = go x ns

factorize :: Int -> [Int]
factorize x = go x primes
  where
    go x (n:ns)
      | n*2 > x = [x]
      | r == 0  = n: go q primes
      | otherwise = go x ns
      where
        (q,r) = quotRem x n

divisors :: Int -> [Int]
divisors x = product <$> sequence [take (k+1) $ iterate (*p) 1 |
                                   (p,k) <- (\xs -> (head xs, length xs)) <$> group (factorize x)]


main = print sol
  where
    sol = head $ filter ((>500) . length . divisors) triangleNums

-- Runtime 0.08 secs
