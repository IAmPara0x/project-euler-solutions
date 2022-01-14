{-# LANGUAGE  NumericUnderscores #-}
module Main where

import qualified Data.Vector as V

n = 10_000

divisors :: Int -> V.Vector Int
divisors 1 = V.fromList [1]
divisors x = V.fromList [i | i <- [1..div x 2], mod x i == 0]

z :: V.Vector Int
z = V.fromList $ map (f . sum . divisors) [1..n]
  where
    f x
      | n < x     = 0
      | otherwise = x

search :: Int -> Int
search x
 | x == y2 && x /= y1  = y1
 | otherwise = 0
  where
    y1 = z V.! (x-1)
    y2 = if 1 <= y1
            then z V.! (y1-1)
            else -1

main :: IO()
main = print $ show $ (sum . map) search [1..n]
