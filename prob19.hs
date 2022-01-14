module Main where

import Data.List

months :: Int -> [Int]
months y
  | mod y 100 == 0 && mod y 400 == 0 = [31,29,31,30,31,30,31,31,30,31,30,31]
  | mod y 4 == 0 && mod y 100 /= 0   = [31,29,31,30,31,30,31,31,30,31,30,31]
  | otherwise                        = [31,28,31,30,31,30,31,31,30,31,30,31]

main :: IO()
main = putStr ("Ans " ++ show ans ++ "\n")
  where
    ans = length $ filter (\x -> mod x 7 == 0) z
    z = drop 12 $ map (+1) $ init $ scanl (+) 0 $ concat $ map (months) [1900..2000]
