module Main where

-- Find the sum of the digits in the number 100!


factorial :: Integer -> Integer
factorial 1 = 1
factorial x = x * factorial (x - 1)

digits :: Integer -> [Int]
digits number = map (\x -> read [x] :: Int) $ show number

main :: IO()
main = print (sum $ digits $ factorial 100)
