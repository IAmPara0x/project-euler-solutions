module Main where

-- What is the sum of the digits of the number 21000?
-- Haskell has arbitary precision multiplication.

main :: IO()
main = print $ sol
  where
    sol = sum $ map (\x -> read [x] :: Int) $ show (2^1000)
