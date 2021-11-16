module Main where

-- How many such routes are there through a 20×20 grid?

perm :: Integer -> Integer
perm 1 = 1
perm x =  x * perm (x - 1)

routes :: Integer -> Integer
routes x = div (perm (x * 2)) (perm x * perm x)

main :: IO()
main = print (routes 20)
