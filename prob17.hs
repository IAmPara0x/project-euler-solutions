module Main where

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

numToWord :: Int -> String
numToWord x
  | x == 1 = "one"
  | x == 2 = "two"
  | x == 3 = "three"
  | x == 4 = "four"
  | x == 5 = "five"
  | x == 6 = "six"
  | x == 7 = "seven"
  | x == 8 = "eight"
  | x == 9 = "nine"
  | x == 11 = "eleven"
  | x == 12 = "twelve"
  | x == 13 = "thirteen"
  | x == 14 = "fourteen"
  | x == 15 = "fiveteen"
  | x == 16 = "sixteen"
  | x == 17 = "seventeen"
  | x == 18 = "eighteen"
  | x == 19 = "nineteen"
  | x == 10 = "ten"
  | x == 20 = "twenty"
  | x == 30 = "thirty"
  | x == 40 = "forty"
  | x == 50 = "fifty"
  | x == 60 = "sixty"
  | x == 70 = "seventy"
  | x == 80 = "eighty"
  | x == 90 = "ninety"
  | x == 100 = "one hundred"
  | x == 200 = "two hundred"
  | x == 300 = "three hundred"
  | x == 400 = "four hundred"
  | x == 500 = "five hundred"
  | x == 600 = "six hundred"
  | x == 700 = "seven hundred"
  | x == 800 = "eight hundred"
  | x == 900 = "nine hundred"
  | x == 1000 = "one thousand"
  | length (show x) == 2 = unwords $ map (\y -> numToWord (read y::Int)) $ f2 $ show x
  | length (show x) == 3 = f3 $ show x
  where
    f2 (t:o) = [t:['0'],o]
    f3 (x:xs) = unwords [numToWord (read [x]:: Int), "hundred and", numToWord (read xs::Int)]


main :: IO()
main = print sol
  where
    sol = sum $ map length $ concat map (words . numToWord) [1..1000]
