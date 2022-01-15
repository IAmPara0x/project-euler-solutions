module Main where

import Data.List

newtype Name = Name String
  deriving (Show)

instance Eq Name where
  (==) (Name a) (Name b) = a == b
  (/=) (Name a) (Name b) = a /= b

instance Ord Name where
  (<=) (Name a1) (Name a2) = z pairs
    where
      pairs = dropWhile (cmp (==)) $ zipLongest '0' a1 a2
      z xs
        | length xs == 0 = True
        | otherwise      = cmp (<) $ head xs

cmp :: Ord a => (a -> a -> Bool) -> (a,a) -> Bool
cmp f (x,y) = f x y

zipLongest :: a -> [a] -> [a] -> [(a,a)]
zipLongest a xs ys
  | lenXs < lenYs = zip (xs ++ replicate (lenYs - lenXs) a) ys
  | lenXs > lenYs = zip xs (ys ++ replicate (lenXs - lenYs) a)
  | otherwise     = zip xs ys
  where
    lenYs = length ys
    lenXs = length xs

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s  = w : splitOn c (f s')
  where
    (w,s') = break (\i -> i == c) s
    f str
      | str == "" = ""
      | otherwise = tail str

delimiter :: Char
delimiter = ','

chars  = ['A'..'Z']

getIndex :: Char -> Int
getIndex c = f c 0
  where
    f c i
      | c == chars !! i = i + 1
      | otherwise       = f c (i + 1)


main :: IO()
main = do
        content <- readFile "p022_names.txt"
        names   <- return $ map (filter (\c -> c /= '"')) $ splitOn delimiter content
        snames <- return $ sort names
        n <- return $ zip [1..] $ map (sum . map getIndex) snames
        print (sum $ map product n)


