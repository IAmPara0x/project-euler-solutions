
-- Find the largest palindrome made from the product of two 3-digit numbers.
-- NOTE: This solution is also sexy.

isPalidrome (x,y) = prod == reverse prod
  where
    prod = show (x * y)

max' (x1,y1) (x2,y2)
  | x1*y1 > x2*y2 = (x1,y1)
  | otherwise = (x2,y2)

sol = foldl1 max' $ filter isPalidrome [(i,j) | i <- [111..999], j <- [111..999], i <= j]
