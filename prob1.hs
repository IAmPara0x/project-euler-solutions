
-- Find the sum of all the multiples of 3 or 5 below 1000.

fac3_5 :: Int -> Bool
fac3_5 x = rem x 3 == 0 || rem x 5 == 0

-- NOTE: Is there a better way?
x = sum [i | i <- [1..1000], fac3_5 i]
