{-
  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
-}

n :: Int
n = 1000

isPythagoreanTriplet :: Int -> Int -> Bool
isPythagoreanTriplet a b = a*a + b*b == c*c
  where
    c = (n - a - b)

sol :: Int
sol = prod $ head [(i,j)| i <- [1..500], j <- [i+1..500], isPythagoreanTriplet i j]
  where
    prod (a,b) = (n - a - b) * a * b
