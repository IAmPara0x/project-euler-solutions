

-- Find the difference between the sum of the squares of the first one hundred
-- natural numbers and the square of the sum.
-- NOTE: sexy solution.

n :: Int
n = 100

sol :: Int
sol = 2 * sum [i*j | i <- [1..n], j <- [1..n], i < j]
