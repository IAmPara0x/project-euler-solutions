-- What is the 10001st prime number?

-- Sieve of Eratosthenes

-- NOTE: first naive solution.
--
isFactor :: Int -> Int -> Bool
isFactor x y = rem y x == 0

seive :: [Int]
seive = primes [2..]
  where
    primes (x:xs) = x : (primes $ filter (not . isFactor x) xs)


-- NOTE: faster solution and even sexier.
--
primes :: [Int]
primes = 2 : filter isPrime [3..]

isPrime n = f primes
  where
    f (x:xs)
      | x*x > n        = True
      | n `rem` x == 0 = False
      | otherwise      = f xs

sol = primes !! 10000
