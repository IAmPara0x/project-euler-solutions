
-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

-- NOTE: works but not elegant. Bad Solution!.

{-
fibseq :: [Int] -> [Int]
fibseq (x1:x2:xs) = (x1 + x2) : ([x1,x2] ++ xs)

fib :: Int -> [Int]
fib n = f (n - 1) [2,1]
  where
    f 0 _ = [1]
    f 1 xs = xs
    f n xs = f (n - 1) (fibseq xs)

sol :: Int
sol = sum $ filter even $ dropWhile (>= 4000000) (fib 40)
-}

-- NOTE: faster and elegant solution.

fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)

sol :: Int
sol = sum $ filter even $ takeWhile (< 4000000) fib
