-- What is the largest prime factor of the number 600851475143 ?

primeFactors :: Integer -> [Integer]
primeFactors 1 = [1]
primeFactors x = factors . filter ((==) 0 . rem x) $ [2..(div x 2)]
  where
    factors [] = [x]
    factors (y:ys) = y : primeFactors (div x y)

sol = last $ primeFactors 600851475143
