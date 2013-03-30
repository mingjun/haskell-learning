{-what a perfect program!! -}
factors :: Integer -> [Integer]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Integer -> Bool
-- can improve on performance
prime n = length(factors n) == 2

primes :: Integer -> [Integer]
primes n = [m | m <- [2..n] , prime m]