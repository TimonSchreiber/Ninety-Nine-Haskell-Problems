module Arithmetic where

import Data.List (group)

-- http://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

-- Problem 31: Determine whether a given integer number is prime

isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = isNotDivisor 2
      where
        isNotDivisor :: Int -> Bool
        isNotDivisor k = k * k > n || (n `mod` k /= 0 && isNotDivisor (k+1))


-- Problme 32: Determine the greates common divisor of two positive integer numbers

myGCD :: Int -> Int -> Int
myGCD 0 y = abs y
myGCD x 0 = abs x
myGCD x y = myGCD (y `mod` x) x


-- Problem 33: Determine whether two positive integer numbers are coprime

coprime :: Int -> Int -> Bool
coprime a b = 1 == myGCD a b


-- Problem 34: Calculate Euler's totient function phi(m)

totient :: Int -> Int
totient 1 = 1
totient m = length $ filter (coprime m) [1 .. m-1]


-- Problem 35: Determine thr prime factors of a given positive integer

primeFactors :: Int -> [Int]
primeFactors = aux [] 2
  where
    aux :: [Int] -> Int -> Int -> [Int]
    aux acc _ 1           = reverse acc
    aux acc d n
        | d * d > n       = reverse (n:acc)
        | n `mod` d == 0  = aux (d:acc) d (n `div` d)
        | otherwise       = aux acc (d+1) n


-- Problem 36: Determine the prime factors and their multiplicities of a given positive integer

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult xs = map (\x -> (head x, length x))
    $ group
    $ primeFactors xs


-- Problem 37: Calculate Euler's totient function phi(m) (improved)

phi :: Int -> Int
phi x = product $ map (\(p, m) -> (p - 1) * p ^ (m - 1)) $ primeFactorsMult x


-- Problem 28: Compare the two methods of calculating Euler's totient function (TODO)


-- Problem 39: A list of prime numbers in a given range

primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a .. b]


-- Problem 40: Goldbach's conjecture

goldbach :: Int -> (Int, Int)
goldbach n
    | n <= 2 || odd n = (0,0)
    | otherwise       = aux 2
      where
        aux :: Int -> (Int, Int)
        aux d
            | isPrime d && isPrime (n - d) = (d, n - d)
            | otherwise                    = aux (d + 1)


-- Problem 41: A list of even numbers and their Goldbach compositions in a given range

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach $ filter even [a .. b]


-- Problem 41.b: Filter by an upper limit for both values of the Goldbach compositions

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b limit = filter (\(x,y) -> x > limit && y > limit)
    $ goldbachList a b


-- Tail recursive fibonacci impelemntation (not related to 99-Problems)
fib :: Int -> Integer
fib = aux (0,1)
  where
    aux :: (Integer, Integer) -> Int -> Integer
    aux (a,b) n
        | n == 0    = a
        | n == 1    = b
        | otherwise = aux (b, a+b) (n-1)