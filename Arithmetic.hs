module Arithmetic where

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