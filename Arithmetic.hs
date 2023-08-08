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