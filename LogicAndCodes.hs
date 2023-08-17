module LogicAndCodes where

import Control.Monad (replicateM)
import Data.List (sortBy, insertBy)
import Data.Ord (comparing)

-- http://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

-- Problem 46: Truth tables for logical expressions (2 Variables)

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' a b =  not $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not $ or' a b

xor' :: Bool -> Bool -> Bool
xor' = (/=)

impl' :: Bool -> Bool -> Bool
impl' a = or' (not a)

equ' :: Bool -> Bool -> Bool
equ' = (==)

table2 :: (Bool -> Bool -> Bool) -> IO ()
table2 f = putStrLn
    $ unlines [ show a ++ " " ++ show b ++ " " ++ show (f a b)
              | a <- [True, False]
              , b <- [True, False]
              ]


-- Problem 47: Define the above predicates as operators

infixr 3 `and'`

infixr 2 `or'`

infixr 2 `nor'`

infixr 2 `xor'`

infixl 1 `impl'`

infix 4 `equ'`


-- Problem 48: Truth table for logical expressions

table :: Int -> ([Bool] -> Bool) -> IO ()
table n f = putStrLn $ unlines [ toStr a ++ " => " ++ show (f a) | a <- aux n ]
  where
    aux n = replicateM n [True, False]
    toStr = unwords . map (\x -> show x ++ space x)
    space True = "  "
    space False = " "


-- Problem 49: Gray codes

gray :: Int -> [String]
gray n = aux ["0", "1"] 1
  where
    aux :: [String] -> Int -> [String]
    aux acc i
        | i >= n    = acc
        | otherwise = aux (left ++ reverse right) (i+1)
          where
            left  = map ('0' :) acc
            right = map ('1' :) acc


-- Problem 50: Huffman codes

data HTree a
    = Leaf a
    | Branch (HTree a) (HTree a)
    deriving Show

huffman :: (Ord a, Ord w, Num w) => [(a, w)] -> [(a, [Char])]
huffman freq = sortBy (comparing fst) . serialize . htree . sortBy (comparing fst) $ [ (w, Leaf x) | (x,w) <- freq ]
  where
    htree :: (Ord a, Num a) => [(a, HTree w)] -> HTree w
    htree [(_,t)] = t
    htree ((w1,t1):(w2,t2):wts) = htree $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts
    serialize :: HTree a -> [(a, [Char])]
    serialize (Leaf x) = [(x, "")]
    serialize (Branch l r)
        =  [ (x, '0':code) | (x,code) <- serialize l ]
        ++ [ (x, '1':code) | (x,code) <- serialize r ]
