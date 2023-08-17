module BinaryTrees where

-- http://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

-- Binary Tree

data Tree a
    = Empty
    | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)


-- Problem 55: Construct completely balanced binary trees

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q,r) = (n - 1) `quotRem` 2 in
    [ Branch 'x' left right
    | i     <- [q .. q + r]
    , left  <- cbalTree i
    , right <- cbalTree (n - i - 1)
    ]


-- Problem 56: Symmetric binary trees

symmetric :: Tree a -> Bool
symmetric Empty          = True
symmetric (Branch _ l r) = mirror l r
  where
    mirror :: Tree a -> Tree a -> Bool
    mirror Empty            Empty            = True
    mirror (Branch _ la ra) (Branch _ lb rb) = mirror la rb && mirror ra lb
    mirror _                _                = False


-- Problem 57: Binary search treatZeroAsInf

construct :: Ord a => [a] -> Tree a
construct = foldl (flip add) Empty
  where
    add :: Ord a => a -> Tree a -> Tree a
    add x Empty = Branch x Empty Empty
    add x t@(Branch y l r) = case compare x y of
        LT -> Branch y (add x l) r
        GT -> Branch y l (add x r)
        EQ -> t


-- Problem 58: Generate-and-test paradigm (I do not understand the question)


-- Problem 59: Construct height-balanced binary trees

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree e 1 = [Branch e Empty Empty]
hbalTree e h =
    [ Branch e left right
    | (hl, hr) <- [(h-2, h-1), (h-1, h-1),(h-1, h-2)]
    , left     <- hbalTree e hl
    , right    <- hbalTree e hr
    ]


-- Problem 60: Construct height-balanced binary trees with a given number of nodes

-- TODO

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes = undefined

-- minNodes
-- maxHeight


-- Problem 61: Count the leaves of a binary tree

countLeaves:: Tree a -> Int
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ left  right) = countLeaves left + countLeaves right


-- Problem 61A: Collect the leaves of a binary tree in a list

leaves :: Tree a -> [a]
leaves Empty                  = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ left  right) = leaves left ++ leaves right


-- Problem 62: Collect the internal nodes of a binary tree in a list

internals :: Tree a -> [a]
internals Empty                  = []
internals (Branch x Empty Empty) = []
internals (Branch x left  right) = x : internals left ++ internals right