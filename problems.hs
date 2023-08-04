-- http://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

-- Part 01: Lists

-- Probmel 01: Find the last element of a list

myLast :: [a] -> a
myLast []       = error "Empty List!"
myLast [x]      = x
myLast (_:xs)   = last xs


-- Problem 02: Find the last-but-ine (or second-last) element of a list

myButLast :: [a] -> a
myButLast []        = error "Not enough elements in List!"
myButLast [x,_]     = x
myButLast (_:tl)    = myButLast tl


-- Problem 03: Find the K'th element of a list

elementAt :: [a] -> Int -> a
elementAt [] _      = error "Empty List!"
elementAt (x:_) 1   = x
elementAt (_:xs) k
    | k < 1         = error "Index out of Bounds!"
    | otherwise     = elementAt xs (k-1)


-- Problem 04: Find the number of elements in a list

myLength :: [a] -> Int
myLength xs = aux xs 0
  where
    aux :: [a] -> Int -> Int
    aux [] n        = n
    aux (_:xs) n    = aux xs (n+1)


-- Problem 05: Reverse a list

myReverse :: [a] -> [a]
myReverse xs = aux xs []
  where
    aux :: [a] -> [a] -> [a]
    aux [] acc      = acc
    aux (x:xs) acc  = aux xs (x:acc)


-- Problem 06: Find out whether a list is a palindrom

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom xs = xs == myReverse xs


-- Problem 07: Flatten a nested list structure

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten lst = myReverse $ aux lst []
  where
    aux :: NestedList a -> [a] -> [a]
    aux (Elem e) acc        = e:acc
    aux (List []) acc       = acc
    aux (List (x:xs)) acc   = aux (List xs) (aux x acc)


-- Problem 08: Eliminate consecutive duplicates of list elements

compress :: (Eq a) => [a] -> [a]
compress xs = myReverse $ aux xs []
  where
    aux :: (Eq a) => [a] -> [a] -> [a]
    aux [] acc          = acc
    aux (x:xs) []       = aux xs [x]
    aux (x:xs) acc
        | x == head acc = aux xs acc
        | otherwise     = aux xs (x:acc)


-- Problem 09: Pack consecutive duplicates of list elements into sublists

pack :: (Eq a) => [a] -> [[a]]
pack xs = myReverse $ aux xs [] []
  where
    aux :: (Eq a) => [a] -> [a] -> [[a]] -> [[a]]
    aux [] current acc      = current:acc
    aux (x:xs) [] acc       = aux xs [x] acc
    aux (x:xs) current acc
        | x `elem` current  = aux xs (x:current) acc
        | otherwise         = aux xs [x] (current:acc)


-- Problem 10: Run-length encoding of a list

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = aux (pack xs) []
  where
    aux :: (Eq a) => [[a]] -> [(Int, a)] -> [(Int, a)]
    aux [] acc      = acc
    aux (x:xs) acc  = aux xs ((length x, head x):acc)

