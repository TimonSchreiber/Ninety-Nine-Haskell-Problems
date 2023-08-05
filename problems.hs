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
encode = aux [] . pack
  where
    aux :: (Eq a) => [(Int, a)] -> [[a]] -> [(Int, a)]
    aux acc []      = reverse acc
    aux acc (x:xs)  = aux ((length x, head x):acc) xs


-- Problem 11: Modified run-length encoding

data ListEntry a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListEntry a]
encodeModified = map aux . pack
  where
    aux :: [a] -> ListEntry a
    aux [x]     = Single x
    aux (x:xs)  = Multiple (1+length xs) x


-- Problem 12: Decode a run-length encoded list

decodeModified :: [ListEntry a] -> [a]
decodeModified = concatMap aux
  where
    aux :: ListEntry a -> [a]
    aux (Single e) = [e]
    aux (Multiple n e) = replicate n e


-- Problem 13: Run-length encoding of a list (direct solution)

-- listToEntry :: [a] -> ListEntry
-- listToEntry (x:xs)
--     | myLength xs == 0  = Single x
--     | otherwise         = Multiple (1 + myLength xs) x

-- encodeDirect :: [a] -> [ListEntry a]
-- encodeDirect xs = map (\(n,e) -> if n == 1 then Single e else Multiple n e) aux xs 0 []
--   where
--     aux :: [a] -> a -> Int -> [(Int, a)]
--     aux [] current counter acc  = (counter, current) : acc
--     aux (x:xs) current counter acc
--         | x == current          = aux xs current (counter+1) acc
--         | otherwise             = aux xs x 1 ((counter, current) : acc)

encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
    where
      helper x [] = [(1,x)]
      helper x (y@(a,b):ys)
        | x == b    = (1+a,x):ys
        | otherwise = (1,x):y:ys

encodeDirect' :: Eq a => [a] -> [ListEntry a]
encodeDirect' = map encodeHelper . encode'
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x


-- Problem 14 Duplicate the elements of a list

-- using helper class
dupli :: [a] -> [a]
dupli = aux []
  where
    aux :: [a] -> [a] -> [a]
    aux acc []      = myReverse acc
    aux acc (x:xs)  = aux (x:x:acc) xs

-- using list comprehension
dupli' :: [a] -> [a]
dupli' xs = concat [[x,x] | x <- xs]


-- Problem 15: Replicate the elements of a list a given number of times

-- suing list comprehension
repli :: [a] -> Int -> [a]
repli xs n = concat [replicate n x | x <- xs]

-- using list monad
repli' :: [a] -> Int -> [a]
repli' xs n = xs >>= replicate n


-- Problem 16: Drop every N'th element from a list

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map snd
    $ filter (\(i,x) -> i `mod` n /= 0)
    $ zip [1 .. ] xs


-- Problem 17: Split a list into two parts; the length of the first part is given

split :: [a] -> Int -> ([a], [a])
split = aux []
  where
    aux :: [a] -> [a] -> Int -> ([a], [a])
    aux acc [] _    = (reverse acc, [])
    aux acc y@(x:xs) k
        | k >= 1    = aux (x:acc) xs (k-1)
        | otherwise = (reverse acc, y)


-- Problem 18: Extract a slice from a list

slice :: [a] -> Int -> Int -> [a]
slice = aux []
  where
    aux :: [a] -> [a] -> Int -> Int -> [a]
    aux acc [] _ _     = reverse acc
    aux acc (x:xs) start end
        | end <= 0    = reverse acc
        | start <= 1  = aux (x:acc) xs start (end-1)
        | otherwise   = aux acc xs (start-1) (end-1)