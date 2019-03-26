
-- Conditional Evaluation with Guards | 69

import Data.List (nub, sortBy)

len :: [a] -> Integer
len (x:xs) = 1 + len xs
len [] = 0


mean :: Fractional a => [a] -> a
mean xs = sum xs / fromInteger (len xs)


list2palindrome :: [a] -> [a]
list2palindrome xs = xs ++ reverse xs
{-
list2palindrome (x:xs) = x : list2palindrome xs ++ [x]
list2palindrome [] = []
-}

is_palindrome :: Eq a => [a] -> Bool
is_palindrome xs = xs == reverse xs
{-
is_palindrome (x:xs) = null xs || x == head ys && is_palindrome (tail ys)
    where ys = reverse xs
is_palindrome [] = True
-}




sort_lsls :: [[a]] -> [[a]]
sort_lsls = sortBy $ \x y -> compare (len x) (len y)


join :: [a] -> [[a]] -> [a]
join _ [] = []
join _ (xs:[]) = xs
join sep (xs:xss) = xs ++ sep ++ join sep xss

data BinTree a = Empty | Node a (BinTree a) (BinTree a)

height :: BinTree a -> Integer
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)





