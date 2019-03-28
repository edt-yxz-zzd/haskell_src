
module Cycle
    (Cycle(..)
    ,distinguish_count_of
    )
where

class Eq a => Cycle a where
    is_clockwise :: Triple a -> Bool

    is_counterclockwise :: Triple a -> Bool
    is_strictly_clockwise :: Triple a -> Bool
    is_strictly_counterclockwise :: Triple a -> Bool
    is_clockwises :: [a] -> Bool
    is_counterclockwises :: [a] -> Bool
    is_cyclewises :: [a] -> Bool

    is_strictly_clockwise abc
        = distinguish_count_of abc == GT && is_clockwise abc
    is_strictly_counterclockwise abc
        = distinguish_count_of abc == GT && is_counterclockwise abc
    is_counterclockwise = not . is_clockwise
    is_clockwises = _is_clockwises is_clockwise
    is_counterclockwises = _is_clockwises is_counterclockwise
    is_cyclewises ls = is_cyclewises ls || is_counterclockwises ls

_is_clockwises :: (Triple a -> Bool) -> [a] -> Bool
_is_clockwises is_clockwise (a:b:ls) = f b ls where
    f b (c:ls)  = is_clockwise (a,b,c) && f c ls
    f _ _       = True

type Triple a = (a, a, a)
distinguish_count_of :: Eq a => Triple a -> Ordering
distinguish_count_of (a,b,c) =
    if a==b
    then if b==c
        then GT         -- [abc]
        else EQ         -- [ab|c]
    else if b==c
        then EQ         -- [a|bc]
        else if a==c
            then EQ     -- [ca|b]
            else LT     -- [a|b|c]


instance Ord a => Cycle (Maybe a) where
    is_clockwise (Just a, Just b, Just c)
        = if a <= b then not (a<c && c<b) else (b<=c && c<=a)
    is_clockwise (Nothing, Just b, Just c) = b <= c
    is_clockwise (Just a, Nothing, Just c) = c <= a
    is_clockwise (Just a, Just b, Nothing) = a <= b
    is_clockwise _ = True


