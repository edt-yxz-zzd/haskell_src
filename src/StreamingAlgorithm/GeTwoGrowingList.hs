
{-# LANGUAGE TypeFamilies #-}
module GeTwoGrowingList
    (GeTwoGrowingList() -- ()
    ,assert_GeTwoGrowings
    ,is_finite_GeTwoGrowings
    ,unGeTwoGrowingList
    ,unsafe_mkGeTwoGrowingList
    ,maybe_make_finite_GeTwoGrowingList
    )
where


import ToList
import UnsafeFromList
import Control.Exception(assert)

newtype GeTwoGrowingList = Private_GeTwoGrowingList [Integer]
    -- 2 <= ls[0] <= ls[1] <= ...
    -- for EngelExpansion
    deriving (Show, Eq, Ord)

assert_GeTwoGrowings :: [Integer] -> [Integer]
assert_GeTwoGrowings = f 2 where
    f pre (h:ts) = assert (pre <= h) $ h : f h ts
    f _ [] = []

is_finite_GeTwoGrowings :: [Integer] -> Bool
is_finite_GeTwoGrowings = f 2 where
    f pre (h:ts) = pre <= h && f h ts
    f _ [] = True

unGeTwoGrowingList :: GeTwoGrowingList -> [Integer]
unGeTwoGrowingList (Private_GeTwoGrowingList ls) = ls
unsafe_mkGeTwoGrowingList :: [Integer] -> GeTwoGrowingList
unsafe_mkGeTwoGrowingList = Private_GeTwoGrowingList . assert_GeTwoGrowings
maybe_make_finite_GeTwoGrowingList :: [Integer] -> Maybe GeTwoGrowingList
maybe_make_finite_GeTwoGrowingList ls
    | is_finite_GeTwoGrowings ls = Just $ Private_GeTwoGrowingList ls
    | otherwise = Nothing


instance ToList GeTwoGrowingList where
    type ElementType4ToList GeTwoGrowingList = Integer
    toList = unGeTwoGrowingList
instance UnsafeFromList GeTwoGrowingList where
    unsafe_fromList = unsafe_mkGeTwoGrowingList










