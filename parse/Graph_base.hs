{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}



module Graph_base
    ( MultiSet_Len2, make_mset2
    , Set_Len2, unsafe_make_set2
    , sort_pair
    , OpToUnsortedPair(..), OpToSortedPair(..)
    )
where
import SeedUtils (lift2)
data MultiSet_Len2 a = MultiSet_Len2 a a
data Set_Len2 a = Set_Len2 a a
make_mset2 :: Eq a => a -> a -> MultiSet_Len2 a
make_mset2 a b = MultiSet_Len2 a b
unsafe_make_set2 :: Eq a => a -> a -> Set_Len2 a
unsafe_make_set2 a b =
    if a == b then error "a == b @unsafe_make_set2" else Set_Len2 a b

unsorted_pair_eq :: Eq a => (a, a) -> (a, a) -> Bool
unsorted_pair_eq lhs@(a, b) rhs@(c, d) =
        if a == c then b == d else (a, b) == (d, c)
unsorted_pair_compare :: Ord a => (a, a) -> (a, a) -> Ordering
unsorted_pair_compare = lift2 sort_pair compare

eq_via_unsorted_pair :: (Eq a, OpToUnsortedPair a c) => c -> c -> Bool
eq_via_unsorted_pair = lift2 to_unsorted_pair unsorted_pair_eq
compare_via_unsorted_pair
    :: (Ord a, OpToUnsortedPair a c) => c -> c -> Ordering
compare_via_unsorted_pair =
    -- lift2 to_unsorted_pair unsorted_pair_compare
    lift2 to_sorted_pair compare
instance Eq a => Eq (Set_Len2 a) where
    (==) = eq_via_unsorted_pair
instance Eq a => Eq (MultiSet_Len2 a) where
    (==) = eq_via_unsorted_pair
    --MultiSet_Len2 a b == MultiSet_Len2 c d =
    --    if a == c then b == d else (a, b) == (d, c)
instance Ord a => Ord (Set_Len2 a) where
    compare = compare_via_unsorted_pair
instance Ord a => Ord (MultiSet_Len2 a) where
    compare = compare_via_unsorted_pair


sort_pair :: Ord a => (a, a) -> (a, a)
sort_pair p@(a, b) = if b < a then (b, a) else p
class OpToUnsortedPair a c | c -> a where
    to_unsorted_pair :: c -> (a, a)
class OpToUnsortedPair a c => OpToSortedPair a c where
    to_sorted_pair :: Ord a => c -> (a, a)
    to_sorted_pair = sort_pair . to_unsorted_pair
instance OpToUnsortedPair a c => OpToSortedPair a c

instance OpToUnsortedPair a (MultiSet_Len2 a) where
    to_unsorted_pair (MultiSet_Len2 a b) = (a, b)
instance OpToUnsortedPair a (Set_Len2 a) where
    to_unsorted_pair (Set_Len2 a b) = (a, b)

