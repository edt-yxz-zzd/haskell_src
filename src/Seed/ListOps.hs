{-# LANGUAGE TypeFamilies #-}

module Seed.ListOps
    ( empty_or_tail, split_if, split1_if, split_eq, join1, join
    , list_len_ge, list_len_eq, full_take, startswith, endswith
    , group_fst, groupByEx
    , fst_eq, fst_op
    , module Data.List
    {-
    , ListLenLe1x
    , list2maybe_listlenle1x, unsafe_list2listlenle1x, listlenle1x2list
    , ListLenLe2x
    , list2maybe_listlenle2x, unsafe_list2listlenle2x, listlenle2x2list
    -}
    )
where

import Data.List
import Numeric.Natural
import Seed.UnsafeUnpack (unsafe_unjust)

--import Data.Semigroup

empty_or_tail :: [a] -> [a]
empty_or_tail (h:ts) = ts
empty_or_tail _ = []
split1_if :: (a->Bool) -> [a] -> ([a], [a])
split1_if f ls = (hs, empty_or_tail ts_) where
    (hs, ts_) = break f ls

-- split_if _ [] == [[]]
-- length (split_if _ _) >= 1
split_if :: (a->Bool) -> [a] -> [[a]]
split_if pred _ls = _f (undefined : _ls) where
    _f (_:ls) = hs : _f ts where
        (hs, ts) = break pred ls
    _f _ = []

split_eq :: Eq a => a -> [a] -> [[a]]
split_eq a = split_if (a==)
join1 :: a -> [[a]] -> [a]
join :: [a] -> [[a]] -> [a]
join = intercalate
join1 a = join [a]




{-
newtype ListLenLe1x a = ListLenLe1x (a, [a])
list2maybe_listlenle1x :: [a] -> Maybe (ListLenLe1x a)
list2maybe_listlenle1x (h:ts) = Just (ListLenLe1x (h, ts))
list2maybe_listlenle1x _ = Nothing

unsafe_list2listlenle1x = unsafe_unjust . list2maybe_listlenle1x
listlenle1x2list (ListLenLe1x (h,ts)) = h:ts


newtype ListLenLe2x a = ListLenLe2x (a, a, [a])
list2maybe_listlenle2x :: [a] -> Maybe (ListLenLe2x a)
list2maybe_listlenle2x (h:h':ts) = Just (ListLenLe2x (h, h', ts))
list2maybe_listlenle2x _ = Nothing

unsafe_list2listlenle2x = unsafe_unjust . list2maybe_listlenle2x
listlenle2x2list (ListLenLe2x (h,h',ts)) = h:h':ts
-}



list_len_eq :: Integral i => i -> [a] -> Bool
list_len_eq i _ | i < 0 = False
list_len_eq i _ls = _list_len_eq i _ls where
    _list_len_eq 0 ls = null ls
    _list_len_eq n (_:ts) = _list_len_eq (pred n) ts
    _list_len_eq _ _ = False
list_len_ge :: Integral i => i -> [a] -> Bool
list_len_ge i _ | i <= 0 = True
list_len_ge _ [] = False
list_len_ge i (h : ts) = list_len_ge (pred i) ts

full_take :: Integral i => i -> [a] -> Maybe [a]
full_take i ls
    | i <= 0 = Just []
    | list_len_ge i ls = Just $ genericTake i ls
    | otherwise = Nothing
{-
full_take i _ | i <= 0 = Just []
full_take _ [] = Nothing
full_take i (h : ts) = maybe Nothing (Just . (h:)) $ full_take (prec i) ts
-}

startswith, endswith :: Eq a => [a] -> [a] -> Bool
startswith hs = f where
    u = length hs
    f ls | list_len_ge u ls = take u ls == hs
    f _ = False
endswith ts = f where
    u = length ts
    f ls | u <= v = drop (v-u) ls == ts
         | otherwise = False
         where
            v = length ls

group_fst :: Eq ord => [(ord, val)] -> [(ord, [val])]
group_fst pairs = map f $ groupBy fst_eq pairs where
    f ls@((ord, _):_) = (ord, map snd ls)
groupByEx :: Eq ord => (a->ord) -> (a->val) -> [a] -> [(ord, [val])]
groupByEx a2ord a2val = group_fst . map info where
    info a = (a2ord a, a2val a)

fst_eq :: Eq k => (k,a) -> (k,b) -> Bool
fst_eq = fst_op (==)
fst_op :: (k->k->r) -> (k,a) -> (k,b) -> r
fst_op op a b = fst a `op` fst b



----
take_heads :: Integer -> [[a]] -> ([a], [[a]])
take_heads i lsls | i <= 0 = ([], lsls)
take_heads i lsls = f i lsls where
    f 0 lsls = ([], lsls)
    f i ((a:ls):lsls) = (a:hs, ls:lsls') where
        (hs, lsls') = f (pred i) lsls
    f i ([]:lsls) = f (pred i) lsls -- skip to avoid infinite search
    f i [] = ([], [])
table2list :: [[a]] -> [a]
table2list lsls = f 1 lsls where
    f _ [] = []
    f i lsls = hs ++ f (succ i) lsls' where
        (hs, lsls') = take_heads i lsls

product2 :: (a->b->c) -> [a] -> [b] -> [[c]]
product2 op ls bs = [map (op a) bs |a <- ls]
product2_ :: (a->b->c) -> [a] -> [b] -> [c]
product2_ op ls bs = table2list $ product2 op ls bs
products :: (a->b->b) -> [[a]] -> [b] -> [b]
products op ass bs = foldr (product2_ op) bs ass
concatT :: [[a]] -> [a]
concatT = concat . transpose

