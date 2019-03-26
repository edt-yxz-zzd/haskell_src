{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module IntegerSet__FiniteRange
    ( null, full, toRanges
    , LeftIntBound (..)
    , RightIntBound (..)
    , ColoredIntegerSet__FiniteRange -- CIS
    , IntegerSet__FiniteRange, IntS
    , safe_len
    , member
    , lookup_lb, lookup_NegInf, lookup
    , isSubrngOf, isProperSubrngOf
    , isSubsetOf, isProperSubsetOf
    , empty, universal, complement
    , range_singleton, singleton
    , range_null
    , insert
    , Item
    , affect_items
    , delete
    , num_ranges
    , difference
    , unionBy
    , intersectionBy
    , toItems
    , unorder_iter
    , unorder_pops
    )
where

import OpDynTheOnlyValue
import Language.Haskell.TH
{-# LANGUAGE TemplateHaskell #-}
import UInt
import Explain
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Prelude as P
import Prelude hiding (null, lookup)

{-
class ColoredInteger a where
    type ColoredInteger2Color a :: *
    unColoredInteger :: a -> (Integer, ColoredInteger2Color a)
    mkColoredInteger :: (Integer, ColoredInteger2Color a) -> a
-}

data LeftIntBound = LeftExcludeNegInf | LeftInclude Integer
    deriving (Eq, Ord, Read, Show)
data RightIntBound = RightExclude Integer | RightExcludePosInf
    deriving (Eq, Ord, Read, Show)
newtype ColoredIntegerSet__FiniteRange a =
    CIS (Map LeftIntBound (RightIntBound, a))
    deriving (Eq, Ord, Read, Show)

type CIS = ColoredIntegerSet__FiniteRange
type IntegerSet__FiniteRange = CIS UndefinedColor
type IntS = IntegerSet__FiniteRange
data UndefinedColor
instOpDynTheOnlyValue [t|UndefinedColor|]







null :: CIS a -> Bool
null (CIS m) = M.null m
full :: CIS a -> Bool
full (CIS m) = M.size m == 1 && case M.toList m of
    [(LeftExcludeNegInf, (RightExcludePosInf, _))] -> True
    _ -> False
toRanges :: CIS a -> [IntRange]
toRanges (CIS m) = map f $ M.toAscList m where
    f (lb, (rb, _)) = (lb, rb)
_range_len :: IntRange -> UInt
_range_len (LeftInclude i, RightExclude j) = unsafe_from (j-i)
_range_len _ = error "_range_len : inf occur"
safe_len :: CIS a -> Maybe UInt
safe_len s@(CIS m) = if M.null m then Just 0 else
    let (lb1, (rb1, a1)) = M.findMin m
        (lb2, (rb2, a2)) = M.findMax m
    in  if  lb1 == LeftExcludeNegInf ||
            rb2 == RightExcludePosInf then Nothing
        else Just . sum . map _range_len $ toRanges s

member :: Integer -> CIS a -> Bool
member i s = isJust $ lookup i s
type IntRange = (LeftIntBound, RightIntBound)
lookup_lb :: LeftIntBound -> CIS a -> Maybe (IntRange, a)
lookup_lb (LeftInclude i) s = lookup i s
lookup_lb _ s = lookup_NegInf s
lookup_NegInf :: CIS a -> Maybe (IntRange, a)
lookup_NegInf (CIS m) = if M.null m then Nothing else
    let (lb, (rb, a)) = M.findMin m in
    if lb == LeftExcludeNegInf then Just ((lb, rb), a) else
    Nothing
lookup :: Integer -> CIS a -> Maybe (IntRange, a)
lookup i (CIS m) = r where
    lb0 = LeftInclude i
    (m1, may, _) = M.splitLookup lb0 m
    r = case may of
        Just (rb, a) -> Just ((lb0, rb), a)
        Nothing -> r'
    r' = if M.null m1 then Nothing else
         let (lb, (rb, a)) = M.findMax m1 in
         case rb of
         RightExclude j -> if i < j then Just ((lb, rb), a) else Nothing
         RightExcludePosInf -> Just ((lb, rb), a)

-- assume lb <= rb
isSubrngOf :: IntRange -> IntRange -> Bool
isSubrngOf (lb1, rb1) (lb2, rb2) = lb2 <= lb1 && rb1 <= rb2
isProperSubrngOf :: IntRange -> IntRange -> Bool
isProperSubrngOf rng1 rng2 = rng1 /= rng2 && isSubrngOf rng1 rng2

bound_at_left :: RightIntBound -> LeftIntBound -> Bool
bound_at_left (RightExclude i) (LeftInclude j) = i <= j
bound_at_left _ _ = False
at_left :: IntRange -> IntRange -> Bool
at_left (_, rb1) (lb2, _) = bound_at_left rb1 lb2
isSubsetOf :: Eq a => CIS a -> CIS a -> Bool
isSubsetOf (CIS a) (CIS b) = f rs1 rs2 where
    g (lb, (rb, a)) = ((lb, rb), a)
    h = map g . M.toAscList
    rs1 = h a
    rs2 = h b
    f rs1@((rng1, a):rs1') rs2@((rng2, b):rs2') =
        if at_left rng2 rng1 then f rs1 rs2' else
        if a == b && isSubrngOf rng1 rng2 then f rs1' rs2 else
        False

isProperSubsetOf :: Eq a => CIS a -> CIS a -> Bool
isProperSubsetOf a b = a /= b && isSubsetOf a b

empty :: CIS a
empty = CIS M.empty

universal :: a -> CIS a
universal a = CIS $ M.singleton LeftExcludeNegInf (RightExcludePosInf, a)
complement :: a -> CIS a -> CIS a
complement a s = f $ toRanges s where
    f ls = CIS . M.fromDistinctAscList $ g LeftExcludeNegInf ls
    g lb0 ((lb, rb):ls) =
        let ls' = if rb == RightExcludePosInf then []
                  else g (_rb2lb rb) ls
        in  if lb0 < lb then (lb0, (_lb2rb lb, a)) : ls' else ls'



range_singleton :: Integer -> IntRange
range_singleton i = (LeftInclude i, RightExclude (i+1))
singleton :: (Integer, a) -> CIS a
singleton (i, a) = CIS $
    M.singleton (LeftInclude i) (RightExclude (i+1), a)
range_null :: IntRange -> Bool
range_null (LeftInclude i, RightExclude j) = not $ i < j
range_null _ = False

insert :: Eq a => IntRange -> a -> CIS a -> CIS a
-- del rngs ; insert rngs' of size 0-3
insert rng _ s | range_null rng = s
insert rng@(lb0, rb0) a0 s = CIS m'' where
    CIS m = delete rng s
    (m1, m2) = M.split lb0 m
    (dels, insert_item) = f1 $ f2 ([], (lb0, (rb0, a0)))
    f2 x@(dels, (lb, (rb, a))) = if M.null m2 then x else
        let (lb2@(LeftInclude i2), (rb2, a2)) = M.findMin m2
        in  if rb == RightExclude i2 && a == a2
            then (lb2:dels, (lb, (rb2, a))) else x
    f1 x@(dels, (lb, (rb, a))) = if M.null m1 then x else
        let (lb1, (rb1@(RightExclude j1), a1)) = M.findMax m1
        in  if lb == LeftInclude j1 && a == a1
            then (lb1:dels, (lb1, (rb, a))) else x
    m' = foldr M.delete m dels
    m'' = uncurry M.insert insert_item m'



type Item a = (LeftIntBound, (RightIntBound, a))
affect_items :: IntRange -> CIS a -> [Item a]
affect_items rng _ | range_null rng = []
affect_items rng@(lb0, rb0) (CIS m) = ls where
    ls = let (m1, may, m2) = M.splitLookup lb0 m in
            f1 m1 ++ fx may ++ f2 m2
    fx may = case may of
                Just (rb, a) -> [(lb0, (rb, a))]
                _ -> []
    f1 m1 = if M.null m1 then [] else
        let (lb, (rb, a)) = M.findMax m1 in
        if range_null (lb0, rb) then [] else [(lb, (rb, a))]
    f2 m2 = M.toAscList $ case rb0 of
        RightExcludePosInf -> m2
        RightExclude j -> fst $ M.split (LeftInclude j) m2

delete :: IntRange -> CIS a -> CIS a
delete rng@(lb0, rb0) s@(CIS m) = CIS m'' where
    ls = affect_items rng s
    inserts = if P.null ls then mid else hs++mid++ts where
        mid = []
        reinsert_head = ((lb1, lb0), a1)
        reinsert_last = ((rb0, rb2), a2)
        (lb1, (rb1, a1)) = head ls
        (lb2, (rb2, a2)) = last ls
        hs = if not $ lb1 < lb0 then [] else
            let LeftInclude j = lb0 in [(lb1, (RightExclude j, a1))]
        ts = if not $ rb0 < rb2 then [] else
            let RightExclude i = rb0 in [(LeftInclude i, (rb2, a2))]
    m' = foldr M.delete m $ map fst ls
    m'' = foldr (uncurry M.insert) m' inserts

num_ranges :: CIS a -> Int
num_ranges (CIS m) = M.size m
sort2 :: Ord a => (a, a) -> (a, a)
sort2 x@(a,b) = if b < a then (b,a) else x
_lb2rb (LeftInclude i) = RightExclude i
_lb2rb _ = error "_lb2rb LeftExcludeNegInf"
_rb2lb (RightExclude i) = LeftInclude i
_rb2lb _ = error "_rb2lb RightExcludePosInf"
_union_item :: Eq a => (a->a->a) -> Item a -> Item a
    -> (Maybe (Bool, Item a), Item a, Maybe (Bool, Item a))
    -- assume insect not empty
_union_item by (lb1, (rb1, a1)) (lb2, (rb2, a2)) = (e1, e2, e3) where
    (lb1', lb2') = sort2 (lb1, lb2)
    (rb1', rb2') = sort2 (rb1, rb2)
    e1 = if lb1' == lb2' then Nothing else
        let isRight = lb1 /= lb1'
            e = (lb1', (_lb2rb lb2', if isRight then a2 else a1))
        in  Just (isRight, e)
    e3 = if rb1' == rb2' then Nothing else
        let isRight = rb2 == rb2'
            e = (_rb2lb rb1', (rb2', if isRight then a2 else a1))
        in  Just (isRight, e)
    e2 = (lb2', (rb1', by a1 a2))

mergeAscList :: Eq a => [Item a] -> [Item a]
mergeAscList (a:ls) = f a ls where
    f a@(lb1, (RightExclude j1, a1))
        (b@(LeftInclude i2, (rb2, a2)):ls) =
        if j1 == i2 && a1 == a2 then f (lb1, (rb2, a1)) ls
        else a : f b ls
    f a [] = [a]
mergeAscList [] = []
mergeAscListAndMake :: Eq a => [Item a] -> CIS a
mergeAscListAndMake = CIS . M.fromDistinctAscList . mergeAscList

unionBy :: Eq a => (a->a->a) -> CIS a -> CIS a -> CIS a
-- union a b | num_ranges a == 
unionBy by a b = r where
    ls1 = f a
    ls2 = f b
    f (CIS m) = M.toAscList m

    r = mergeAscListAndMake $ union_ls ls1 ls2
    union_ls ls1@(e1@(lb1, (rb1, a1)):ls1')
             ls2@(e2@(lb2, (rb2, a2)):ls2') =
        if bound_at_left rb1 lb2 then e1 : union_ls ls1' ls2 else
        if bound_at_left rb2 lb1 then e2 : union_ls ls1 ls2' else
        let (may_b_x1, x2, may_b_x3) = _union_item by e1 e2
            outs = maybe id ((:) . snd) may_b_x1 [x2]
        in  outs ++ case may_b_x3 of
            Just (isRight, x3) ->
                if isRight then union_ls ls1' (x3:ls2')
                else union_ls (x3:ls1') ls2'
            Nothing -> union_ls ls1' ls2'
difference :: Eq a => CIS a -> CIS a -> CIS a
difference a b = r where
    ls1 = f a
    ls2 = f b
    f (CIS m) = M.toAscList m

    r = mergeAscListAndMake $ diff_ls ls1 ls2
    diff_ls ls1@(e1@(lb1, (rb1, a1)):ls1')
            ls2@(e2@(lb2, (rb2, a2)):ls2') =
        if bound_at_left rb1 lb2 then e1 : diff_ls ls1' ls2 else
        if bound_at_left rb2 lb1 then diff_ls ls1 ls2' else
        let (may_b_x1, x2, may_b_x3) = _union_item const e1 e2
            outs_ = case may_b_x1 of
                Just (isRight, x1) -> if isRight then id else (x1:)
                _ -> id
            outs = outs_ [x2]
        in  outs ++ case may_b_x3 of
            Just (isRight, x3) ->
                if isRight then diff_ls ls1' (x3:ls2')
                else diff_ls (x3:ls1') ls2'
            Nothing -> diff_ls ls1' ls2'

intersectionBy :: Eq a => (a->a->a) -> CIS a -> CIS a -> CIS a
intersectionBy by a b = r where
    ls1 = f a
    ls2 = f b
    f (CIS m) = M.toAscList m

    r = mergeAscListAndMake $ inter_ls ls1 ls2
    inter_ls ls1@(e1@(lb1, (rb1, a1)):ls1')
             ls2@(e2@(lb2, (rb2, a2)):ls2') =
        if bound_at_left rb1 lb2 then inter_ls ls1' ls2 else
        if bound_at_left rb2 lb1 then inter_ls ls1 ls2' else
        let (may_b_x1, x2, may_b_x3) = _union_item by e1 e2
        in  x2 : case may_b_x3 of
            Just (isRight, x3) ->
                if isRight then inter_ls ls1' (x3:ls2')
                else inter_ls (x3:ls1') ls2'
            Nothing -> inter_ls ls1' ls2'



type RangeColor a = (IntRange, a)
toItems :: CIS a -> [Item a]
toItems (CIS m) = M.toAscList m
toRangeColors :: CIS a -> [RangeColor a]
toRangeColors (CIS m) = map f $ M.toAscList m where
    f (lb, (rb, a)) = ((lb, rb), a)
_split_inf_ranges :: CIS a ->
    Either a (Maybe (RightIntBound, a), CIS a, Maybe (LeftIntBound, a))
_split_inf_ranges s@(CIS m)
    | null s = Right (Nothing, s, Nothing)
    | full s =
        let (LeftExcludeNegInf, (RightExcludePosInf, a)) = M.findMin m
        in  Left a
    | otherwise = Right (x1, CIS m', x2) where
    e1 = M.findMin m
    e2 = M.findMax m
    x1 = case e1 of
        (LeftExcludeNegInf, rb_a) -> Just rb_a
        _ -> Nothing
    x2 = case e2 of
        (lb, (RightExcludePosInf, a)) -> Just (lb, a)
        _ -> Nothing
    f1 = maybe id (\_->M.deleteMin) x1
    f2 = maybe id (\_->M.deleteMax) x2
    m' = f1 $ f2 m

iterNegInf2X :: Integer -> [Integer]
iterNegInf2X i = [i-1, i-2..]
iterY2PosInf :: Integer -> [Integer]
iterY2PosInf i = [i..]
iterNegInf2X_Y2PosInf i j
    | i > j = iterNegInf2X_Y2PosInf i i
    | otherwise = merge_2ls (iterNegInf2X i) (iterY2PosInf j)

merge_2ls :: [a] -> [a] -> [a]
merge_2ls (a:s) (b:t) = a:b : merge_2ls s t
merge_2ls s [] = s
merge_2ls [] t = t


unorder_iter :: CIS a -> [(Integer, a)]
unorder_iter s = r where
    info = _split_inf_ranges s
    with a x = (x, a)
    r = case info of
        Left a -> map (with a) $ iterNegInf2X_Y2PosInf 0 0
        Right (may_rb_a, s', may_lb_a) -> iter_finite s' ++
            merge_2ls (f1 may_rb_a) (f2 may_lb_a)

    iter_finite s = concat . map f $ toRangeColors s where
        f ((LeftInclude i, RightExclude j), a) = map (with a) [i..j-1]
    f1 (Just (RightExclude j, a)) = map (with a) $ iterNegInf2X j
    f1 _ = []
    f2 (Just (LeftInclude i, a)) = map (with a) $ iterY2PosInf i
    f2 _ = []

unorder_pops :: CIS a -> [((Integer, a), CIS a)]
unorder_pops s = f s ls where
    ls = unorder_iter s
    f s (x@(i, a):ls) = (x, s') : f s' ls where
        s' = delete (range_singleton i) s
    f s [] = []





{-
    inserts = if P.null ls then mid else hs++mid++ts where
        mid = [(lb0, (rb0, a0))]
        reinsert_head = ((lb1, lb0), a1)
        reinsert_last = ((rb0, rb2), a2)
        (lb1, (rb1, a1)) = head ls
        (lb2, (rb2, a2)) = last ls
        hs = if not $ lb1 < lb0 then [] else
            let LeftInclude j = lb0 in [(lb1, (RightExclude j, a1))]
        ts = if not $ rb0 < rb2 then [] else
            let RightExclude i = rb0 in [(LeftInclude i, (rb2, a2))]
    inserts' = f inserts where
        f (a@(lb1, (rb1, a1)):b@(lb2, (rb2, a2)):ls) =
            if a1 /= a2 then a : f (b:ls) else
            f ((lb1, (rb2, a1)):ls)
    m'' = foldr (uncurry M.insert) m' inserts'








--}
--}
--}
--}
--}
