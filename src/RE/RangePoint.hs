{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module RE.RangePoint where


import Prelude hiding (Bounded)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.List (genericLength, genericIndex, sort, groupBy)
import RE.Bounded


data LeftBound p = LeftIncluded p | LeftExcluded p -- I < E
    deriving (Eq, Ord, Show, Read, Functor)
data RightBound p = RightExcluded p | RightIncluded p -- E < I
    deriving (Eq, Ord, Show, Read, Functor)
unLeftBound :: LeftBound p -> p
unRightBound :: RightBound p -> p
includedLeftBound :: LeftBound p -> Bool
includedRightBound :: RightBound p -> Bool
left2touchedRightBound :: LeftBound p -> RightBound p
right2touchedLeftBound :: RightBound p -> LeftBound p
touchedBound :: Eq p => RightBound p -> LeftBound p -> Bool
    -- [/(.., rb], (lb, ..)/]
    -- [/(.., rb), [lb, ..)/]
touchedBound (RightIncluded p) (LeftExcluded q) = p == q
touchedBound (RightExcluded p) (LeftIncluded q) = p == q
touchedBound _ _ = False
left2touchedRightBound (LeftIncluded p) = RightExcluded p
left2touchedRightBound (LeftExcluded p) = RightIncluded p
right2touchedLeftBound (RightIncluded p) = LeftExcluded p
right2touchedLeftBound (RightExcluded p) = LeftIncluded p

unLeftBound (LeftIncluded p) = p
unLeftBound (LeftExcluded p) = p
unRightBound (RightIncluded p) = p
unRightBound (RightExcluded p) = p
includedLeftBound (LeftIncluded p) = True
includedLeftBound (LeftExcluded p) = False
includedRightBound (RightIncluded p) = True
includedRightBound (RightExcluded p) = False
compare_Point_LeftBound :: Ord p => p -> LeftBound p -> Ordering
compare_Point_RightBound :: Ord p => p -> RightBound p -> Ordering
compare_Point_LeftBound = compare . LeftIncluded
compare_Point_RightBound = compare . RightIncluded

type Range p = (LeftBound p, RightBound p)
    -- assume p < q ==>> there are infinite point between p and q
point2range :: p -> Range p
point2range p = (LeftIncluded p, RightIncluded p)
is_in_range :: Ord p => p -> Range p -> Bool
is_subrange :: Ord p => Range p -> Range p -> Bool
is_empty_range :: Ord p => Range p -> Bool
is_subrange (lb1, rb1) (lb2, rb2) = lb2 <= lb1 && rb1 <= rb2
is_in_range = is_subrange . point2range
is_empty_range (lb, rb) = case unLeftBound lb `compare` unRightBound rb of
    LT -> False -- using assume
    EQ -> not (includedLeftBound lb && includedRightBound rb)
    GT -> True




class (Bounded p, Ord (RawValue p)) => RangePoint p where
    type RawValue p :: *
    -- raw_value2point v < raw_value2point u ==>> v < u
    raw_value2point :: RawValue p -> p
    raw_value2range :: RawValue p -> Range p
    -- raw_value2point v `is_in_range` raw_value2range v
    point2may_raw_value :: p -> Maybe (RawValue p)
    -- q `is_in_range` raw_value2range v ==>> point2may_raw_value q == Just v
    -- not (q `is_in_range` raw_value2range v) ==>> point2may_raw_value q == Nothing
    range2may_raw_value :: Range p -> Maybe (RawValue p)
    -- not $ is_empty_range rng ==/=>> isJust $ range2may_raw_value rng
    -- is_empty_range rng ==>> isNothing $ range2may_raw_value rng
    -- range2may_raw_value rng == Just v ==>> raw_value2point v `is_in_range` rng
    range2may_raw_value rng | is_empty_range rng = Nothing
    range2may_raw_value rng = case rng of
        (LeftIncluded p, RightIncluded q) ->
            if p == q then point2may_raw_value p else f p $ f q r0
        (_, RightIncluded q) -> f q r0
        (LeftIncluded p, _) -> f p r0
        _ -> r0
        where
            f point may0 = maybe may0 Just $ point2may_raw_value point
            r1 = _range2may_raw_value rng
            r0 = do
                v <- r1
                if raw_value2point v `is_in_range` rng then r1
                else Nothing

    _range2may_raw_value :: Range p -> Maybe (RawValue p)
        -- assert "not $ is_empty_range rng"
        -- assert "unLeftBound (fst rng) < unRightBound (snd rng)"


{-
    RawValue
        [0..9], [0..], map neg [0..], Integer
        Char, Natural, String, Integer

-}
mkRightOpenRange :: p -> p -> Range p
mkRightOpenRange a b = (LeftIncluded a, RightExcluded b)
{-
mkClosedRange :: RangePoint p => RawValue p -> RawValue p -> Range p
mkClosedRange from to = (fst $ raw_value2range from, snd $ raw_value2range to)
-}
mkRange :: RangePoint p => RawValue p -> RawValue p -> Range p
mkRange from to = (fst $ raw_value2range from, snd $ raw_value2range to)


mkFullRange :: Bounded p => Range p
mkFullRange = (LeftIncluded min_bound, RightIncluded max_bound)
-- full_range :: Bounded p => Range p

instance RangePoint Char where
    type RawValue Char = Char
    raw_value2point = id
    raw_value2range ch = if ch == max_bound then point2range ch else
        mkRightOpenRange ch (succ ch)
    point2may_raw_value = Just
    _range2may_raw_value (LeftExcluded ch, _) = Just $ succ ch

newtype MinBound2RangePoint n = MinBound2RangePoint (InsertMax n)
    deriving (Eq, Ord, Show, Read, OpMinBound, OpMaxBound, Functor)
newtype NoBound2RangePoint i = NoBound2RangePoint (InsertMinMax i)
    deriving (Eq, Ord, Show, Read, OpMinBound, OpMaxBound, Functor)

instance (Enum n, OpMinBound n) => RangePoint (MinBound2RangePoint n) where
    type RawValue (MinBound2RangePoint n) = n
    raw_value2point = MinBound2RangePoint . InsertMax_Eq
    raw_value2range n = mkRightOpenRange
        (raw_value2point n) (raw_value2point $ succ n)
    point2may_raw_value (MinBound2RangePoint (InsertMax_Eq n)) = Just n
    point2may_raw_value (MinBound2RangePoint InsertMax_Max) = Nothing
    -- (n,n+1) or (n, m) or (n, +oo]\)
    _range2may_raw_value (LeftExcluded (MinBound2RangePoint (InsertMax_Eq n)), _)
        = Just $ succ n
instance (Enum i, Ord i) => RangePoint (NoBound2RangePoint i) where
    type RawValue (NoBound2RangePoint i) = i
    raw_value2point = NoBound2RangePoint . InsertMinMax_Eq
    raw_value2range n = mkRightOpenRange
        (raw_value2point n) (raw_value2point $ succ n)
    point2may_raw_value (NoBound2RangePoint (InsertMinMax_Eq n)) = Just n
    point2may_raw_value (NoBound2RangePoint InsertMinMax_Min) = Nothing
    point2may_raw_value (NoBound2RangePoint InsertMinMax_Max) = Nothing
    -- (n,n+1) or (n, m) or (n, +oo) or (n,+oo]
    _range2may_raw_value (LeftExcluded (NoBound2RangePoint (InsertMinMax_Eq n)), _)
        = Just $ succ n
    -- (-oo, m) or [-oo, m)
    _range2may_raw_value (_, RightExcluded (NoBound2RangePoint (InsertMinMax_Eq n)))
        = Just $ pred n
    -- (/[-oo, +oo]/)
    _range2may_raw_value (noo, poo) =
        case (unLeftBound noo, unRightBound poo) of
            (  NoBound2RangePoint InsertMinMax_Min
             , NoBound2RangePoint InsertMinMax_Max
             ) -> Just $ toEnum 0

-- stronger than (not . is_empty_range)
range_has_raw_value :: RangePoint p => Range p -> Bool
range_has_raw_value = isJust . range2may_raw_value



---------------------



ranges2ordered_ranges :: Ord p => [Range p] -> [Range p]
ranges2ordered_ranges = sort
remove_empty_ranges :: Ord p => [Range p] -> [Range p]
remove_empty_ranges = filter (not . is_empty_range)
group_ranges_by_LeftBound
    :: Ord p => [Range p] -> Map (LeftBound p) (Set (RightBound p))
group_ranges_by_LeftBound = M.fromList . map f . groupBy (\x y-> fst x == fst y)
    where
        f pair1s = (fst $ head pair1s, S.fromList $ map snd pair1s)
leftBound2rightBounds_to_sorted_disjoint_ranges
    :: Ord p => Map (LeftBound p) (Set (RightBound p)) -> [Range p]
leftBound2rightBounds_to_sorted_disjoint_ranges = f where
    f d = case M.minViewWithKey d of
        Nothing -> []
        Just ((lb, rbs), d') ->
            let (rb, rbs') = S.deleteFindMin rbs
                lb' = right2touchedLeftBound rb
                d'' = if null rbs' then d' else M.insertWith S.union lb' rbs' d'
            in  (lb, rb) : f d''
ranges2sorted_disjoint_ranges :: Ord p => [Range p] -> [Range p]
ranges2sorted_disjoint_ranges
    = leftBound2rightBounds_to_sorted_disjoint_ranges
    . group_ranges_by_LeftBound
    . ranges2ordered_ranges . remove_empty_ranges
remove_no_raw_value_ranges :: RangePoint p => [Range p] -> [Range p]
ranges2sorted_disjoint_ranges_HasRawValue :: RangePoint p => [Range p] -> [Range p]
remove_no_raw_value_ranges = filter range_has_raw_value
ranges2sorted_disjoint_ranges_HasRawValue = remove_no_raw_value_ranges
    . ranges2sorted_disjoint_ranges
sorted_disjoint_ranges2full_touched_ranges
    :: Bounded p => [Range p] -> [Range p]
sorted_disjoint_ranges2full_touched_ranges = f (LeftIncluded min_bound)
  where
    f prev_gap_left_bound (rng@(lb, rb) : rngs) =
        (prev_gap_left_bound, left2touchedRightBound lb) : rng :
        f (right2touchedLeftBound rb) rngs
    f prev_gap_left_bound [] =
        [(prev_gap_left_bound, RightIncluded max_bound)]


is_touched_ranges :: Eq p => [Range p] -> Bool
is_full_touched_ranges :: Bounded p => [Range p] -> Bool
is_full_touched_ranges [] = False
is_full_touched_ranges rngs
    =  fst (head rngs) == fst mkFullRange
    && is_touched_ranges rngs
    && snd (last rngs) == snd mkFullRange
is_touched_ranges rngs = len_lt_2 gs where
    gs = groupBy (\r s -> snd r == left2touchedRightBound (fst s)) rngs
    len_lt_2 (a:b:ls) = False
    len_lt_2 _ = True
touched_ranges2touched_ranges_MergeNoRawValueRange
    :: RangePoint p => [Range p] -> [Range p]
touched_ranges2touched_ranges_MergeNoRawValueRange rngs = rngs'' where
    rngss = groupBy (const $ not . range_has_raw_value) rngs
    merge_rng1s rngs = (fst $ head rngs, snd $ last rngs)
    rngs' = map merge_rng1s rngss
    handle_head_ifNoRawValue (rng : rng2 : rngs)
        | not $ range_has_raw_value rng = (fst rng, snd rng2) : rngs
    handle_head_ifNoRawValue rngs = rngs

    -- len < 2 or head has value
    -- head has no value ==>> len == 1
    rngs'' = handle_head_ifNoRawValue rngs'

ranges2full_touched_ranges_MergeNoRawValueRange
    :: RangePoint p => [Range p] -> [Range p]
ranges2full_touched_ranges_MergeNoRawValueRange
    = touched_ranges2touched_ranges_MergeNoRawValueRange
    . sorted_disjoint_ranges2full_touched_ranges
    . ranges2sorted_disjoint_ranges


