-- Num for fromInteger
-- Fractional for (/), Num
-- Real for Ord, Num
module Interval4
    (Interval4() -- ()
    ,unInterval4
    ,unsafe_mkInterval4
    ,interval4_flip
    ,interval_to_interval4
    ,interval_ex_to_interval4
    ,interval4_to_interval_ex
    ---------------
    ---------------
    ,CyclePreservedTransform(..)
    ,CyclePreservedBiTransform(..)
    ,cycle_preserved_transform2interval_transfrom
    ,cycle_preserved_bitransform2interval_bitransfrom_ex
    )
where

import Interval
import IntervalEx
import Cycle
import qualified Data.List as Ls (sort, elemIndex, foldr1)

type Pair a = (a, a)
type Triple a = (a, a, a)
type Tuple4 a = (a,a,a,a)
unsafe_fromList_Tuple4 :: [a] -> Tuple4 a
unsafe_fromList_Tuple4 [a,b,c,d] = (a,b,c,d)
unsafe_fromList_Tuple4 _ = error "unsafe_fromList_Tuple4"
toList_Tuple4 :: Tuple4 a -> [a]
toList_Tuple4 (a,b,c,d) = [a,b,c,d]
is_clockwises4 :: Cycle a => Tuple4 a -> Bool
is_clockwises4 = is_clockwises . toList_Tuple4
is_cyclewises4 :: Cycle a => Tuple4 a -> Bool
is_cyclewises4 = is_cyclewises . toList_Tuple4



newtype Interval4 num -- private
    = Private_Clockwise (Tuple4 (Maybe num))
    -- (a,b,c,d) clockwise
    -- (a->b->c) is the interval
    -- (c->d->a) is the complement
unInterval4 :: Interval4 num -> Tuple4 (Maybe num)
unInterval4 (Private_Clockwise t) = t
unsafe_mkInterval4 :: Ord num => Tuple4 (Maybe num) -> Interval4 num
unsafe_mkInterval4 t4@(a,b,c,d) =
    if not $ is_clockwises4 t4
    then error "unsafe_mkInterval4 not clockwise"
    else if a==b&&b==c&&c==d
    then error "unsafe_mkInterval4 SHOULD NOT be all the same"
    else Private_Clockwise t4


interval4_flip :: Interval4 num -> Interval4 num
interval4_flip (Private_Clockwise (a,b,c,d)) = Private_Clockwise (c,d,a,b)
interval_to_interval4
    :: (Real num, Fractional num)
    => Interval num -> Interval4 num
interval_to_interval4 = f where
    f (Inside a b) = unsafe_mkInterval4 $ case compare a b of
        EQ -> let m=Just a in (m,m,m, Nothing)
        LT -> g a b
        GT -> g b a
    f (LessEq a) = unsafe_mkInterval4
        (Nothing, Just $ a-1, Just a, Just $ a+1)
    f (Outside a b) = interval4_flip $ f (Inside a b)
    f (GreaterEq a) = interval4_flip $ f (LessEq a)
    g a b = (Just a, Just $ (a+b)/2, Just $ b, Nothing)

interval_ex_to_interval4
    :: (Real num, Fractional num)
    => IntervalEx num -> Interval4 num
interval_ex_to_interval4 (IntervalEx interval)
    = interval_to_interval4 interval
interval_ex_to_interval4 Inside_oo_oo
    = unsafe_mkInterval4 (Nothing, Nothing, Nothing, Just 0)
interval_ex_to_interval4 Outside_oo_oo
    = unsafe_mkInterval4 (Nothing, Just 0, Nothing, Nothing)


interval4_to_interval_ex
    :: Ord num => Interval4 num -> IntervalEx num
interval4_to_interval_ex interval4 = case unInterval4 interval4 of
    -- clockwise
    (a, b, c, d) -> case compare a c of
        LT -> mk_inside a c
        GT -> if c==Nothing then mk_inside a c else mk_outside c a
        EQ -> if a==b then mk_inside a c else mk_outside a c
    where
        mk_inside (Just a) (Just c) = IntervalEx $ Inside a c
        mk_inside (Just a) Nothing = IntervalEx $ GreaterEq a
        mk_inside Nothing (Just c) = IntervalEx $ LessEq c
        mk_inside Nothing Nothing = Inside_oo_oo

        mk_outside (Just a) (Just c) = IntervalEx $ Outside a c
        mk_outside Nothing Nothing = Outside_oo_oo
        mk_outside _ _ = error "logic error: mk_outside"









type Transfrom a = a -> a
type BiTransfrom a = a -> a -> a
type BiTransfromEx a b = a -> a -> (a, b)
newtype CyclePreservedTransform num
    = CyclePreservedTransform (Transfrom (Maybe num))
newtype CyclePreservedBiTransform num
    = CyclePreservedBiTransform (BiTransfrom (Maybe num))




cycle_preserved_transform2interval_transfrom
    :: (Real num, Fractional num)
    => CyclePreservedTransform num -> Transfrom (IntervalEx num)
cycle_preserved_transform2interval_transfrom
    cycle_preserved_transform interval_ex = result where
    CyclePreservedTransform transfrom = cycle_preserved_transform
    -- fix bug:
    transfrom0 = transfrom $ Just 0
    transfrom1 = transfrom $ Just 1
    is_constant_transfrom = transfrom0 == transfrom1

    interval4 = interval_ex_to_interval4 interval_ex
    interval4' = unsafe_mkInterval4 $ unsafe_fromList_Tuple4 r4
    result = if not is_constant_transfrom
        then interval4_to_interval_ex interval4'
        else case transfrom0 of
            Nothing -> Inside_oo_oo
            Just r -> IntervalEx (Inside r r)

    result4 = fmap transfrom . toList_Tuple4 $ unInterval4 interval4
    result4' = let [a,b,c,d] = result4 in [c,b,a,d]
    r4 = if is_clockwises $ take 3 result4
        then if not $ is_clockwises result4
            then error "is_clockwises result4"
            else result4
        else if not $ is_clockwises result4'
            then error "is_clockwises result4\'"
            else result4'

{-
bug: fixed!
    cycle_preserved_transform2interval_transfrom
        donot allow "constant transfrom"
-}

cycle_preserved_bitransform2interval_bitransfrom_ex
    :: (Real num, Fractional num)
    => CyclePreservedBiTransform num
    -> BiTransfromEx (IntervalEx num) Bool
    -- selector :: Bool
    --  False -> update x; since FixY product the max ranges
    --  True -> update y
cycle_preserved_bitransform2interval_bitransfrom_ex
    cycle_preserved_bitransform interval_ex_X interval_ex_Y
    = (interval_ex_Z, selector) where
    -- interval_ex_Z
    -- selector
    (xm, xM) = interval_ex_to_boundaries interval_ex_X
    (ym, yM) = interval_ex_to_boundaries interval_ex_Y
    CyclePreservedBiTransform bitransfrom = cycle_preserved_bitransform

    fix_ym = CyclePreservedTransform $ \x-> bitransfrom x ym
    fix_yM = CyclePreservedTransform $ \x-> bitransfrom x yM
    fix_xm = CyclePreservedTransform $ \y-> bitransfrom xm y
    fix_xM = CyclePreservedTransform $ \y-> bitransfrom xM y

    f = cycle_preserved_transform2interval_transfrom
    interval_ex_Z__ym = f fix_ym interval_ex_X
    interval_ex_Z__yM = f fix_yM interval_ex_X
    interval_ex_Z__xm = f fix_xm interval_ex_Y
    interval_ex_Z__xM = f fix_xM interval_ex_Y

    -- the order is important
    interval_ex_Z = unsafe_union_touched_interval_exs
        [interval_ex_Z__xm, interval_ex_Z__ym
        ,interval_ex_Z__xM, interval_ex_Z__yM
        ]
    selectorY = True
    selectorX = False
    selector = if i <= 1 then selectorY else selectorX
    (Just i) = Ls.elemIndex max_size rng_sizes
    max_size = maximum rng_sizes
    rng_sizes = map interval_ex_size
        [interval_ex_Z__xm, interval_ex_Z__xM -- max ==>> y
        ,interval_ex_Z__ym, interval_ex_Z__yM -- max ==>> x
        ]












-- partial_transfrom_ONEvar_strictly_monotone1var_maybe_1pole
-- partial_transfrom ONEvar strictly_monotone1var maybe_1pole
--  partial: pole <==> Nothing
--  ONEvar - one input variables
--  strictly_monotone1var - fix other variables, strictly_monotone for the only variable
--
--
--

{-
data FixX
data FixY
interval_call4partial_transfrom_ONEvar_strictly_monotone1var_maybe_1pole
    :: (Real num, Fractional num)
    => Maybe num -> (num -> Maybe num)
    -> Interval num -> IntervalEx num

interval_call4partial_transfrom_TWOvar_strictly_monotone1var_maybe_1pole
    :: (Real num, Fractional num)
    => (num -> num -> Maybe num)
    -> (Maybe num, num -> FixY -> Maybe num)
    -> (Maybe num, FixX -> num -> Maybe num)
    -> Interval num -> Interval num
    -> (IntervalEx num, Bool)
        -- selector::Bool
        --  False -> update x; since FixY product the max ranges
        --  True -> update y
-}

{-
interval_call4partial_transfrom_ONEvar_strictly_monotone1var_maybe_1pole
    output_at_oo partial_transfrom = this where
    this interval
        = case interval of
            Inside a b -> fInside a b
            LessEq a -> fLessEq a
            Outside a b -> interval_flip_ex $ fInside a b
            GreaterEq a -> interval_flip_ex $ fLessEq a

    fLessEq a = f3_mmm
                    output_at_oo
                    (partial_transfrom $ a-1)
                    (partial_transfrom a)
    fInside a b
        | a==b = fAt a
        | otherwise = f3_mmm
                    (partial_transfrom a)
                    (partial_transfrom $ (a+b)/2)
                    (partial_transfrom b)
    fAt a = maybe Inside_oo_oo f $ partial_transfrom a
        where f r = IntervalEx $ Inside r r
    f3_mmm mL mMID mR = case mR of
        Just r  -> f3_mmn mL mMID r
        Nothing -> f2_mmX mL mMID
    f2_mmX mL mMID = case mL of
        Just r  -> f2_nmX r mMID
        Nothing -> error "logic-error: two pole"
    f3_mmn mL mMID r = case mL of
        Just r' -> f3_nmn r' mMID r
        Nothing -> f2_nmX r mMID
    f3_nmn rL mMID rR = IntervalEx $ case mMID of
        Just rMID -> if (rL<rMID) == (rMID<rR)
                    then inside else outside
        Nothing -> outside -- contain oo
        where
            (rMIN, rMAX) = if rL < rR then (rL, rR) else (rR, rL)
            inside = Inside rMIN rMAX
            outside = Outside rMIN rMAX
    f2_nmX r mMID = IntervalEx $ case mMID of
        Just rMID -> if rMID < r then LessEq r else GreaterEq r
        Nothing -> error "logic-error: two pole"
-}



{-
interval_call4partial_transfrom_TWOvar_strictly_monotone1var_maybe_1pole
    partial_transfrom_xy
    (output_at_oo_Y, partial_transfrom_xY) -- fix y
    (output_at_X_oo, partial_transfrom_Xy) -- fix x
    = this where
    this interval_x interval_y = case interval_x of
-}



