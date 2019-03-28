
module IntervalEx
    (RealEx(..)
    ,IntervalEx(..)
    ,interval_ex_size
    ,wider_than_ex
    ,interval_flip_ex
    ,interval_ex_to_boundaries
    ---------------
    ,unsafe_real_ex_pair2interval_ex
    ,interval_ex2real_ex_pairs
    ,interval2real_ex_pairs
    -- ,merge_sorted_ordered_pairs
    ,unsafe_union_touched_interval_exs
    )
where


import Interval
import qualified Data.List as Ls (sort, elemIndex, foldr1)
type Pair a = (a,a)

data RealEx num = N_oo | TheNum num | P_oo
    deriving (Read, Show, Eq, Ord)
data IntervalEx num = Inside_oo_oo | Outside_oo_oo | IntervalEx (Interval num)
    deriving (Read, Show)
interval_ex_size :: Real num => IntervalEx num -> IntervalSize num
interval_ex_size (IntervalEx interval) = interval_size interval
interval_ex_size Inside_oo_oo = InsideSize 0
interval_ex_size Outside_oo_oo = OutsideSize 0
wider_than_ex :: Real num => IntervalEx num -> IntervalEx num -> Bool
wider_than_ex lhs rhs = interval_ex_size lhs > interval_ex_size rhs




interval_flip_ex :: IntervalEx num -> IntervalEx num
interval_flip_ex (IntervalEx interval) = IntervalEx (interval_flip interval)
interval_flip_ex Inside_oo_oo = Outside_oo_oo
interval_flip_ex Outside_oo_oo = Inside_oo_oo

interval_ex_to_boundaries :: IntervalEx num -> Pair (Maybe num)


interval_ex_to_boundaries (IntervalEx interval)
    = interval_to_boundaries interval
interval_ex_to_boundaries Inside_oo_oo = (Nothing, Nothing)
interval_ex_to_boundaries Outside_oo_oo = (Nothing, Nothing)


---------------------------------------
assert_ex :: String -> Bool -> a -> a
assert_ex msg b a = if b then a else error msg

unsafe_real_ex_pair2interval_ex
    :: Real num => Pair (RealEx num) -> IntervalEx num
unsafe_real_ex_pair2interval_ex (a,b) = if not $ a <= b
    then error "unsafe_real_ex_pair2interval_ex (a,b) but not a<=b"
    else case (a,b) of
        (TheNum x, TheNum y) -> IntervalEx $ Inside x y
        (TheNum x, P_oo) -> IntervalEx $ GreaterEq x
        (N_oo, TheNum y) -> IntervalEx $ LessEq y
        (N_oo, P_oo) -> Outside_oo_oo
        (N_oo, N_oo) -> Inside_oo_oo
        (P_oo, P_oo) -> Inside_oo_oo
        _ -> error "unsafe_real_ex_pair2interval_ex unknown case"
    where
        err = error "logic-error: unsafe_real_ex_pair2interval_ex: inside_oo_oo"


interval_ex2real_ex_pairs
    :: Ord num => IntervalEx num -> [Pair (RealEx num)]
interval_ex2real_ex_pairs interval_ex = case interval_ex of
    IntervalEx interval -> interval2real_ex_pairs interval
    Inside_oo_oo -> [(N_oo, N_oo), (P_oo,P_oo)]
    Outside_oo_oo -> [(N_oo, P_oo)]


interval2real_ex_pairs
    :: Ord num => Interval num -> [Pair (RealEx num)]
interval2real_ex_pairs = f where
    f (Inside a b) = [(TheNum a', TheNum b')]
        where (a', b') = minmax (a,b)
    f (Outside a b) = [(N_oo, TheNum a'), (TheNum b', P_oo)]
        where (a', b') = minmax (a,b)
    f (LessEq a) = [(N_oo, TheNum a)]
    f (GreaterEq a) = [(TheNum a, P_oo)]
minmax (a,b) = if a <= b then (a,b) else (b,a)



merge_sorted_ordered_pairs :: Ord a => [(a,a)] -> [(a,a)]
-- precondition: all (\(a,b)->a<=b) pairs && sort pairs == pairs
merge_sorted_ordered_pairs (h:ts) = f h ts where
    f pair@(a,b) (h@(c,d):pairs)
        = if c <= b then f (a,d) pairs else pair : f h pairs
    f pair [] = [pair]
merge_sorted_ordered_pairs [] = []

unsafe_union_touched_interval_exs
    :: Real num => [IntervalEx num] -> IntervalEx num
unsafe_union_touched_interval_exs [] = error "unsafe_union_touched_interval_exs: []"
unsafe_union_touched_interval_exs interval_exs = result where
    _1_pairs = merge_sorted_ordered_pairs . Ls.sort $ do
        interval_ex <- interval_exs
        interval_ex2real_ex_pairs interval_ex
    _last = last _1_pairs
    pairs = if head _1_pairs == (N_oo, N_oo) && snd _last == P_oo
                then tail _1_pairs
                else if _last == (P_oo, P_oo) && fst (head _1_pairs) == N_oo
                then init _1_pairs
                else _1_pairs

    err = error "unsafe_union_touched_interval_exs: not touch"
    result = case map unsafe_real_ex_pair2interval_ex pairs of
        [interval_ex] -> interval_ex
        [outL, outR] -> case (outL, outR) of
            (IntervalEx (LessEq a), IntervalEx (GreaterEq b))
                -> IntervalEx $ Outside a b
            _ -> err
        [] -> error "unsafe_union_touched_interval_exs [] -- input should not be empty"
        _ -> err




