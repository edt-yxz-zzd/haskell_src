
{-
-- Num for fromInteger
-- Fractional for (/), Num
-- Real for Ord, Num


requires:
    CyclePreservedTransform
        should be strictly monotone
        should not be const!
        bijective!
        !!!avoid LinearFractionalTransformation[0,_;0,_]!!!
        cycle' = map CyclePreservedTransform cycle
        cycle preserved but not clockwiseness
    CyclePreservedBiTransform
        f x = CyclePreservedBiTransform x FixY
        g y = CyclePreservedBiTransform FixX y
        ==>> f,g are CyclePreservedTransform
-}


module Interval
    (Interval(..)
    ,IntervalSize(..)
    ,interval_size
    ,wider_than
    ,interval_flip
    ,interval_to_boundaries
    )
where


type Pair a = (a,a)

data Interval num = Inside num num | LessEq num | GreaterEq num | Outside num num
    deriving (Read, Show)
data IntervalSize num = InsideSize num | HalfSpace | OutsideSize num
    deriving (Read, Show, Eq)

instance Ord num => Ord (IntervalSize num) where
    InsideSize lhs `compare` InsideSize rhs = compare lhs rhs
    OutsideSize lhs `compare` OutsideSize rhs = compare rhs lhs -- reverse
    HalfSpace `compare` HalfSpace = EQ

    InsideSize _ `compare` _ = LT
    _ `compare` InsideSize _ = GT
    _ `compare` OutsideSize _ = LT
    OutsideSize _ `compare` _ = GT

_interval_size :: Real num => num -> num -> num
_interval_size a b = b - a
interval_size :: Real num => Interval num -> IntervalSize num
interval_size (Inside a b) = InsideSize $ _interval_size a b
interval_size (Outside a b) = OutsideSize $ _interval_size a b
interval_size _ = HalfSpace

-- > not ==
wider_than :: Real num => Interval num -> Interval num -> Bool
wider_than lhs rhs = interval_size lhs > interval_size rhs


interval_flip :: Interval num -> Interval num
interval_flip (Inside a b) = Outside a b
interval_flip (Outside a b) = Inside a b
interval_flip (LessEq a) = GreaterEq a
interval_flip (GreaterEq a) = LessEq a

interval_to_boundaries :: Interval num -> Pair (Maybe num)
interval_to_boundaries (Inside a b) = (Just a, Just b)
interval_to_boundaries (Outside a b) = (Just a, Just b)
interval_to_boundaries (LessEq a) = (Nothing, Just a)
interval_to_boundaries (GreaterEq a) = (Just a, Nothing)











