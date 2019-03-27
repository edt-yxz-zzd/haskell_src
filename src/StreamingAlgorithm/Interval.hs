
module Interval
    (Interval(..)
    ,IntervalSize(..)
    ,interval_size
    ,wider_than
    )
where

data Interval num = Inside num num | Outside num num
    deriving (Read, Show)
data IntervalSize num = InsideSize num | OutsideSize num
    deriving (Read, Show, Eq)

instance Ord num => Ord (IntervalSize num) where
    InsideSize lhs `compare` InsideSize rhs = compare lhs rhs
    OutsideSize lhs `compare` OutsideSize rhs = compare rhs lhs
    InsideSize _ `compare` OutsideSize _ = LT
    OutsideSize _ `compare` InsideSize _ = GT

_interval_size :: Real num => num -> num -> num
_interval_size a b = b - a
interval_size :: Real num => Interval num -> IntervalSize num
interval_size (Inside a b) = InsideSize $ _interval_size a b
interval_size (Outside a b) = OutsideSize $ _interval_size a b

-- > not ==
wider_than :: Real num => Interval num -> Interval num -> Bool
wider_than lhs rhs = interval_size lhs > interval_size rhs




