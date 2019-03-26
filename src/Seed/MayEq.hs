

module Seed.MayEq
where

data TriBool = TFalse | TUnknown | TTrue
    deriving (Eq, Ord, Show, Read)
class MayEq a where
    (=?=), may_eq :: a -> a -> TriBool
    (=?=) = may_eq
    may_eq = (=?=)
    {-# MINIMAL (=?=) | may_eq #-}
infix 4 =?=

tribool :: a -> a -> a -> TriBool -> a
tribool f u t TFalse = f
tribool f u t TUnknown = u
tribool f u t TTrue = t

ifTri :: TriBool -> a -> a -> a -> a
ifTri b f u t = tribool f u t b









