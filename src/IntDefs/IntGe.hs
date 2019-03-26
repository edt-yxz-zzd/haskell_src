{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}




module IntDefs.IntGe
    ( SInt
    , IntGe (..), UIntGe(..)
    --, UnsafeFromInteger ()
    , makeUnsafeFromInteger
    --, UnsafeFromNatural ()
    , makeUnsafeFromNatural
    , intge_toInteger, unsafe_intge_fromInteger, makeIntGe
    , uintge_toNatural, unsafe_uintge_fromNatural, makeUIntGe
    , intge_showsPrec__Integral, intge_readsPrec__Integral
    , intge_eq, intge_ne, intge_lt, intge_le, intge_gt, intge_ge
    , intge_compare
    , intge_add, intge_sub, intge_mul, intge_div
        , intge_mod, intge_quot, intge_rem, intge_min, intge_max
    , intge_quotRem, intge_divMod

    --, intge_
    )
where

import Seed.UnsafeUnpack (unsafe_unright)
import Seed.Boxed
import Numeric.Natural

type SInt = Integer


newtype UnsafeFromInteger i =
    UnsafeFromInteger__private { runUnsafeFromInteger :: (Integer -> i)}
makeUnsafeFromInteger :: (Integer->i) -> UnsafeFromInteger i
makeUnsafeFromInteger = UnsafeFromInteger__private
newtype UnsafeFromNatural i =
    UnsafeFromNatural { runUnsafeFromNatural :: (Natural -> i)}
makeUnsafeFromNatural :: (Natural->i) -> UnsafeFromNatural i
makeUnsafeFromNatural = UnsafeFromNatural


class IntGe i where
    labelled_sint_lower_bound :: Label i Integer
    __private_intge_constructor :: UnsafeFromInteger i
    __private_intge_destructor :: i -> Integer

    intge_lower_bound :: i
    intge_lower_bound = x where
        x = unsafe_intge_fromInteger $ unlabel x labelled_sint_lower_bound

    default labelled_sint_lower_bound :: UIntGe i => Label i Integer
    default __private_intge_constructor :: UIntGe i => UnsafeFromInteger i
    default __private_intge_destructor :: UIntGe i => i -> Integer
    labelled_sint_lower_bound = fmap toInteger labelled_uint_lower_bound
    __private_intge_constructor = makeUnsafeFromInteger $
        runUnsafeFromNatural __private_uintge_constructor . fromInteger
    __private_intge_destructor = toInteger . __private_uintge_destructor
class IntGe i => UIntGe i where
    labelled_uint_lower_bound :: Label i Natural
    labelled_uint_lower_bound = fmap fromInteger labelled_sint_lower_bound
    __private_uintge_constructor :: UnsafeFromNatural i
    __private_uintge_destructor :: i -> Natural



intge_toInteger :: IntGe i => i -> Integer
intge_toInteger = __private_intge_destructor
uintge_toNatural :: UIntGe i => i -> Natural
uintge_toNatural = __private_uintge_destructor
unsafe_intge_fromInteger :: IntGe i => Integer -> i
unsafe_intge_fromInteger = unsafe_unright . makeIntGe
unsafe_uintge_fromNatural :: UIntGe i => Natural -> i
unsafe_uintge_fromNatural = unsafe_unright . makeUIntGe

makeIntGe :: (Monad m, Integral i, IntGe u) => i -> m u
makeIntGe a
    | i < lower = fail $
        "makeIntGe i where i = " ++ show i ++ " < " ++ show lower
    | otherwise = return u
    where
        i = toInteger a
        u = runUnsafeFromInteger __private_intge_constructor i
        lower = unlabel u labelled_sint_lower_bound

makeUIntGe :: (Monad m, Integral i, UIntGe u) => i -> m u
makeUIntGe a
    | i < lower = fail $
        "makeUIntGe i where i = " ++ show i ++ " < " ++ show lower
    | otherwise = return u
    where
        i = fromIntegral a :: Natural
        u = runUnsafeFromNatural __private_uintge_constructor i
        lower = unlabel u labelled_uint_lower_bound


{-
intge_typename :: forall u. IntGe u => Label u String
intge_typename = fmap f labelled_sint_lower_bound where
    f i = "IntGe" ++ show i :: String
to_intge_typename :: IntGe u => u -> String
to_intge_typename u = unlabel u intge_typename

intge_showsPrec__raw :: IntGe u => Int -> u -> ShowS
intge_showsPrec__raw d u = showParen (d > app_prec) $
    showString "unsafe_intge_fromInteger "
    . showsPrec (app_prec+1) (intge_toInteger u)
    where app_prec = 10
-}
intge_showsPrec__Integral :: (IntGe u, Integral u) => Int -> u -> ShowS
intge_showsPrec__Integral d u = showsPrec d (toInteger u)
intge_readsPrec__Integral :: (IntGe u, Integral u) => Int -> ReadS u
intge_readsPrec__Integral d = map f . readsPrec d where
        f (i, s) = (fromInteger i, s)






intge_op__viaSInt_ex :: IntGe i => (SInt -> SInt -> a) -> i -> i -> a
intge_op__viaSInt_ex op a b = intge_toInteger a `op` intge_toInteger b
intge_eq, intge_ne, intge_lt, intge_le, intge_gt, intge_ge
    :: IntGe i => i -> i -> Bool
intge_compare :: IntGe i => i -> i -> Ordering
intge_compare = intge_op__viaSInt_ex compare
intge_eq = intge_op__viaSInt_ex (==)
intge_ne = intge_op__viaSInt_ex (/=)
intge_lt = intge_op__viaSInt_ex (<)
intge_le = intge_op__viaSInt_ex (<=)
intge_gt = intge_op__viaSInt_ex (>)
intge_ge = intge_op__viaSInt_ex (>=)






intge_op__viaSInt :: IntGe i => (SInt -> SInt -> SInt) -> i -> i -> i
intge_op__viaSInt op a b = unsafe_intge_fromInteger $
                            intge_op__viaSInt_ex op a b
intge_add, intge_sub, intge_mul, intge_div
    , intge_mod, intge_quot, intge_rem, intge_min, intge_max
    :: IntGe i => i -> i -> i
intge_ = undefined
intge_min = intge_op__viaSInt min
intge_max = intge_op__viaSInt max
intge_add = intge_op__viaSInt (+)
intge_sub = intge_op__viaSInt (-)
intge_mul = intge_op__viaSInt (*)
intge_div = intge_op__viaSInt div
intge_mod = intge_op__viaSInt mod
intge_quot = intge_op__viaSInt quot
intge_rem = intge_op__viaSInt quot

intge_op2__viaSInt
    :: IntGe i => (SInt -> SInt -> (SInt, SInt)) -> i -> i -> (i, i)
intge_op2__viaSInt op a b =
    (unsafe_intge_fromInteger s, unsafe_intge_fromInteger t)
    where (s, t) = intge_op__viaSInt_ex op a b

intge_quotRem, intge_divMod :: IntGe i => i -> i -> (i,i)
intge_quotRem = intge_op2__viaSInt quotRem
intge_divMod = intge_op2__viaSInt divMod






-------------------------------------------------



