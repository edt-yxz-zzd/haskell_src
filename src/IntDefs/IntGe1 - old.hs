
module IntDefs.IntGe1 (IntGe1())
where


import IntDefs.IntGe
import Seed.Boxed
import Numeric.Natural
--import IntDefs.UInt
-- import Seed.UnsafeUnpack (unsafe_right)

xxx = 1
--type PInt = XXXInt
type IntGe1 = XXXInt


newtype XXXInt = XXXInt {unXXXInt::Natural}
type IntGeXXX = XXXInt


xxx :: Num a => a

{- for non-UInt
instance IntGe XXXInt where
    labelled_sint_lower_bound = box xxx
    __private_intge_constructor = makeUnsafeFromInteger XXXInt
    __private_intge_destructor = unXXXInt
-}
instance IntGe XXXInt where
instance UIntGe XXXInt where
    labelled_uint_lower_bound = box xxx
    __private_uintge_constructor = makeUnsafeFromNatural XXXInt
    __private_uintge_destructor = unXXXInt




instance Integral XXXInt where
    toInteger = intge_toInteger
    quotRem = intge_quotRem
    divMod = intge_divMod
    quot = intge_quot
    rem = intge_rem
    div = intge_div
    mod = intge_mod
instance Num XXXInt where
    fromInteger = unsafe_intge_fromInteger
    (*) = intge_mul
    (+) = intge_add
    (-) = intge_sub
    abs = id
    signum = const 1
    negate u = error $
        (++) "no negative value for IntGe" . show $
        unlabel u labelled_sint_lower_bound

instance Real XXXInt where
    toRational = toRational . toInteger
instance Eq XXXInt where
    (==) = intge_eq
    (/=) = intge_ne
instance Ord XXXInt where
    (<) = intge_lt
    (<=) = intge_le
    (>) = intge_gt
    (>=) = intge_ge
    compare = intge_compare
    min = intge_min
    max = intge_max


instance Enum XXXInt where
    toEnum = fromIntegral
    fromEnum = fromIntegral
intge_ = undefined


instance Show XXXInt where
    showsPrec = intge_showsPrec__Integral
instance Read XXXInt where
    readsPrec = intge_readsPrec__Integral



