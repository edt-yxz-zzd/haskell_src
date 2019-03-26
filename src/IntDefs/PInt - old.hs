
module IntDefs.PInt (PInt(), IntGe1())
where


import IntDefs.IntGe
import Seed.Boxed
--import Numeric.Natural
--import IntDefs.UInt
-- import Seed.UnsafeUnpack (unsafe_right)

newtype PInt = PInt {unPInt::Integer}
type IntGe1 = PInt

instance IntGe PInt where
    lower_bound = box 1
    __private_intge_constructor = makeUnsafeFromInteger PInt
    __private_intge_destructor = unPInt



instance Integral PInt where
    toInteger = intge_toInteger
    quotRem = intge_quotRem
    divMod = intge_divMod
    quot = intge_quot
    rem = intge_rem
    div = intge_div
    mod = intge_mod
instance Num PInt where
    fromInteger = unsafe_intge_fromInteger
    (*) = intge_mul
    (+) = intge_add
    (-) = intge_sub
    abs = id
    signum = const 1
    negate u = error $
        (++) "no negative value for IntGe" . show $ unlabel u lower_bound

instance Real PInt where
    toRational = toRational . toInteger
instance Eq PInt where
    (==) = intge_eq
    (/=) = intge_ne
instance Ord PInt where
    (<) = intge_lt
    (<=) = intge_le
    (>) = intge_gt
    (>=) = intge_ge
    compare = intge_compare
    min = intge_min
    max = intge_max


instance Enum PInt where
    toEnum = fromIntegral
    fromEnum = fromIntegral
intge_ = undefined


instance Show PInt where
    showsPrec = intge_showsPrec__Integral
instance Read PInt where
    readsPrec = intge_readsPrec__Integral



