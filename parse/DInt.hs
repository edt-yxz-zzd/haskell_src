{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- int not 0
-- denominator
module DInt
    ( DInt -- (..)
    )
where
import Explain as E
import NumOp as N
import SeedUtils (unjust, justif, jcheck, Test(..), type_as, map2, lift2)
import Prelude as P
import ReadHelper__DInt (readsPrec_viaDInt)
import PInt
instance Explain DInt PInt where
instance OpSafeFrom PInt DInt where
instance OpFrom PInt DInt where
    from p = DInt (E.explain p)





newtype DInt = DInt Integer -- nonnegative
    deriving (Eq, Ord, Show)


instance Read DInt where
    readsPrec d s = readsPrec_viaDInt unsafe_from d s


instance Test DInt where
    test _ = True
--_min :: Integer
--_min = 0
_fine :: Integer -> Bool
_fine = test

--instance LimitedFrom Integer DInt where
--    limited_from i = DInt $ max _min i
instance OpSafeFrom Integer DInt where
    safe_from i = justif (_fine i) $ DInt i
instance OpSafeFrom DInt Integer where
instance OpFrom DInt Integer where
    from (DInt i) = i
instance Explain Integer DInt where


_into f (DInt a) = f a
_lift f = DInt . _into f
_into2 op (DInt a) (DInt b) = (op a b)
_lift2 op a b = DInt $ _into2 op a b
_lift2__list op a b = map DInt $ _into2 op a b

instance OpSafeAdd DInt where
    DInt a `safe_add` DInt b = safe_from $ a P.+ b
    unsafe_add a = unjust . safe_add a

instance OpSafeSub DInt where
    DInt a `safe_sub` DInt b = safe_from $ a P.- b
    unsafe_sub a = unjust . safe_sub a



instance OpSafeMul DInt
instance OpLimitedMul DInt
instance OpMul DInt where
    mul = _lift2 (P.*)



-- avoid 0 result
instance OpSafeFloorDivEx DInt DInt DInt where
    safe_floor_div_ex n d = safe_from $ _into2 P.div n d
instance OpSafeModEx DInt DInt DInt where
    safe_mod_ex n d = safe_from $ _into2 P.mod n d
instance OpSafeDivModEx DInt DInt DInt DInt where
    safe_divmod_ex n d = map2 safe_from $ _into2 P.divMod n d



instance OpSafeFloorDivEx Integer DInt DInt where
instance OpSafeModEx Integer DInt DInt where
instance OpFloorDivEx Integer DInt DInt where
    floor_div_ex n d = _into2 P.div n d
instance OpModEx Integer DInt DInt where
    mod_ex n d = _into2 P.mod n d
instance OpSafeDivModEx Integer Integer DInt DInt where
    safe_divmod_ex n d = map2 Just $ divmod_ex n d
    unsafe_divmod_ex = divmod_ex
instance OpDivModEx Integer Integer DInt DInt where
    divmod_ex n d = _into2 P.divMod n d

-- for DynIntegral Integer
instance OpSafeFloorDivEx Integer Integer DInt where
instance OpSafeModEx Integer Integer DInt where
instance OpFloorDivEx Integer Integer DInt where
    floor_div_ex n (DInt d) = P.div n d
instance OpModEx Integer Integer DInt where
    mod_ex n (DInt d) = P.mod n d
instance OpSafeDivModEx Integer Integer Integer DInt where
    safe_divmod_ex n d = map2 Just $ divmod_ex n d
    unsafe_divmod_ex = divmod_ex

instance OpDivModEx Integer Integer Integer DInt where
    divmod_ex n (DInt d) = P.divMod n d
instance OpSafeDivEx Rational Integer DInt where
instance OpDivEx Rational Integer DInt where
    div_ex n (DInt d) = lift2 P.toRational (P./) n d


-- for DynIntegralD DInt
instance OpSafeAbs DInt where
instance OpLimitedAbs DInt where
instance OpAbs DInt where
    abs = P.abs
instance OpSafeNeg DInt where
instance OpLimitedNeg DInt where
instance OpNeg DInt where
    neg = P.negate
instance OpSafeGcd DInt where
instance OpGcd DInt where
    gcd = P.gcd
instance OpSafeDivEx DInt DInt DInt where
    safe_div_ex n d = _into2 safe_div_ex n d >>= return . DInt
instance OpSafeDivEx Rational DInt DInt where
instance OpDivEx Rational DInt DInt where
    div_ex (DInt n) (DInt d) = lift2 P.toRational (P./) n d




--------------------  Prelude Num / Real / Enum / Integral
instance Num DInt where
    (+) = (N.unsafe_add) -- (N.+)
    (*) = (N.*)
    (-) = (N.unsafe_sub)
    abs = _lift P.abs
    negate = _lift negate
    signum = _lift signum
    fromInteger = E.unsafe_from


instance Real DInt where
    toRational (DInt i) = toRational i


instance Enum DInt where
    succ (DInt d) = unsafe_from $ succ d
    pred (DInt d) = unsafe_from $ pred d
    toEnum = E.unsafe_from . P.toInteger
    fromEnum = P.fromInteger . E.explain
    enumFrom (DInt i) = map DInt $ if i < 0 then [i.. -1] else [i..]
    enumFromThen (DInt a) (DInt b) = map DInt $
        if a > b && b > 0 && meet0 then [a, b..1] else
        if a < b && b < 0 && meet0 then [a, b.. -1]
        else [a, b..]
      where meet0 = 0 == P.mod b (a P.- b)
    enumFromTo (DInt a) (DInt b) = map DInt $
        if a < 0 && 0 < b then [a.. -1]
        else [a..b]
    enumFromThenTo (DInt a) (DInt b) (DInt c) = map DInt $
        if a > b && b > 0 && 0 > c && meet0 then [a, b..1] else
        if a < b && b < 0 && 0 < c && meet0 then [a, b.. -1]
        else [a, b..c]
      where meet0 = 0 == P.mod b (a P.- b)

instance Integral DInt where
    toInteger = E.explain
    -- quotRem n d = let (q, r) = _into2 quotRem n d in (DInt q, DInt r)
    quotRem n d = map2 unsafe_from $ _into2 quotRem n d
    divMod n d = map2 unsafe_from $ _into2 divMod n d







b1 = (take 3 [1, 4..] :: [DInt]) == [1, 4, 7]
b2 = (take 3 [4, 1..] :: [DInt]) == [4, 1]

b = all id [b1, b2]
main = do
    print "DInt"
    print b



