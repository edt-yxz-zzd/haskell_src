{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module PInt
    ( PInt -- (..)
    )
where
import Explain as E
import NumOp as N
import SeedUtils (unjust, justif, jcheck, Test(..), type_as, map2, lift2)
import Prelude as P
import ReadHelper__PInt (readsPrec_viaPInt)

newtype PInt = PInt Integer -- nonnegative
    deriving (Eq, Ord, Show)


instance Read PInt where
    readsPrec d s = readsPrec_viaPInt unsafe_from d s


{-
newtype ReadPInt = ReadPInt Integer
    deriving (Read)
instance Read PInt where
    readsPrec n s = do
        (ReadPInt i, tail) <- readsPrec n ("Read"++s)
        if i < _min
        then []
        else return (PInt i, tail)
-}

instance Test PInt where
    test _ = True
_min :: Integer
_min = 1
_fine :: Integer -> Bool
_fine = (>= _min)

instance LimitedFrom Integer PInt where
    limited_from i = PInt $ max _min i
instance OpSafeFrom Integer PInt where
    safe_from i = justif (_fine i) $ PInt i
instance OpSafeFrom PInt Integer where
instance OpFrom PInt Integer where
    from (PInt i) = i
instance Explain Integer PInt where


_into f (PInt a) = f a
_lift f = PInt . _into f
_into2 op (PInt a) (PInt b) = (op a b)
_lift2 op a b = PInt $ _into2 op a b
_lift2__list op a b = map PInt $ _into2 op a b

instance OpSafeAdd PInt
instance OpLimitedAdd PInt
instance OpAdd PInt where
    add = _lift2 (P.+)
    -- PInt a `add` PInt b = PInt (a P.+ b)


instance OpSafeSub PInt where
    PInt a `safe_sub` PInt b = safe_from $ a P.- b
    unsafe_sub a = unjust . safe_sub a
instance OpLimitedSub PInt where
    PInt a `limited_sub` PInt b = limited_from $ a P.- b




instance OpSafeMul PInt
instance OpLimitedMul PInt
instance OpMul PInt where
    mul = _lift2 (P.*)



-- avoid 0 result
instance OpSafeFloorDivEx PInt PInt PInt where
    safe_floor_div_ex n d = safe_from $ _into2 P.div n d
instance OpSafeModEx PInt PInt PInt where
    safe_mod_ex n d = safe_from $ _into2 P.mod n d
instance OpSafeDivModEx PInt PInt PInt PInt where
    safe_divmod_ex n d = map2 safe_from $ _into2 P.divMod n d



instance OpSafeFloorDivEx Integer PInt PInt where
instance OpSafeModEx Integer PInt PInt where
instance OpFloorDivEx Integer PInt PInt where
    floor_div_ex n d = _into2 P.div n d
instance OpModEx Integer PInt PInt where
    mod_ex n d = _into2 P.mod n d
instance OpSafeDivModEx Integer Integer PInt PInt where
    safe_divmod_ex n d = map2 Just $ divmod_ex n d
    unsafe_divmod_ex = divmod_ex
instance OpDivModEx Integer Integer PInt PInt where
    divmod_ex n d = _into2 P.divMod n d


-- for DynIntegralP PInt
instance OpSafeAbs PInt where
instance OpLimitedAbs PInt where
instance OpAbs PInt where
    abs = P.abs
instance OpSafeGcd PInt where
instance OpGcd PInt where
    gcd = P.gcd
instance OpSafeDivEx PInt PInt PInt where
    safe_div_ex n d = _into2 safe_div_ex n d >>= return . PInt
instance OpSafeDivEx Rational PInt PInt where
instance OpDivEx Rational PInt PInt where
    div_ex (PInt n) (PInt d) = lift2 P.toRational (P./) n d

instance OpSafePowEx PInt PInt PInt where
instance OpPowEx PInt PInt PInt where
    pow_ex b n = P.fromInteger (P.toInteger b P.^ P.toInteger n)

--------------------  Prelude Num / Real / Enum / Integral
instance Num PInt where
    (+) = (N.+)
    (*) = (N.*)
    (-) = (N.unsafe_sub)
    abs = id
    negate u = error "negate u :: pint while u > 0"
    signum u@(PInt i) = 1
    fromInteger = E.unsafe_from


instance Real PInt where
    toRational (PInt i) = toRational i

instance Enum PInt where
    succ = _lift succ
    pred (PInt p) = unsafe_from $ pred p
        -- error "pred 1 :: pint"
    toEnum = E.unsafe_from . P.toInteger
    fromEnum = P.fromInteger . E.explain
    enumFrom = map PInt . enumFrom . E.explain
    enumFromThen a b = case compare a b of
        EQ -> repeat a
        GT -> enumFromThenTo a b (PInt _min)
        LT -> _lift2__list enumFromThen a b
    enumFromTo a b = _lift2__list enumFromTo a b
    enumFromThenTo (PInt a) = _lift2__list (enumFromThenTo a)

instance Integral PInt where
    toInteger = E.explain
    quotRem n d = map2 unsafe_from $ _into2 quotRem n d
    divMod n d = map2 unsafe_from $ _into2 divMod n d


b1 = (take 3 [1, 4..] :: [PInt]) == [1, 4, 7]
b2 = (take 3 [4, 1..] :: [PInt]) == [4, 1]

b = all id [b1, b2]
main = do
    print "PInt"
    print b




