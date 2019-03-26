{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UInt
    ( UInt -- (..)
    )
where
import Explain as E
import NumOp as N
import SeedUtils (unjust, justif, jcheck, Test(..), type_as, map2)
import Prelude as P
import ReadHelper__UInt (readsPrec_viaUInt)
import PInt
instance Explain UInt PInt where
instance OpSafeFrom PInt UInt where
instance OpFrom PInt UInt where
    from p = UInt (E.explain p)





newtype UInt = UInt Integer -- nonnegative
    deriving (Eq, Ord, Show)


instance Read UInt where
    readsPrec d s = readsPrec_viaUInt unsafe_from d s

{-
newtype ReadUInt = ReadUInt Integer
    deriving (Read)
instance Read UInt where
    readsPrec d s = do
        (ReadUInt i, tail) <- readParen (d > app_ctor_prec)
            (\s -> readsPrec d ("Read"++s)) s
        if i < _min
        -- then error "read \"UInt i\" where i < 0"
        then []
        else return (UInt i, tail)
      where app_ctor_prec = 10
            -- infix_ctor_prec = undefined -- not a infix
-}

instance Test UInt where
    test (UInt i) = i /= 0
_min :: Integer
_min = 0
_fine :: Integer -> Bool
_fine = (>= _min)

instance LimitedFrom Integer UInt where
    limited_from i = UInt $ max _min i
instance OpSafeFrom Integer UInt where
    safe_from i = justif (_fine i) $ UInt i
instance OpSafeFrom UInt Integer where
instance OpFrom UInt Integer where
    from (UInt i) = i
instance Explain Integer UInt where


_into f (UInt a) = f a
_lift f = UInt . _into f
_into2 op (UInt a) (UInt b) = (op a b)
_lift2 op a b = UInt $ _into2 op a b
_lift2__list op a b = map UInt $ _into2 op a b

instance OpSafeAdd UInt
instance OpLimitedAdd UInt
instance OpAdd UInt where
    add = _lift2 (P.+)
    -- UInt a `add` UInt b = UInt (a P.+ b)


instance OpSafeSub UInt where
    UInt a `safe_sub` UInt b = safe_from $ a P.- b
    unsafe_sub a = unjust . safe_sub a
instance OpLimitedSub UInt where
    UInt a `limited_sub` UInt b = limited_from $ a P.- b




instance OpSafeMul UInt
instance OpLimitedMul UInt
instance OpMul UInt where
    mul = _lift2 (P.*)



instance OpSafeFloorDivEx UInt UInt UInt where
    safe_floor_div_ex n d = justif (test d) $ _lift2 P.div n d
instance OpSafeModEx UInt UInt UInt where
    safe_mod_ex n d = justif (test d) $ _lift2 P.mod n d
instance OpSafeDivModEx UInt UInt UInt UInt where
    safe_divmod_ex n d =
        if test d then map2 (Just . UInt) $ _into2 P.divMod n d
        else (Nothing, Nothing)


instance OpSafeFloorDivEx Integer UInt UInt where
    safe_floor_div_ex n d = justif (test d) $ _into2 P.div n d
instance OpSafeModEx Integer UInt UInt where
    safe_mod_ex n d = justif (test d) $ _into2 P.mod n d
instance OpSafeDivModEx Integer Integer UInt UInt where
    safe_divmod_ex n d =
        if test d then map2 Just $ _into2 P.divMod n d
        else (Nothing, Nothing)



-- for DynIntegral Integer
instance OpSafePowEx Integer Integer UInt where
instance OpPowEx Integer Integer UInt where
    pow_ex b (UInt n) = b ^ n
-- for DynIntegralU UInt
instance OpSafePowEx UInt UInt UInt where
instance OpPowEx UInt UInt UInt where
    pow_ex = _lift2 (P.^)
instance OpSafeAbs UInt where
instance OpLimitedAbs UInt where
instance OpAbs UInt where
    abs = P.abs
instance OpSafeGcd UInt where
instance OpGcd UInt where
    gcd = P.gcd
instance OpSafeDivEx UInt UInt UInt where
    safe_div_ex (UInt n) (UInt d) = safe_div_ex n d >>= return . UInt
instance OpSafeModEx UInt UInt PInt where
instance OpModEx UInt UInt PInt where
    mod_ex (UInt n) d = UInt $ P.mod n $ toInteger d
instance OpSafeFloorDivEx UInt UInt PInt where
instance OpFloorDivEx UInt UInt PInt where
    floor_div_ex (UInt n) d = UInt $ P.div n $ toInteger d
instance OpSafeDivModEx UInt UInt UInt PInt where
instance OpDivModEx UInt UInt UInt PInt where
    divmod_ex (UInt n) d = map2 UInt $ P.divMod n $ toInteger d


--------------------  Prelude Num / Real / Enum / Integral
instance Num UInt where
    (+) = (N.+)
    (*) = (N.*)
    (-) = (N.unsafe_sub)
    abs = id
    negate u = if test u then error "negate u :: uint while u > 0" else u
    signum u@(UInt i) = if i < 2 then u else (UInt 1)
    fromInteger = E.unsafe_from


instance Real UInt where
    toRational (UInt i) = toRational i

instance Enum UInt where
    succ = _lift succ
    pred u = if test u then _lift pred u else
        error "pred 0 :: uint"
    toEnum = E.unsafe_from . P.toInteger
    fromEnum = P.fromInteger . E.explain
    enumFrom = map UInt . enumFrom . E.explain
    enumFromThen a b = case compare a b of
        EQ -> repeat a
        GT -> enumFromThenTo a b (UInt _min)
        LT -> _lift2__list enumFromThen a b
    enumFromTo a b = _lift2__list enumFromTo a b
    enumFromThenTo (UInt a) = _lift2__list (enumFromThenTo a)

instance Integral UInt where
    toInteger = E.explain
    -- quotRem n d = let (q, r) = _into2 quotRem n d in (UInt q, UInt r)
    quotRem n d = map2 UInt $ _into2 quotRem n d
    divMod n d = map2 UInt $ _into2 divMod n d
        -- otherwise : UInt *** Exception: negate u :: uint while u > 0
-- divMod n d =  if signum r == negate (signum d) then (q-1, r+d) else qr
--                            where qr@(q,r) = quotRem n d







b1 = (take 3 [1, 4..] :: [UInt]) == [1, 4, 7]
b2 = (take 3 [4, 1..] :: [UInt]) == [4, 1]

b = all id [b1, b2]
main = do
    print "UInt"
    print b



