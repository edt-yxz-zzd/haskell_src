{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Bijection.IntBijection.IntBijection
where

import Seed.Boxed
import Seed.UnsafeUnpack (unsafe_right)
import Bijection.Bijection
import Numeric.Natural
import Data.List
import IntTransform.Integer2Bytes (uint2digits__be, digits2uint__be)
import IntDefs.IntDefs (SInt, UInt, QInt, IntGe
    -- , qualified UIntGeEx as UIntGe)
    , UIntGeEx
    )
import Seed.ListOps
    ( empty_or_tail, split_if, split1_if, split_eq, join1, join
    )



type UInts = [UInt]
type UInt1s = (UInt, UInts)
type UInt2s = (UInt, UInt1s)

data Neg__SInt2SInt
instance Bijection Neg__SInt2SInt where
    type BijectionFrom Neg__SInt2SInt = SInt
    type BijectionTo Neg__SInt2SInt = SInt
    forward = const negate
    backward = forward

data AddConst__SInt2SInt = AddConst__SInt2SInt SInt
instance Bijection AddConst__SInt2SInt where
    type BijectionFrom AddConst__SInt2SInt = SInt
    type BijectionTo AddConst__SInt2SInt = SInt
    forward (AddConst__SInt2SInt i) = (+i)
    backward (AddConst__SInt2SInt i) a = a - i


neg2odd :: SInt -> UInt
neg2odd i   | i < 0 = fromInteger $ (-i) * 2 - 1
            | otherwise = fromInteger $ 2*i
odd2neg :: UInt -> SInt
odd2neg u   | odd u = negate . toInteger $ (u+1) `div` 2
            | otherwise = toInteger $ div u 2

data Odd2Neg__UInt2SInt
instance Bijection Odd2Neg__UInt2SInt where
    type BijectionFrom Odd2Neg__UInt2SInt = UInt
    type BijectionTo Odd2Neg__UInt2SInt = SInt
    forward = const odd2neg
    backward = const neg2odd




-- data UInt2OffsetedModUInts



---------------------------- QInt/IntGe2
qint2uint :: QInt -> UInt
qint2uint = fromIntegral
unsafe_makeQInt :: Integral i => i -> QInt
unsafe_makeQInt = fromIntegral


---------------------------- UIntGeEx

uintge2uint :: UIntGeEx u => u -> UInt
uintge2uint = fromIntegral


uintge2int :: UIntGeEx u => u -> Integer
uintge2int = toInteger . uintge2uint
uintge_binop :: (UIntGeEx u, Integral i) => (SInt -> SInt -> SInt) -> u -> i -> u
uintge_binop op u i = unsafe_makeIntGe $ uintge2int u `op` toInteger i
uintge_add, uintge_sub, uintge_mul, uintge_div, uintge_mod
    :: (UIntGeEx u, Integral i) => u -> i -> u
uintge_add = uintge_binop (+)
uintge_sub = uintge_binop (-)
uintge_mul = uintge_binop (*)
uintge_div = uintge_binop div
uintge_mod = uintge_binop mod













class Monad m => MOp_GetQIntModulo m where
    get_qint_modulo :: m QInt

data GetQIntModuloT m a = GetQIntModuloT { runGetQIntModuloT :: QInt -> m a}
    -- deriving (Monad)

instance Monad m => Functor (GetQIntModuloT m) where
    fmap f (GetQIntModuloT p2ma) = GetQIntModuloT $ \qint->
        fmap f (p2ma qint)
instance Monad m => Applicative (GetQIntModuloT m) where
    pure a = GetQIntModuloT $ \qint -> pure a
    GetQIntModuloT p2mf <*> GetQIntModuloT p2ma = GetQIntModuloT $ \qint->
        p2mf qint <*> p2ma qint
instance Monad m => Monad (GetQIntModuloT m) where
    fail err = GetQIntModuloT $ \qint -> fail err
    GetQIntModuloT f >>= a2G = GetQIntModuloT $ \qint->
        f qint >>= \a -> runGetQIntModuloT (a2G a) qint
instance Monad m => MOp_GetQIntModulo (GetQIntModuloT m) where
    get_qint_modulo = GetQIntModuloT $ \qint -> return qint

i2i :: (Integral i, Integral j) => i -> j
i2i = fromInteger . toInteger

get_uint_modulo :: MOp_GetQIntModulo m => m UInt
get_uint_modulo = fmap qint2uint get_qint_modulo
addM, subM, mulM :: MOp_GetQIntModulo m => UInt -> UInt -> m UInt
addM = binopM (+)
subM = binopM (-)
mulM = binopM (*)
binopM
    :: MOp_GetQIntModulo m
    => (SInt -> SInt -> SInt) -> UInt -> UInt -> m UInt
binopM op a b = do
    m <- get_uint_modulo
    if a >= m || b >= m then fail "parameter >= modulo" else
        return . fromInteger . mod (op a' b') $ toInteger m
    where
        a' = toInteger a
        b' = toInteger b



data OffsetedModUInts = OMUs__private QInt UInt [UInt]
    -- modulo length digits
    -- length digits < inf
    -- all (< modulo) digits
    -- ??big-endian??
makeOffsetedModUInts
    :: Monad m => QInt -> [UInt] -> m OffsetedModUInts
makeOffsetedModUInts modulo digits
    | all (< qint2uint modulo) digits = return omu
    | otherwise                       = fail "not $ all (< modulo) digits"
    where
        omu = OMUs__private modulo (genericLength digits) digits
makeOffsetedModUIntsM
    :: MOp_GetQIntModulo m => [UInt] -> m OffsetedModUInts
makeOffsetedModUIntsM digits = do
    m <- get_qint_modulo
    makeOffsetedModUInts m digits






data UnpackOffsetedModUInts =
    UOMs {modulo :: QInt, len :: UInt, digits :: [UInt]}
unpack_offsetedModUInts :: OffsetedModUInts -> UnpackOffsetedModUInts
unpack_offsetedModUInts (OMUs__private modulo len digits) =
    UOMs {modulo = modulo, len = len, digits = digits}

unjust :: Maybe a -> a
unjust (Just a) = a
unjust _ = error "unjust Nothing"
unsafe_makeOffsetedModUInts m = unsafe_right . makeOffsetedModUInts m



-------------------------------------


data UInt2OffsetedModUInts__be = U2OMUs__be QInt


instance Bijection UInt2OffsetedModUInts__be where
    type BijectionFrom UInt2OffsetedModUInts__be = UInt
    type BijectionTo UInt2OffsetedModUInts__be = OffsetedModUInts
    forward (U2OMUs__be q) u = unsafe_makeOffsetedModUInts q digits where
        digits = uint2digits__be (qint2uint q) u
    backward (U2OMUs__be q') omu = case unpack_offsetedModUInts omu of
        UOMs {modulo = q, digits=digits} ->
            if q /= q'
            then error "UInt2OffsetedModUInts__be: modulo not matched"
            else digits2uint__be (qint2uint q) digits


digits2digitss__dec_base :: OffsetedModUInts -> (UInt, [[UInt]])
digits2digitss__dec_base omu = case unpack_offsetedModUInts omu of
    UOMs {modulo = m, digits = digits} ->
        let upper = qint2uint m - 1
        in  (upper, split_eq upper digits)
unsafe_digits2digitss__baseGe3
    :: OffsetedModUInts -> (QInt, [OffsetedModUInts])
unsafe_digits2digitss__baseGe3 omu = (base, r) where
    (base_, digitss) = digits2digitss__dec_base omu
    base = unsafe_makeQInt base_
    r = map (unsafe_makeOffsetedModUInts base) digitss



-- select the output modulo
class (Bijection a, BijectionFrom a ~ UInt, BijectionTo a ~ OffsetedModUInts)
    => IUInt2OffsetedModUInts a
instance (Bijection a, BijectionFrom a ~ UInt, BijectionTo a ~ OffsetedModUInts)
    => IUInt2OffsetedModUInts a

class (Bijection a, BijectionFrom a ~ UInt, BijectionTo a ~ UInts)
    => IUInt2UInts a
instance (Bijection a, BijectionFrom a ~ UInt, BijectionTo a ~ UInts)
    => IUInt2UInts a
class (Bijection a, BijectionFrom a ~ UInt, BijectionTo a ~ SInt)
    => IUInt2SInt a
instance (Bijection a, BijectionFrom a ~ UInt, BijectionTo a ~ SInt)
    => IUInt2SInt a




data UInt2EitherUInt1sUint2s__viaOffsetedModUInts where
    -- split (max digits)
    UInt2EitherUInt1sUint2s__viaOffsetedModUInts__modulo_q_or_lesser
        :: (IUInt2OffsetedModUInts a, IUInt2OffsetedModUInts b)
        => QInt -> (QInt -> a) -> (QInt -> b)
        -> UInt2EitherUInt1sUint2s__viaOffsetedModUInts
{-
instance Bijection UInt2EitherUInt1sUint2s__viaOffsetedModUInts where
    type BijectionFrom UInt2EitherUInt1sUint2s__viaOffsetedModUInts = UInt
    type BijectionTo UInt2EitherUInt1sUint2s__viaOffsetedModUInts
        = Either UInt1s UInt2s
    forward (UInt2EitherUInt1sUint2s__viaOffsetedModUInts__modulo_q_or_lesser
            q f_m_q f_m_lesser) u = uint1s_or_uint2s
      where
        m_q = f_m_q q
        _omu = forward m_q u
        q' = omu2modulo _omu
        omu = if q' /= q then error $ "ValueError: " ++
            "UInt2UInt1s__viaOffsetedModUInts__modulo_q_or_lesser q f_m_q _ " ++
            "where (f_m_q q) not map uint to [uint mod q]"
            else _omu
        digits = omu2digits omu
        _max = foldl' max 0 digits
        digitss = split_eq _max digits
        uint1s_or_uint2s
            -- len == 1
            | _max == 0 = Left . ls2ls1 $ [genericLength digits]
            -- len >= 2
            | _max == 1 = Left . ls2ls1 $ map genericLength digitss
            -- len >= 2
            | otherwise = Right ls2ls2 $ map
                    (backward . f_m_lesser $ unsafe_makeQInt _max) digitss
        ls2ls1 (h:ts) = (h,ts)
        ls2ls2 (h:ts) = (h, ls2ls1 ts)
    backward (UInt2EitherUInt1sUint2s__viaOffsetedModUInts__modulo_q_or_lesser
            q f_m_q f_m_lesser) uint1s_or_uint2s = u
      where
        m_q = f_m_q q
        u = case uint1s_or_uint2s of
                Left uint1s -> f1s uint1s
                Right uint2s -> f2s uint2s
        f1s (h, ts) = backward m_q omu where
            zeross = map (flip genericReplicate 0) $ h:ts
            omu = unsafe_makeOffsetedModUInts 2 $ join1 1 zeross
        f2s (h, (h2, ts)) = where
            
        _omu = forward m_q u
        q' = omu2modulo _omu
        omu = if q' /= q then error $ "ValueError: " ++
            "UInt2UInt1s__viaOffsetedModUInts__modulo_q_or_lesser q f_m_q _ " ++
            "where (f_m_q q) not map uint to [uint mod q]"
            else _omu
-}



data UInt2UInts__viaOffsetedModUInts where
    -- split (base-1)
    UInt2UInts__viaOffsetedModUInts__modulo_q_or_q1
        :: (IUInt2OffsetedModUInts a, IUInt2OffsetedModUInts b)
        => QInt -> (QInt -> a) -> (QInt -> b) -> UInt2UInts__viaOffsetedModUInts



instance Bijection UInt2UInts__viaOffsetedModUInts where
    type BijectionFrom UInt2UInts__viaOffsetedModUInts = UInt
    type BijectionTo UInt2UInts__viaOffsetedModUInts = UInts
    forward (UInt2UInts__viaOffsetedModUInts__modulo_q_or_q1 q f_m_q f_m_q1) u
        = uints where
        q1 = uintge_add q 1
        m_q = f_m_q q
        m_q1 = f_m_q1 q1
        _omu = forward m_q1 u
        q1' = omu2modulo _omu
        omu = if q1' /= q1 then error $ "ValueError: " ++
            "UInt2UInts__viaOffsetedModUInts__modulo_q_or_q1 q _ f_m_q1 " ++
            "where (f_m_q1 (q+1)) not map uint to [uint mod q+1]"
            else _omu
        --(base__ge2, digitss) = digits2digitss__dec_base omu
        -- base__ge2 == q
        -- q1 >= 3
        (base__ge2, digitss) = unsafe_digits2digitss__baseGe3 omu
        uints = map (backward m_q) digitss

    backward (UInt2UInts__viaOffsetedModUInts__modulo_q_or_q1 q f_m_q f_m_q1)
        uints = u where
        q1 = uintge_add q 1
        m_q = f_m_q q
        m_q1 = f_m_q1 q1
        _digitss = map (forward m_q) uints
        pred omu_q = q == omu2modulo omu_q
        digitss = if not $ all pred _digitss then error $ "ValueError: " ++
                "UInt2UInts__viaOffsetedModUInts__modulo_q_or_q1 q f_m_q _ " ++
                "where (f_m_q q) not map uint to [uint mod q]"
                else map omu2digits _digitss

        digits = join1 (qint2uint q) digitss
        omu = unsafe_makeOffsetedModUInts q1 digits
        u = backward m_q1 omu

omu2modulo omu = case unpack_offsetedModUInts omu of
    UOMs {modulo=m} -> m
omu2digits omu = case unpack_offsetedModUInts omu of
    UOMs {digits=digits} -> digits



{-
uint2uints__viaOffsetedModUInts
uint2uints__viaOffsetedModUInts qint u = uints where
    omu = forward (UInt2OffsetedModUInts $ uintge_add qint 1) u
    (base, digitss) = unsafe_makeOffsetedModUInts omu
    uints = map (backward $ UInt2OffsetedModUInts qint) digitss
uints2uint__viaOffsetedModUInts qint uints = u where
    digitss = map (forward $ UInt2OffsetedModUInts qint) uints
    upper = qint2uint qint
    digits = join1 upper digitss

offsetedModUints2digits = 
--}
--}
--}
--}

