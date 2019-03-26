{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Bijection.ModIntsBijection
where

import Bijection.Bijection
import IntDefs.IntDefs
import ListDefs.ListDefs
    ( unsafe_list2list1x, List1x, list1x2list
    , unsafe_list2list2x, List2x, list2x2list
    )
import Seed.ListOps (split_eq, join1, genericLength, genericReplicate)
data UIntsModQInt2UIntss1xModPInt__M_minus_1


type UInts = [UInt]
--type UInts1 = List1x UInt
type UIntss1x = List1x UInts
type UIntss2x = List2x UInts

-- M [u] where u < M
data UIntLsMod m ls = UIntLsMod m ls
-- [M] [u] where M[i] < M[j] and if len Ms then u < M[0]
data UIntLsModEx ms ls = UIntLsModEx ms ls
type UIntsMod m = UIntLsMod m UInts
type UIntss1xMod m = UIntLsMod m UIntss1x
type UIntss2xMod m = UIntLsMod m UIntss2x
--data UIntsMod m = UIntsMod m [UInt]
--data UIntssMod m = UIntssMod m [[UInt]]

i2i :: (Integral i, Integral j) => i -> j
i2i = fromIntegral
q2u :: QInt -> UInt
q2u = i2i
q2p :: QInt -> PInt
q2p = i2i
p2u :: PInt -> UInt
p2u = i2i

type M_minus_1 = UIntsModQInt2UIntss1xModPInt__M_minus_1
instance Bijection M_minus_1 where
    type BijectionFrom M_minus_1 = UIntsMod QInt
    type BijectionTo M_minus_1 = UIntss1xMod PInt
    forward _ (UIntLsMod q us) = UIntLsMod p uss1 where
        p = q2p q - 1
        m = p2u p
        uss = split_eq m us
        uss1 = unsafe_list2list1x uss
    backward _ (UIntLsMod p uss1) = UIntLsMod q us where
        q = i2i (p+1)
        m = p2u p
        uss = list1x2list uss1
        us = join1 m uss


data SplitMax = MaxLe1 Int3x (List1x UInt) | MaxGe2 Int3x (UIntss2xMod QInt)
data UIntsModInt3x2SplitMax

instance Bijection UIntsModInt3x2SplitMax where
    type BijectionFrom UIntsModInt3x2SplitMax = UIntsMod Int3x
    type BijectionTo UIntsModInt3x2SplitMax = SplitMax
    forward _ (UIntLsMod ge3 us) = r where
        max_ = foldr max 1 us
        uss = split_eq max_ us
        uss1x = unsafe_list2list1x uss
        uss2x = unsafe_list2list2x uss
        r | max_ == 1 = MaxLe1 ge3 $ fmap genericLength uss1x
          | otherwise = MaxGe2 ge3 $ UIntLsMod (i2i max_) uss2x

    backward _ (MaxLe1 ge3 us1x) = r where
        us = list1x2list us1x
        us' = join1 1 $ map (flip genericReplicate 0) us
        r = UIntLsMod ge3 us'
    backward _ (MaxGe2 ge3 (UIntLsMod q uss2x)) = r where
        uss = list2x2list uss2x
        us = join1 (q2u q) uss
        r = UIntLsMod ge3 us


-- (ge3::Int3x, List1x UInt, Maybe (UInt, q::QInt mod ge3))
-- (ge3::Int3x, List1x UInt, Maybe (u0::UInt, q-2::UInt mod ge3-2))
-- (ge3::Int3x, List1x UInt, Maybe (u0*(ge3-2)+ q-2 :: UInt))
-- (ge3::Int3x, List1x UInt, Maybe (u0*(ge3-2)+ q-2 +1 :: PInt))
-- (ge3::Int3x, List1x UInt, (0 | u0*(ge3-2)+ q-2 +1) :: UInt)
-- (ge3::Int3x, UInts2x)
data SplitMax2UInts1x

instance (Bijection a, BijectionFrom a ~ UInts2xModQInt, BijectionTo a ~ UIntss2xModQInt)
    => S

