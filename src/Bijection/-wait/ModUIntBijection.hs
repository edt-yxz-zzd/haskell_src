{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}


module Bijection.ModUIntBijection
where



import Bijection.TypeDefs
import Bijection.Bijection
import IntDefs.IntDefs
import ListDefs.ListDefs
    ( unsafe_list2list1x, List1x, list1x2list
    , unsafe_list2list2x, List2x, list2x2list
    )
import Seed.ListOps (split_eq, join1, genericLength, genericReplicate)
import TH.SeedUtils (def__instances)

data UIntsModQInt2UIntss1xModPInt__M_minus_1
type M_minus_1 = UIntsModQInt2UIntss1xModPInt__M_minus_1
instance Bijection M_minus_1 where
    type BijectionFrom M_minus_1 = UIntsMod QInt
    type BijectionTo M_minus_1 = UIntss1xMod PInt
    forward _ (UIntsMod q us) = UIntss1xMod p uss1 where
        p = q2p q - 1
        m = p2u p
        uss = split_eq m us
        uss1 = unsafe_list2list1x uss
    backward _ (UIntss1xMod p uss1) = UIntsMod q us where
        q = i2i (p+1)
        m = p2u p
        uss = list1x2list uss1
        us = join1 m uss

def__instances [d|
    class (Bijection a, BijectionFrom a ~ UIntsMod u, BijectionTo a ~ to)
        => BijectionFromUIntsMod u to a | a -> u to where
    |]

data SplitMax = MaxLe1 (List1x UInt) | MaxGe2 (UIntss2xMod QInt)
data UIntsModInt3x2Int3xSplitMax

instance Bijection UIntsModInt3x2Int3xSplitMax where
    type BijectionFrom UIntsModInt3x2Int3xSplitMax = UIntsMod Int3x
    type BijectionTo UIntsModInt3x2Int3xSplitMax = (Int3x, SplitMax)
    forward _ (UIntsMod ge3 us) = (ge3, r) where
        max_ = foldr max 1 us
        uss = split_eq max_ us
        uss1x = unsafe_list2list1x uss
        uss2x = unsafe_list2list2x uss
        r | max_ == 1 = MaxLe1 $ fmap genericLength uss1x
          | otherwise = MaxGe2 $ UIntss2xMod (i2i max_) uss2x

    backward _ (ge3, MaxLe1 us1x) = r where
        us = list1x2list us1x
        us' = join1 1 $ map (flip genericReplicate 0) us
        r = UIntsMod ge3 us'
    backward _ (ge3, MaxGe2 (UIntss2xMod q uss2x)) = r where
        uss = list2x2list uss2x
        us = join1 (q2u q) uss
        r = UIntsMod ge3 us



