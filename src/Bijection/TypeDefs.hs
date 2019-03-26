{-# LANGUAGE TemplateHaskell #-}

module Bijection.TypeDefs
where



import Bijection.Bijection
import IntDefs.IntDefs
import ListDefs.ListDefs
    ( unsafe_list2list1x, List1x, list1x2list
    , unsafe_list2list2x, List2x, list2x2list
    )
import Seed.ListOps (split_eq, join1, genericLength, genericReplicate)


type UInts = [UInt]
--type UInts1 = List1x UInt
type UIntss1x = List1x UInts
type UIntss2x = List2x UInts

-- M [u] where u < M
data UIntMod m = UIntMod m UInt
data UIntsMod m = UIntsMod m UInts
data UIntss1xMod m = UIntss1xMod m UIntss1x
data UIntss2xMod m = UIntss2xMod m UIntss2x
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


