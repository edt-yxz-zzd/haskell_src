



{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , DefaultSignatures
            , Rank2Types
            , KindSignatures
            , GeneralizedNewtypeDeriving
            , ScopedTypeVariables #-}

module ValueStateB_R where

import Boxed


-- V - Value
class VStateB s v | s -> v
class VStateB s v => VStateR s v where
    vget :: s -> v
class VStateB (p v) v => VStatePB p v -- auto
class VStateR (p v) v => VStatePR p v where -- auto
    pget :: p v -> v

class VCaseB bs where
    vcaseb :: bs q v -> bs p u
class (VStatePB (bs name) v, VCaseB bs)
    => VPropertyB bs name v where -- auto
    vcase :: (VPropertyB bs name' u) => bs name v -> bs name' u
class (VStatePR (bs name) v, VPropertyB bs name v)
    => VPropertyR bs name v -- auto

instance VStateB (p v) v => VStatePB p v -- auto
instance VStateR (p v) v => VStatePR p v where -- auto
    pget = vget
instance (VStatePB (bs name) v, VCaseB bs)
    => VPropertyB bs name v where -- auto
    vcase = vcaseb
instance (VStatePR (bs name) v, VPropertyB bs name v)
    => VPropertyR bs name v -- auto

data SelfF
data Wrapped
data Child child parent
newtype VPropertyBox s attrs v = VPropertyBox { unVPropertyBox :: s }
    deriving (Show, Eq, Ord, Read)
instance Boxed s (VPropertyBox s attrs v) where
    box = VPropertyBox
    unbox = unVPropertyBox

instance VCaseB (VPropertyBox s) where
    vcaseb = box . unbox

instance VStateB (VPropertyBox s SelfF s) s
instance VStateB s v => VStateB (VPropertyBox s Wrapped v) v where
instance VStateR (VPropertyBox s SelfF s) s where
    vget = unbox
instance VStateR s v => VStateR (VPropertyBox s Wrapped v) v where
    vget = vget . unbox


instance (VStatePB (VPropertyBox s attr1) v, 
          VStatePB (VPropertyBox v attr2) u)
    => VStateB (VPropertyBox s (Child attr2 attr1) u) u where

instance (VStatePR (VPropertyBox s attr1) v, 
          VStatePR (VPropertyBox v attr2) u,
          -- why cannot deduce follow line??
          VStateB (VPropertyBox s (Child attr2 attr1) u) u)
    => VStateR (VPropertyBox s (Child attr2 attr1) u) u where
    vget = (vget :: VPropertyBox v attr2 u -> u) . box . 
        (vget :: VPropertyBox s attr1 v -> v) . vcase



