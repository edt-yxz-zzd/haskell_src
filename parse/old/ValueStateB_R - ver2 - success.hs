



{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , DefaultSignatures
            , Rank2Types
            , TypeOperators
            , KindSignatures
            , GeneralizedNewtypeDeriving
            , ScopedTypeVariables #-}

module ValueStateB_R where

import Boxed


-- V - Value
class VStateB (p :: * -> *) v
    | p -> v -- (p v) vs (m s) in class MonadState s m | m->s
class VStateB p v => VStateR p v where
    vget :: p v -> v

class VCaseB bs where
    vcaseb :: bs q v -> bs p u
class (VStateB (bs name) v, VCaseB bs)
    => VPropertyB bs name v where -- auto
    vcase :: (VPropertyB bs name' u) => bs name v -> bs name' u
class (VStateR (bs name) v, VPropertyB bs name v)
    => VPropertyR bs name v -- auto

instance (VStateB (bs name) v, VCaseB bs)
    => VPropertyB bs name v where -- auto
    vcase = vcaseb
instance (VStateR (bs name) v, VPropertyB bs name v)
    => VPropertyR bs name v -- auto

data SelfF
data Wrapped
-- data Child parent child
data parent :.: child
type Child = (:.:)

--newtype SelfP a = SelfP { unSelfP :: a } deriving (Show)
type SelfP = NamedNew SelfF
newtype VPropertyBox s attrs v = VPropertyBox { unVPropertyBox :: s }
    deriving (Show, Eq, Ord, Read)
instance Boxed s (VPropertyBox s attrs v) where
    box = VPropertyBox
    unbox = unVPropertyBox


instance VCaseB (VPropertyBox s) where
    vcaseb = box . unbox

--instance VStateB SelfP s
instance VStateB (VPropertyBox s SelfF) s
instance VStateB p v => VStateB (VPropertyBox (p v) Wrapped) v where
instance VStateR (VPropertyBox s SelfF) s where
    vget = unbox
instance VStateR p v => VStateR (VPropertyBox (p v) Wrapped) v where
    vget = vget . unbox


instance (VStateB (VPropertyBox s attr1) v, 
          VStateB (VPropertyBox v attr2) u)
    => VStateB (VPropertyBox s (Child attr1 attr2)) u where

instance (VStateR (VPropertyBox s attr1) v, 
          VStateR (VPropertyBox v attr2) u,
          -- why cannot deduce follow line??
          VStateB (VPropertyBox s (Child attr1 attr2)) u)
    => VStateR (VPropertyBox s (Child attr1 attr2)) u where
    vget = (vget :: VPropertyBox v attr2 u -> u) . box . 
        (vget :: VPropertyBox s attr1 v -> v) . vcase



type ToAttr a = forall s p u v. () => 
    VPropertyBox s p u -> VPropertyBox s a v
type AsAttr a = forall s p u v. () => 
    VPropertyBox s p u -> VPropertyBox s (Child p a) v

to_parent :: VPropertyBox s (Child p a) v -> VPropertyBox s p u
to_parent = vcaseb
to_self :: s -> VPropertyBox s SelfF s
to_self = box

