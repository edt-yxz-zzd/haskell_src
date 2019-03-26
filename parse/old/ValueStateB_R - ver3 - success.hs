



{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , DefaultSignatures
            , TypeFamilies
            , Rank2Types
            , TypeOperators
            , KindSignatures
            , GeneralizedNewtypeDeriving
            , ScopedTypeVariables #-}

module ValueStateB_R where

import Boxed
import SeedUtils

class VStateB s v | s -> v
class VStateB s v => VStateR s v where
    vgetb :: s -> v
    default vgetb :: (VStatePR p v, p v ~ s) => s -> v
    vgetb = vget
class VStateB (p v) v => VStatePB p v | p -> v -- NO auto; but each bs
class (VStateR (p v) v, VStatePB p v) => VStatePR p v where -- NO; auto
    -- since vgetb is ambiguous
    -- vgetb :: p v -> u
    vget :: p v -> v
    -- default vget :: p v -> v
    -- vget = vgetb

-- error: instance VStateB (p v) v => VStatePB p v
-- error: instance (VStateR (p v) v, VStatePB p v) => VStatePR p v
instance VStatePB (VPropertyBox s attrs) v
    => VStateB (VPropertyBox s attrs v) v
instance VStatePR (VPropertyBox s attrs) v
    => VStateR (VPropertyBox s attrs v) v





-- V - Value


class VCaseB bs where
    vcaseb :: bs q v -> bs p u
class (VStatePB (bs name) v, VCaseB bs)
    => VPropertyB bs name v | bs name -> v where -- NO auto
    vcase :: (VPropertyB bs name' u) => bs name v -> bs name' u
class (VStatePR (bs name) v, VPropertyB bs name v)
    => VPropertyR bs name v -- auto

instance (VStatePB (VPropertyBox s name) v, VCaseB (VPropertyBox s))
    => VPropertyB (VPropertyBox s) name v where -- auto
    vcase = vcaseb
instance (VStatePR (bs name) v, VPropertyB bs name v)
    => VPropertyR bs name v -- auto

data SelfF
data Wrapped
-- data Child parent child
data parent :.: child
type Child = (:.:)

--newtype SelfP a = SelfP { unSelfP :: a } deriving (Show)
-- type SelfP = NamedNew SelfF
newtype VPropertyBox s attrs v = VPropertyBox { unVPropertyBox :: s }
    deriving (Show, Eq, Ord, Read)
instance Boxed s (VPropertyBox s attrs v) where
    box = VPropertyBox
    unbox = unVPropertyBox


instance VCaseB (VPropertyBox s) where
    vcaseb = box . unbox



instance VStatePB (VPropertyBox s SelfF) s
instance VStatePR (VPropertyBox s SelfF) s where
    vget = unbox
instance VStateB s v => VStatePB (VPropertyBox s Wrapped) v where
instance VStateR s v => VStatePR (VPropertyBox s Wrapped) v where
    vget = vgetb . unbox


instance (VStatePB (VPropertyBox s attr1) v -- PB instead of B
         ,VStatePB (VPropertyBox v attr2) u
         )
    => VStatePB (VPropertyBox s (Child attr1 attr2)) u where

instance (VStatePR (VPropertyBox s attr1) v
         ,VStatePR (VPropertyBox v attr2) u
          -- why cannot deduce follow line??
         --,VStateB (VPropertyBox s (Child attr1 attr2) u) u
         )
    => VStatePR (VPropertyBox s (Child attr1 attr2)) u where
    vget = (vget :: VPropertyBox v attr2 u -> u) . box . 
               (vget :: VPropertyBox s attr1 v -> v) . vcase
    -- vgetb = vgetb . box . vgetb . vcase



type VCaseAttr q =
    forall s p u v.
                (VPropertyB (VPropertyBox s) p u
                ,VPropertyB (VPropertyBox s) q v)
                => VPropertyBox s p u -> VPropertyBox s q v
type VGetAttr c =
    forall s p u v. -- SHOULD NO u v, since s p c -> u, v
                (VPropertyB (VPropertyBox s) (Child p c) v
                ,VPropertyB (VPropertyBox s) p u)
                => VPropertyBox s p u -> VPropertyBox s (Child p c) v
type VIsAttr c =
    forall s p v.
                VPropertyB (VPropertyBox s) (Child p c) v
                => VPropertyBox s (Child p c) v
{-
type family ValueT (bs :: * -> * -> *) name
type instance ValueT bs name = VPropertyB bs name v => v

type family AsAttr c
type instance AsAttr c = Int {-
    forall s p. -- NO u v, since s p c -> u, v
                (VPropertyB (VPropertyBox s) (Child p c) v
                ,VPropertyB (VPropertyBox s) p u)
                => VPropertyBox s p u -> VPropertyBox s (Child p c) v
--}-}


to_parent :: (VPropertyB (VPropertyBox s) (Child p a) v,
              VPropertyB (VPropertyBox s) p u)
           => VPropertyBox s (Child p a) v -> VPropertyBox s p u
to_parent = vcase
--toXxxP :: VGetAttr XxxF
--toXxxP = vcase
vself :: s -> VPropertyBox s SelfF s
vself = box


--}

