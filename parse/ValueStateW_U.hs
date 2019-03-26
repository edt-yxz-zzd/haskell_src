

{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , DefaultSignatures
            , Rank2Types
            , TypeFamilies
            , KindSignatures
            , TypeOperators
            , GeneralizedNewtypeDeriving
            , ScopedTypeVariables #-}


module ValueStateW_U where

import Boxed
import ValueStateB_R
import Data.Functor.Identity (runIdentity, Identity(..))


class VStateB s v => VStateW s v where
    vsetb :: v -> s -> s
    default vsetb :: (VStatePW p v, p v ~ s) => v -> p v -> p v
    vsetb = vset
class (VStatePB p v, VStateW (p v) v) => VStatePW p v | p -> v where
    vset :: v -> p v -> p v
class (VStatePW (bs name) v, VPropertyB bs name v)
    => VPropertyW bs name v -- auto

class VStateW s v => VStateU s v where
    vupdateb :: (v -> v) -> (s -> s)
    -- or from VStateR ??
    default vupdateb :: (VStatePU p v, p v ~ s) => (v -> v) -> (s -> s)
    vupdateb = vupdate
class (VStateU (p v) v, VStatePW p v) => VStatePU p v where
    vupdate :: (v -> v) -> (p v -> p v)
    default vupdate :: VStatePR p v => (v -> v) -> (p v -> p v)
    vupdate f pv = vset (f $ vget pv) pv
{-
    -- RW even f is Maybe!:
    --    vfupdate :: Functor f => (v->f v) -> p v -> f (p v)
    default vfupdate :: (Functor f, VStateR p v)
        => (v->f v) -> p v -> f (p v)
    vfupdate f pv = fmap (\v' -> vset v' pv) . f $ vget pv
    -- vupdate :: (v->v) -> s -> s
    -- vmay_update :: (v->Maybe v) -> s -> Maybe s
-}

class (VStatePU (bs name) v, VPropertyW bs name v)
    => VPropertyU bs name v -- auto


class (VStateR p v, VStateU p v) => VStateRW p v -- auto
class (VStatePR p v, VStatePU p v) => VStatePRW p v -- auto
class (VStatePRW (bs name) v, VPropertyR bs name v, VPropertyU bs name v)
    => VPropertyRW bs name v -- auto



instance VStatePW (VPropertyBox s name) v
    => VStateW (VPropertyBox s name v) v
instance VStatePU (VPropertyBox s name) v
    => VStateU (VPropertyBox s name v) v


instance (VStatePW (bs name) v, VPropertyB bs name v)
    => VPropertyW bs name v where -- auto

instance (VStatePU (bs name) v, VPropertyW bs name v)
    => VPropertyU bs name v where -- auto


instance (VStateR p v, VStateU p v)
    => VStateRW p v where -- auto
instance (VStatePR p v, VStatePU p v)
    => VStatePRW p v where -- auto
instance (VStatePRW (bs name) v, 
          VPropertyR bs name v, 
          VPropertyU bs name v)
    => VPropertyRW bs name v where -- auto


-- box (functor v)
swap_bf :: (Boxed (f a) (b(f a)), Boxed a (b a), Functor f)
            => b (f a) -> f (b a)
swap_bf = fmap box . unbox
u2fvTbu2fbv:: (Boxed u bu, Boxed v bv, Functor f)
          => (u -> f v) -> (bu -> f bv)
u2fvTbu2fbv f = fmap box . f . unbox
instance VStatePW (VPropertyBox s SelfF) s where
    vset v _ = box v
instance VStateW s v => VStatePW (VPropertyBox s Wrapped) v where
    vset v = liftB $ vsetb v
instance VStatePU (VPropertyBox s SelfF) s where
    vupdate = liftB
instance VStateU s v => VStatePU (VPropertyBox s Wrapped) v where
    vupdate f = liftB $ vupdateb f





instance (VStatePU (VPropertyBox s attr1) v,  -- NOTE: VStateU
          VStatePW (VPropertyBox v attr2) u)
    => VStatePW (VPropertyBox s (Child attr1 attr2)) u where
    vset u = vcaseb . vupdate g . to_vparent where
        g = unbox . vset u . (vcaseb :: VCaseAttr attr2) . as_vself


instance (VStatePU (VPropertyBox s attr1) v, 
          VStatePU (VPropertyBox v attr2) u)
    => VStatePU (VPropertyBox s (Child attr1 attr2)) u where
    vupdate f = vcase . vupdate g . to_vparent where
        g = unbox . vupdate f . (vcaseb :: VCaseAttr attr2) . as_vself

--}

