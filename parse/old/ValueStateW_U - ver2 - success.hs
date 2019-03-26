

{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , DefaultSignatures
            , Rank2Types
            , KindSignatures
            , TypeOperators
            , GeneralizedNewtypeDeriving
            , ScopedTypeVariables #-}


module ValueStateW_U where

import Boxed
import ValueStateB_R
import Data.Functor.Identity (runIdentity, Identity(..))


class VStateB p v => VStateW p v where
    vset :: v -> p v -> p v
class (VStateW (bs name) v, VPropertyB bs name v) => VPropertyW bs name v -- auto

class VStateW p v => VStateU p v where
    vfupdate :: Functor f => (v->f v) -> p v -> f (p v)
    default vfupdate :: (Functor f, VStateR p v)
        => (v->f v) -> p v -> f (p v)
    vfupdate f pv = fmap (\v' -> vset v' pv) . f $ vget pv
    -- vupdate :: (v->v) -> s -> s
    -- vmay_update :: (v->Maybe v) -> s -> Maybe s
class (VStateU (bs name) v, VPropertyW bs name v) => VPropertyU bs name v -- auto
class (VStateR p v, VStateU p v) => VStateRW p v -- auto
class (VStateRW (bs name) v, VPropertyR bs name v, VPropertyU bs name v)
    => VPropertyRW bs name v -- auto



instance (VStateW (bs name) v, VPropertyB bs name v)
    => VPropertyW bs name v where -- auto

instance (VStateU (bs name) v, VPropertyW bs name v)
    => VPropertyU bs name v where -- auto
instance (VStateR p v, VStateU p v)
    => VStateRW p v where -- auto
instance (VStateRW (bs name) v, 
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
instance VStateW (VPropertyBox s SelfF) s where
    vset v _ = box v
instance VStateW p v => VStateW (VPropertyBox (p v) Wrapped) v where
    vset v = liftB $ vset v
instance VStateU (VPropertyBox s SelfF) s where
    vfupdate = u2fvTbu2fbv -- fmap box . f . unbox
vupdate :: VStateU p v => (v->v) -> (p v -> p v)
vupdate f = runIdentity . vfupdate  (Identity . f)
instance VStateU p v => VStateU (VPropertyBox (p v) Wrapped) v where
    vfupdate = u2fvTbu2fbv . vfupdate -- fmap box . vfupdate f . unbox





instance (VStateU (VPropertyBox s attr1) v,  -- NOTE: VStateU
          VStateW (VPropertyBox v attr2) u)
    => VStateW (VPropertyBox s (Child attr1 attr2)) u where
    vset u = vcaseb . vupdate g . to_parent where
        g = unbox . vset u . (vcaseb :: ToAttr attr2) . to_self


instance (VStateU (VPropertyBox s attr1) v, 
          VStateU (VPropertyBox v attr2) u)
    => VStateU (VPropertyBox s (Child attr1 attr2)) u where
    vfupdate f = fmap vcase . vfupdate g . to_parent where
        g = fmap unbox . vfupdate f . (vcaseb :: ToAttr attr2) . to_self

