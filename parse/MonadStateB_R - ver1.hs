


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

module MonadStateB_R where

import ValueStateB_R
import Boxed

-- B - Basic; R - Read;
-- M - Monad


class Monad (m v) => MStateB m v | m -> v
class MStateB m v => MStateR m v where
    mget :: (m v) v
class MCaseB bm where
    mcaseb :: bm p v a -> bm q u a
class (MStateB (bm name) v, MCaseB bm)
    => MPropertyB bm name v | bm name -> v where -- auto
    mcase :: (MPropertyB bm name' u) => bm name v a -> bm name' u a
class (MStateR (bm name) v, MPropertyB bm name v)
    => MPropertyR bm name v -- auto



instance (MStateB (bm name) v, MCaseB bm)
    => MPropertyB bm name v where -- auto
    mcase = mcaseb
instance (MStateR (bm name) v, MPropertyB bm name v)
    => MPropertyR bm name v -- auto




newtype MStateBox m attrs v a = MStateBox { unMStateBox :: (m a) }
    deriving (Monad)
instance Boxed (m a) (MStateBox m name v a) where
    box = MStateBox
    unbox = unMStateBox
instance MCaseB (MStateBox m) where
    mcaseb = box . unbox


instance MStateB m s => MStateB (MStateBox (m s) SelfF) s where
instance MStateR m s => MStateR (MStateBox (m s) SelfF) s where
    mget = box mget


instance (Monad s,
          MStateB (MStateBox s attr1) v, 
          VStateB (VPropertyBox v attr2) u)
    => MStateB (MStateBox s (Child attr1 attr2)) u where

instance (Monad s,
          MStateR (MStateBox s attr1) v, 
          VStateR (VPropertyBox v attr2) u)
    => MStateR (MStateBox s (Child attr1 attr2)) u where
    mget = mcaseb $ (mget :: MStateBox s attr1 v v) >>= 
        return . vget . (vcaseb :: ToAttr attr2) . to_self
-- -}
