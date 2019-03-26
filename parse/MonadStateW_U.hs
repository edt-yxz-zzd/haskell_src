


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

module MonadStateW_U where

import ValueStateB_R
import ValueStateW_U
import MonadStateB_R
import Boxed



class MStateB m v => MStateW m v where
    msetb :: v -> m ()
    default msetb :: (MStatePW n v, n v ~ m) => v -> (n v) ()
    msetb = mset
class (MStatePB n v, MStateW (n v) v) => MStatePW n v where
    mset :: v -> (n v) ()
class MStateW m v => MStateU m v where
    mupdateb :: (v->v) -> m ()
    default mupdateb :: (MStatePU n v, n v ~ m) => (v->v) -> (n v) ()
    mupdateb = mupdate
class (MStatePW n v, MStateU (n v) v) => MStatePU n v where
    mupdate :: (v->v) -> (n v) ()
    default mupdate :: (MStatePR n v, n v ~ m) => (v->v) -> (n v) ()
    mupdate f = mget >>= mset . f

class (MStateR m v, MStateU m v) => MStateRW m v where
class (MStatePR n v, MStatePU n v, MStateRW (n v) v)
    => MStatePRW n v where
instance (MStateR m v, MStateU m v) => MStateRW m v where
instance (MStatePR n v, MStatePU n v, MStateRW (n v) v)
    => MStatePRW n v where


class (MStatePW (bm name) v, MPropertyB bm name v)
    => MPropertyW bm name v -- auto
class (MStatePU (bm name) v, MPropertyW bm name v)
    => MPropertyU bm name v -- auto
instance (MStatePW (bm name) v, MPropertyB bm name v)
    => MPropertyW bm name v -- auto
instance (MStatePU (bm name) v, MPropertyW bm name v)
    => MPropertyU bm name v -- auto












instance MStatePW (MPropertyBox m attrs) v
    => MStateW (MPropertyBox m attrs v) v
instance MStatePU (MPropertyBox m attrs) v
    => MStateU (MPropertyBox m attrs v) v where
    -- mgetb = mget


instance MStateW m s => MStatePW (MPropertyBox m SelfF) s where
    mset = box . msetb
instance MStateU m s => MStatePU (MPropertyBox m SelfF) s where
    mupdate = box . mupdateb





instance (Monad m,
          MStatePU (MPropertyBox m attr1) v, 
          VStatePW (VPropertyBox v attr2) u)
    => MStatePW (MPropertyBox m (Child attr1 attr2)) u where
    mset u = mcase $ (mupdate f :: MPropertyBox m attr1 v ()) where
        f = unbox . vset u . (vcase :: VGetAttr attr2) . as_vself
instance (Monad m,
          MStatePU (MPropertyBox m attr1) v, 
          VStatePU (VPropertyBox v attr2) u)
    => MStatePU (MPropertyBox m (Child attr1 attr2)) u where
    mupdate g = mcase $ (mupdate f :: MPropertyBox m attr1 v ()) where
        f = unbox . vupdate g . (vcase :: VGetAttr attr2) . as_vself



-- -}
