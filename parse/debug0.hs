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


import ValueStateB_R
import Boxed
import ValueStateB_R
import ValueStateB_R__test

import Boxed
import qualified Control.Monad.State as M


import Boxed
import SeedUtils


import qualified Control.Monad.State.Class as M
import qualified Control.Monad.State as MS
import Control.Monad (liftM, ap)


newtype WrapMonadState s m a = WrapMonadState (m a) deriving (Monad)
wget :: M.MonadState s m => WrapMonadState s m s
wget = WrapMonadState M.get
wset :: M.MonadState s m => s -> WrapMonadState s m ()
wset = WrapMonadState . M.put
unWrapMonadState :: M.MonadState s m => WrapMonadState s m a -> m a
unWrapMonadState (WrapMonadState ma) = ma

instance M.MonadState s m => M.MonadState s (WrapMonadState s m) where
    get = wget
    put = wset
instance M.MonadState s m => MStateB (WrapMonadState s m) s where
instance M.MonadState s m => MStateR (WrapMonadState s m) s where
    mgetb = wget
    --msetb = wset




type Mo = WrapMonadState
type MoD = Mo D

mget_self :: MPropertyR (MPropertyBox m) SelfF v => MPropertyBox m SelfF v v
mget_self = mget
unbox_mself :: MStateB m s => MPropertyBox m SelfF s a -> m a
unbox_mself = unbox
s1 = M.runState (unWrapMonadState mgetb) d
g m = M.runState (unWrapMonadState $ m) d
s2 = g $ unbox_mself mget
s3 = g $ unbox mget_self
s4 = g $ unbox $ toSizeM mget_self
s5 = g $ unbox $ (toSizeM mget_self >> mget) -- dead loop!!! why???

main = print s5
--toSizeM :: MGetAttr SizeF
toSizeM :: (MPropertyB (MPropertyBox s) (Child p SizeF) v
                ,MPropertyB (MPropertyBox s) p u)
                => MPropertyBox s p u a -> MPropertyBox s (Child p SizeF) v a
toSizeM = mcase

type MXX = MoD (M.State D)
-- R MXX D
-- PR (B MXX SelfF) D
xx :: MPropertyBox MXX SelfF D D
xx = mget_self

-- yy :: MPropertyBox MXX (Child SelfF SizeF) Int D
yy = toSizeM xx
zz :: MPropertyBox MXX (Child SelfF SizeF) Int Int
zz = mget -- seq zz () -- dead!!!
dead = seq zz ()

aa :: MPropertyBox MXX SelfF D D
aa = mget
bb :: MPropertyBox MXX SelfF D Int
bb = aa >>= 
        return . vget . (vcase :: VGetAttr SizeF) . as_vself
zz' :: MPropertyBox MXX (Child SelfF SizeF) Int Int
zz' = mcase bb -- seq zz' () -- well!!!
well = seq zz' ()

{-
--toSelfP :: s -> VPropertyBox s SelfF s
--toSelfP = box
toSelfM = as_mself
sd = mget . toSelfM $ d



-- toSizeP :: VPropertyBox s p u -> VPropertyBox s (Child SizeF p) v


-- self :: VPropertyBox D SelfF D
self = toSelfP d
-- ss :: VPropertyBox D (Child SelfF SizeF) v
ss = toSizeP self
sv = vget $ (vcase :: VGetAttr SizeF) self -- vget ss
self' = to_parent ss -- == self


















--}
--}
--}















-- B - Basic; R - Read;
-- M - Monad


class Monad m => MStateB m v | m -> v
class MStateB m v => MStateR m v where
    mgetb :: m v
    default mgetb :: (MStatePR n v, n v ~ m) => (n v) v
    mgetb = mget

class MStateB (n v) v => MStatePB n v | n -> v
class (MStatePB n v, MStateR (n v) v) => MStatePR n v where
    mget :: (n v) v

class MCaseB bm where
    mcaseb :: bm p v a -> bm q u a
class (MStatePB (bm name) v, MCaseB bm)
    => MPropertyB bm name v | bm name -> v where -- auto
    mcase :: (MPropertyB bm name' u) => bm name v a -> bm name' u a
class (MStatePR (bm name) v, MPropertyB bm name v)
    => MPropertyR bm name v -- auto



instance (MStatePB (bm name) v, MCaseB bm)
    => MPropertyB bm name v where -- auto
    mcase = mcaseb -- dead loop head ??
instance (MStatePR (bm name) v, MPropertyB bm name v)
    => MPropertyR bm name v -- auto




newtype MPropertyBox m attrs v a = MPropertyBox { unMPropertyBox :: (m a) }
    -- deriving (Monad)
instance Monad m => Monad (MPropertyBox m attrs v) where
    return = box . return
    bma >> bmb = box $ unbox bma >> unbox bmb
    bma >>= bf = box $ unbox bma >>= unbox . bf -- cannot access this function??
instance Boxed (m a) (MPropertyBox m name v a) where
    box = MPropertyBox
    unbox = unMPropertyBox
instance MCaseB (MPropertyBox m) where
    mcaseb = box . unbox


instance MStatePB (MPropertyBox m attrs) v
    => MStateB (MPropertyBox m attrs v) v
instance MStatePR (MPropertyBox m attrs) v
    => MStateR (MPropertyBox m attrs v) v


instance MStateB m s => MStatePB (MPropertyBox m SelfF) s where
instance MStateR m s => MStatePR (MPropertyBox m SelfF) s where
    -- ?? bug: mget = box mgetb -- dead loop
    mget = box (mgetb :: m s)
    -- mget :: B m SelfF s s --> mgetb :: m s --may--> mget :: m s
    -- len of mget signature decrease
    -- but what is m??
    -- if m = B m' attr s
    -- the len of attr maynot dec
    -- B (B m' attr s) Self --may--> B m' attr
    -- len of (B m attr) dec


instance (Monad m,
          MStatePB (MPropertyBox m attr1) v, 
          VStatePB (VPropertyBox v attr2) u)
    => MStatePB (MPropertyBox m (Child attr1 attr2)) u where


data SameType a = SameType a a
-- bug: when v = u, attr2 = SelfF/Wrapped/...
--      (mget :: MPropertyBox s attr1 v v) -> (mget :: same)
--      Child attr1 SelfF
instance (Monad m,
          MStatePR (MPropertyBox m attr1) v, 
          VStatePR (VPropertyBox v attr2) u)
    => MStatePR (MPropertyBox m (Child attr1 attr2)) u where
    mget = out where -- mc $ mp >>= f where
        -- :break follow lines
        -- mc = mcase -- break here and :continue ==>> dead
        out = muu1
        muu2 = box $ ((unbox mp :: m v) >>= f2 :: m u) -- this is fine!
        tmp1 = SameType out muu1
        tmp2 = SameType out muu2
        b = box
        u = unbox
        -- (>>>===) = (>>=) -- overflow here!!!
        -- (>>>===) = (>>=) :: forall a b. MPropertyBox m attr1 v a -> (a->MPropertyBox m attr1 v b) -> MPropertyBox m attr1 v b -- overflow here!!!
        -- (>>>===) = (>>=) :: forall . MPropertyBox m attr1 v v -> (v->MPropertyBox m attr1 v u) -> MPropertyBox m attr1 v u -- overflow here!!!
        ddd0 = id (>>=) -- must overflow here; can not access (>>=)
        -- 1 -> 0 -> 2 -> 0
        ddd1 = id 1 -- overflow after me
        ddd2 = id 2 -- overflow before me
        ddd3 = seq ddd1 ddd4
        ddd4 = seq ddd0 ddd5
        ddd5 = seq ddd2 ddd0

        (>>>===) = ddd3
        mp' = (mp >>>===) -- break here
        mvu = mp' f
        mu = u mvu
        --muu = mc mvu
        muu1 = b mu -- this is dead
        mp = mp_ -- does not break here!!
        mp_ = (mget :: MPropertyBox m attr1 v v)
        f = return . vget . (vcase :: VGetAttr attr2) . as_vself
        f2 = return . vget . (vcase :: VGetAttr attr2) . as_vself
    {-mget = ((mcaseb :: MPropertyBox m attr1 v u -> MPropertyBox m (Child attr1 attr2) u u)
        $ ((mget :: MPropertyBox m attr1 v v) >>= 
        return . vget . (vcase :: VGetAttr attr2) . as_vself
        :: MPropertyBox m attr1 v u)
        :: MPropertyBox m (Child attr1 attr2) u u)
    -}
    --mget = mcase $ (mget :: MPropertyBox m attr1 v v) >>= 
    --    return . vget . (vcase :: VGetAttr attr2) . as_vself
    -- mget :: B m (Child p c) u u --> mget :: B m p v v
    -- attr name of mget signature decrease
    -- len of (B m attr) dec


as_mself :: MStateB m s => m a -> MPropertyBox m SelfF s a
as_mself = box
to_mparent :: (MPropertyB (MPropertyBox m) (Child p c) v
              ,MPropertyB (MPropertyBox m) p u)
           => MPropertyBox m (Child p c) v a -> MPropertyBox m p u a
to_mparent = mcase

--  toXxxM :: MGetAttr XxxF
--  toXxxM = mcase


type MCaseAttr q =
    forall s p u v a.
                (MPropertyB (MPropertyBox s) p u
                ,MPropertyB (MPropertyBox s) q v)
                => MPropertyBox s p u a -> MPropertyBox s q v a
type MGetAttr c =
    forall s p u v a. -- SHOULD NO u v, since s p c -> u, v
                (MPropertyB (MPropertyBox s) (Child p c) v
                ,MPropertyB (MPropertyBox s) p u)
                => MPropertyBox s p u a -> MPropertyBox s (Child p c) v a
type MIsAttr c =
    forall s p v a.
                MPropertyB (MPropertyBox s) (Child p c) v
                => MPropertyBox s (Child p c) v a


-- -}



