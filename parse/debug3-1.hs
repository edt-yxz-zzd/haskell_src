
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


-- remove B PB, dead; but if remove B PB R, no dead

import Boxed
import SeedUtils
import ValueStateB_R
import ValueStateB_R__test
import qualified Control.Monad.State as M



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

instance M.MonadState s m -- => MStateR (WrapMonadState s m) s
    => MStatePR (MPropertyBox (WrapMonadState s m) SelfF) s where
    mget = box wget
{-
instance M.MonadState s m => MStateR (WrapMonadState s m) s where
    mgetb = wget
    --msetb = wset
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
-}


----------------------------------------



type MXX = WrapMonadState D (M.State D)


zz :: MPropertyBox MXX (Child SelfF SizeF) Int Int
zz = mget -- seq zz () -- dead!!!
dead = seq zz ()


-------------------------------------
















-- B - Basic; R - Read;
-- M - Monad






class Monad (n v) => MStatePR n v | n -> v where
    mget :: (n v) v

class MCaseB bm where
    mcaseb :: bm p v a -> bm q u a
class (MStatePR (bm name) v, MCaseB bm)
    => MPropertyR bm name v | bm name -> v where -- auto
    mcase :: (MPropertyR bm name' u) => bm name v a -> bm name' u a


instance (MStatePR (bm name) v, MCaseB bm)
    => MPropertyR bm name v where -- auto
    mcase = mcaseb




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





-- -}



