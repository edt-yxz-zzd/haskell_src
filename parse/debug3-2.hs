
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

-- remove B PB, dead;

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
instance M.MonadState s m => MStateR (WrapMonadState s m) s where
    mgetb = wget
    --msetb = wset

----------------------------------------


type MXX = WrapMonadState D (M.State D)


zz :: MPropertyBox MXX (Child SelfF SizeF) Int Int
zz = mget -- seq zz () -- dead!!!
dead = seq zz ()


-------------------------------------
















-- B - Basic; R - Read;
-- M - Monad



class Monad m => MStateR m v | m -> v where
    mgetb :: m v
    default mgetb :: (MStatePR n v, n v ~ m) => (n v) v
    mgetb = mget


class MStateR (n v) v => MStatePR n v | n -> v where
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



instance MStatePR (MPropertyBox m attrs) v
    => MStateR (MPropertyBox m attrs v) v


-- len of (B m attr) dec: MPropertyBox m SelfF -->> m
instance MStateR m s => MStatePR (MPropertyBox m SelfF) s where
    mget = box (mgetb :: m s)






data SameType a = SameType a a

-- len of (B m attr) dec: MPropertyBox m (Child attr1 attr2) -->> MPropertyBox m attr1
instance (Monad m,
          MStatePR (MPropertyBox m attr1) v, 
          VStatePR (VPropertyBox v attr2) u)
    => MStatePR (MPropertyBox m (Child attr1 attr2)) u where
    mget = out where
        out = muu1
        muu2 = box $ ((unbox mp :: m v) >>= f2 :: m u) -- this is fine!
        tmp1 = SameType out muu1
        tmp2 = SameType out muu2


        -- (>>>===) = (>>=) :: forall . MPropertyBox m attr1 v v -> (v->MPropertyBox m attr1 v u) -> MPropertyBox m attr1 v u -- overflow here!!!
        ddd0 = id (>>=) -- must overflow here; can not access (>>=)
        -- 1 -> 0 -> 2 -> 0
        ddd1 = id 1 -- overflow after me
        ddd2 = id 2 -- overflow before me
        ddd3 = seq ddd1 ddd4
        ddd4 = seq ddd0 ddd5
        ddd5 = seq ddd2 ddd0

        (>>>===) = ddd3
        mvu = mp >>>=== f1
        muu1 = mcase mvu -- this is dead
        -- does not break here!!
        mp = (mget :: MPropertyBox m attr1 v v)
        f1 = return . vget . (vcase :: VGetAttr attr2) . as_vself
        f2 = return . vget . (vcase :: VGetAttr attr2) . as_vself







-- -}



