
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


module MonadNew where





import Boxed
import qualified Control.Monad.State.Class as M
import qualified Control.Monad.State as MS
import Control.Monad (liftM, ap)
import SeedUtils


-- liftM
-- appM :: Monad m => (a -> b) -> m a -> m b
-- appM = ap . return
class (Monad m, New p) => MonadNew p m where
    -- bug fixed: use liftN instead of wrap . f . unwrap
    lift_mp :: (a -> b) -> m (p a) -> m (p b)
    lift_mp f mpa = mpa >>= return . liftN f
    lift_pm :: (a -> b) -> p (m a) -> p (m b)
    lift_pm = liftN . liftM
    swap_mp :: m (p a) -> p (m a)
    swap_mp mpa = wrap $ mpa >>= return . unwrap
    swap_pm :: p (m a) -> m (p a)
    swap_pm pma = unwrap pma >>= return . wrap
    (>==) :: m (p a) -> (a -> m b) -> m (p b)
    mpa >== f = mpa >>= swap_pm . liftN f
    mwrap :: m a -> m (p a)
    mwrap = liftM wrap
    munwrap :: m (p a) -> m a
    munwrap = liftM unwrap

instance (Monad m, New p) => MonadNew p m



