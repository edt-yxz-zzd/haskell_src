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



module WrapMonadState where
import Boxed
import SeedUtils
import MonadStateB_R
import MonadStateW_U

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
instance M.MonadState s m => MStateW (WrapMonadState s m) s where
    msetb = wset
instance M.MonadState s m => MStateU (WrapMonadState s m) s where
    mupdateb f = wget >>= wset . f


