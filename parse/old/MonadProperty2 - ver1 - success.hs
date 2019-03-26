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


{-
module MonadStateRW
    ( MonadStateR(..)
    , MonadStateW(..)
    , MonadStateM(..)
    , MonadStateRW(..)
    )
where
-}



{-
    pget/pset or mget/mset?
    if we use MonadState to implement MonadState
        then we need pget/pset
    but how we implement pget/pset?
        modify all those properties can be tedious

 -}



import Boxed
import Control.Monad.State.Class as M
import Control.Monad.State as MS
import Control.Monad (liftM)

class (Monad m, New p) => MonadNew p m where
    -- bug fixed: use liftN instead of wrap . f . unwrap
    lift_mp :: (a -> b) -> m (p a) -> m (p b)
    lift_mp f mpa = mpa >>= return . liftN f
    swap_mp :: m (p a) -> p (m a)
    swap_mp mpa = wrap $ mpa >>= return . unwrap
    swap_pm :: p (m a) -> m (p a)
    swap_pm pma = unwrap pma >>= return . wrap
    (>==) :: m (p a) -> (a -> m b) -> m (p b)
    mpa >== f = mpa >>= swap_pm . liftN f
instance (Monad m, New p) => MonadNew p m




-- Control.Applicative
class New p => PropertyR v p s | p s -> v where
    pget :: p s -> p v
    -- pget = liftN s2v
    -- pset :: v -> p s -> p s
class (Monad m, New p, PropertyR v p s) => MonadStateR v p s m
    -- m p -> ...
    | m -> s, p s -> v where
    mself :: m (p s)
    mget :: m (p v)
    mget = liftM pget mself
    mcall :: (v -> a) -> m (p a)
    mcall f = lift_mp f mget -- mget >>= return . liftN f

data D = D { size :: Int, time :: Int }
data SelfF
data SizeF
data TimeF
newtype SizeP a = SizeP a
instance New SizeP where
    wrap = SizeP
    unwrap (SizeP a) = a
instance PropertyR Int SizeP D where
    pget = liftN size

newtype TimeP a = TimeP a
instance New TimeP where
    wrap = TimeP
    unwrap (TimeP a) = a
instance PropertyR Int TimeP D where
    pget = liftN time

newtype WrapMonadState s m a = WrapMonadState (m a) deriving (Monad)
wget :: MonadState s m => WrapMonadState s m s
wget = WrapMonadState get
unWrapMonadState (WrapMonadState ma) = ma
instance (MonadState s m, PropertyR v p s)
    => MonadStateR v p s (WrapMonadState s m) where
    mself = liftM wrap wget
{-
instance (MonadState s m, PropertyR v SizeP s)
    => MonadStateR v SizeP s (WrapMonadState s m) where
    mself = liftM wrap wget
instance (MonadState D m, PropertyR Int SizeP D)
    => MonadStateR Int SizeP D (WrapMonadState D m) where
    mself = liftM wrap wget
-}

getSizeP :: MonadStateR v SizeP s m => m (SizeP v)
getSizeP = mget

--getSizedS :: MonadStateR v SizeP s m => m (SizeP s)
--getSizedS = mself

{-
instance (MonadState s m, PropertyR v TimeP s)
    => MonadStateR v TimeP s (WrapMonadState s m) where
    mself = liftM wrap wget
-}

getTimeP :: MonadStateR v TimeP s m => m (TimeP v)
getTimeP = mget

d = D {size=1, time=1000}
s = pget (SizeP d)
fd p = unwrap $ MS.evalState (unWrapMonadState p) d
ss = fd getSizeP
tt = fd getTimeP


{-
class Monad m => MonadStateW m s | m->s where
    mset :: s -> m ()
class MonadStateW p m s => MonadStateM p m s where
    -- s -> no update? or update with new state s
    mupdate_if :: (p m s -> Maybe (p m s)) -> m ()
    mupdate :: (p m s -> p m s) -> m ()
    mupdate f = mupdate_if (Just . f)
class (MonadStateR p m s, MonadStateM p m s)
    => MonadStateRW p m s where
    mexe_if :: (p m s -> Maybe (a, p m s)) -> m (Maybe a)
    mexe_if f = mcall f >>= \mp -> case mp of
        Nothing -> return Nothing
        Just (a, pms) -> mset pms  >> (return $ Just a)

    mexe :: (p m s -> (a, p m s)) -> m a
    mexe f = mcall f >>= \(a, pms) -> mset pms >> return a
instance (MonadStateR p m s, MonadStateM p m s)
    => MonadStateRW p m s


{-
-- Functional dependencies conflict between instance declarations
instance MonadStateRW (StateF SelfF) m s
    => M.MonadState (StateF SelfF m s) m where
    get = mget
    put = mput
-}



-- -}



