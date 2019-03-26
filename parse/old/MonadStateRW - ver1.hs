{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , Rank2Types
            , KindSignatures
            , ScopedTypeVariables #-}



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





class Monad m => MonadStateR v p s m
    | p m -> s where
    mget :: p (m s)
    mcall :: (s -> a) -> m a
    mcall f = mget >>= return . f

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



