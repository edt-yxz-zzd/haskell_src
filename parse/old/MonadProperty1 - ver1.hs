{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , Rank2Types
            , KindSignatures
            , ScopedTypeVariables #-}
{-
module MonadPropertyRW
    ( MonadPropertyR(..)
    , MonadPropertyW(..)
    , MonadPropertyM(..)
    , MonadPropertyRW(..)
    )
where
-}


{-
    pget/pset or mget/mset?
    if we use MonadState to implement MonadProperty
        then we need pget/pset
    but how we implement pget/pset?
        modify all those properties can be tedious

 -}



import Boxed
import Control.Monad.State.Class as M

newtype PropertyF g f s v = PropertyF { unPropertyF :: v }
instance New (PropertyF g f m) where
    wrap = PropertyF
    unwrap (PropertyF v) = v

data SelfF
data SizeF
data TimeF
class New (p g f s) => SizeP p g f s v | s -> p g f v where
    -- e.g. pfs = PropertyF f s
    -- s -> f !! since this is  SizeP constaint !!!
    mkSizeP :: v -> p g f s v
    mkSizeP = wrap
    unSizeP :: p g f s v -> v
    unSizeP = unwrap


mgetSizeP :: (SizeP p g f (m()) v,
              MonadPropertyR p g f m v) => m v
mgetSizeP = mget >>= return . unSizeP

data D = D { size :: Int, time :: Int }

data ConcreteTypePG -- PropertyGroup
data MonadStatePG s

instance SizeP PropertyF ConcreteTypePG SizeF D Int
instance M.MonadState D m
    => SizeP PropertyF (MonadStatePG D) SizeF (m ()) Int

instance M.MonadState D m
    => MonadPropertyR PropertyF (MonadStatePG D) SizeF m Int where
    mget = get >>= return . mkSizeP . size
-- -}



{-
class New p => TimeP p s | p -> s where
    mkTimeP :: s -> p s
    mkTimeP = wrap
    unTimeP :: p s -> s
    unTimeP = unwrap
-- instance M.MonadState D m => TimeP (PropertyF TimeF D) Int

instance M.MonadState D m => MonadPropertyR (PropertyF TimeF) m Int where
    mget = get >>= return . mkTimeP . time
d = D 1 2

-- -}


type PM p g f m = (p g f (m ()) :: * -> *)
-- p :: (* -> *) -> * -> *
class (Monad m, New (PM p g f m)) => MonadPropertyR p g f m v
    | p g f m -> v where -- f m -> p??
    -- type PMS :: (* -> * -> *) -> (* -> *) -> *
    mget :: m (PM p g f m v)
    -- mget = get >>= return . wrap
    mcall :: (PM p g f m v -> a) -> m a
    mcall f = mget >>= return . f
{-
class Monad m => MonadPropertyW p m s | m->s where
    mset :: p m s -> m ()
class MonadPropertyW p m s => MonadPropertyM p m s where
    -- s -> no update? or update with new state s
    mupdate_if :: (p m s -> Maybe (p m s)) -> m ()
    mupdate :: (p m s -> p m s) -> m ()
    mupdate f = mupdate_if (Just . f)
class (MonadPropertyR p m s, MonadPropertyM p m s)
    => MonadPropertyRW p m s where
    mexe_if :: (p m s -> Maybe (a, p m s)) -> m (Maybe a)
    mexe_if f = mcall f >>= \mp -> case mp of
        Nothing -> return Nothing
        Just (a, pms) -> mset pms  >> (return $ Just a)

    mexe :: (p m s -> (a, p m s)) -> m a
    mexe f = mcall f >>= \(a, pms) -> mset pms >> return a
instance (MonadPropertyR p m s, MonadPropertyM p m s)
    => MonadPropertyRW p m s


{-
-- Functional dependencies conflict between instance declarations
instance MonadPropertyRW (PropertyF SelfF) m s
    => M.MonadState (PropertyF SelfF m s) m where
    get = mget
    put = mput
-}



-- -}



