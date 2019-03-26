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



-- 1) define newtype property XxxxP
-- 2)   instance New, PropertyR/W...
-- 3) instance MonadStateR/W...
-- 4)   may need to define a WrapConstaintNewType
-- 5) define convience access function: get/setXxxxP


import Boxed
import qualified Control.Monad.State.Class as M
import qualified Control.Monad.State as MS
import Control.Monad (liftM, ap)
import SeedUtils
import News
import WrapMonadState
import Property




class Monad (fm p) => MonadProperty fm p v | fm p -> v where
    mcase :: MonadProperty fm q u => fm p a -> fm q a
newtype PropertyM m p a = PropertyM { unPropertyM :: m a } deriving (Monad)

instance Boxed (m a) (PropertyM m p a) where
    box = PropertyM
    unbox = unPropertyM
instance Monad m => MonadProperty (PropertyM m) p v where
    mcase = box . unbox

mToSizeF :: (MonadProperty fm p v, MonadProperty fm SizeF u)
            => fm p a -> fm SizeF a
-- mFromSizeP ::
mToSizeF = mcase


class MonadProperty fm p v => MonadPropertyR fm p v where
    mget :: fm p v
instance (M.MonadState s m, PropertyR v (SizeP) s
          -- , MonadProperty (PropertyM (WrapMonadState s m)) SelfF s
          ) =>
    MonadPropertyR (PropertyM (WrapMonadState s m)) (SizeF) v where
    mget = box $ wget >>=
        return . rliftN (pget :: NamedNew SizeF s -> NamedNew SizeF v)
instance (M.MonadState s m, PropertyR v (SelfP) s
          -- , MonadProperty (PropertyM (WrapMonadState s m)) SelfF s
          ) =>
    MonadPropertyR (PropertyM (WrapMonadState s m)) (SelfF) v where
    mget = box $ wget >>=
        return . rliftN (pget :: NamedNew (SelfF) s -> NamedNew (SelfF) v)



data ChildF c p
type RootF f = ChildF f SelfF
type R f = f

{-
instance (Monad m, MonadProperty (PropertyM m) p s, 
          PropertyR v (NamedNew f) s) =>
          MonadProperty (PropertyM m) (Child f p) v where
-}

instance (Monad m, MonadPropertyR (PropertyM m) p s, 
          -- MonadProperty (PropertyM m) (ChildF f p) v,
          PropertyR v (NamedNew f) s) =>
          MonadPropertyR (PropertyM m) (ChildF f p) v where
    mget = mcase $ (mget :: (PropertyM m) p s) >>= 
            return . rliftN (pget :: NamedNew f s -> NamedNew f v)

m2ParentF :: (MonadProperty fm (ChildF c p) v, MonadProperty fm p s)
    => fm (ChildF c p) a -> fm p a
m2ParentF = mcase







--M.MonadState s m => 



-- mgetSizeF :: (MonadPropertyR fm SelfF s, 
--              PropertyR v (NamedNew SizeF) s)
{-
mgetSizeF :: (MonadPropertyR fm (SizeF) v)
            => fm (SizeF) v
mgetSizeF = mget
-- -}

mgetSizeF :: (MonadPropertyR fm (ChildF SizeF SelfF) v)
            => fm (ChildF SizeF SelfF) v
mgetSizeF = mget
-- -}
mgetSelfF :: (MonadPropertyR fm (SelfF) v)
            => fm (SelfF) v  -- SelfF
mgetSelfF = mget
-- mgetSizeF = mgetSelfF
d = D {size = 324, time = -123}
-- eval d m = MS.runState (unWrapMonadState $ unPropertyM m) d
eval d m = MS.runState (unWrapMonadState $ unPropertyM m) d
r = eval d mgetSizeF
s = eval d mgetSelfF
p = eval d $ m2ParentF mgetSizeF

















