{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}



module MonadEx where

import Control.Applicative

voidA :: Applicative f => f ()
voidA = pure ()
voidM :: Monad m => m ()
voidM = return ()
ignoreF :: Functor f => f a -> f ()
ignoreF = fmap $ const ()
ignoreA :: Applicative f => f a -> f ()
ignoreA = (*> voidA)
ignoreM :: Monad f => f a -> f ()
ignoreM = (>> voidM)
(>><) :: Monad m => m a -> m b -> m a
ma >>< mb = do
    a <- ma
    mb
    return a
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mBool mThen mElse = do
    b <- mBool
    if b then mThen else mElse
class (Monad m, Applicative m, Functor m) => Monadx m
instance (Monad m, Applicative m, Functor m) => Monadx m
