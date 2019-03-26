{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Seed.MonadOps
    ( module Seed.MonadOps
    , module Control.Applicative
    , module Control.Monad
    , module Data.Functor
    , module Data.Bifunctor
    )
where


import Data.Traversable
import Data.Foldable
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Bifunctor


voidA :: Applicative f => f ()
voidA = pure ()
ignoreF :: Functor f => f a -> f ()
ignoreF = void
{-
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
-}

infixl 1 >><
{-
(>><) :: Monad m => m a -> m b -> m a
ma >>< mb = do
    a <- ma
    mb
    return a
-}
(>><) :: Applicative m => m a -> m b -> m a
fa >>< fb = pure const <*> fa <*> fb
-- !!!!!!!! ifA =/= ifM !!!!!!!!!!!!!!!!
ifA :: Applicative f => f Bool -> f a -> f a -> f a
ifA fBool fThen fElse = pure f <*> fBool <*> fThen <*> fElse where
    f b t e = if b then t else e
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mBool mThen mElse = do
    b <- mBool
    if b then mThen else mElse




-- fold :: (Foldable t, Monoid m) => t m -> m
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b 
-- (Functor t, Foldable t) => Traversable t
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
concatTF_ex
    :: (Traversable t, Applicative f) => (t a -> b) -> t (f a) -> f b
concatTF_ex f = fmap f . sequenceA
swapTF :: (Traversable t, Applicative f) => t (f a) -> f (t a)
swapTF = sequenceA

concatTFM :: (Traversable t, Applicative f, Monoid a) => t (f a) -> f a
concatTFM = concatTF_ex fold
concatTFM_ex
    :: (Traversable t, Applicative f, Monoid b)
    => (a->b) -> t (f a) -> f b
concatTFM_ex = concatTF_ex . foldMap
foldTFr
    :: (Traversable t, Applicative f)
    => (a->b->b) -> b -> t (f a) -> f b
foldTFr f b = concatTF_ex $ foldr f b



class Monad m => Monad2Functor m where
    fmapM2F :: (a->b) -> (m a -> m b)
    fmapM2F f ma = ma >>= return . f
instance Monad m => Monad2Functor m where
instance Monad m => Monad2Applicative m where
class Monad m => Monad2Applicative m where
    pureM2App :: a -> m a
    pureM2App = return
    (<***>) :: m (a->b) -> (m a -> m b)
    mf <***> ma = do
        f <- mf
        a <- ma
        return $ f a



list2plusM :: MonadPlus m => [a] -> m a
list2plusM = foldr mplus mzero . map return

