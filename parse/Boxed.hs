{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , Rank2Types
            , KindSignatures
            , ScopedTypeVariables #-}
            -- 


{-
    use (liftN f) instead of wrap . f . unwrap
    since liftN f ==>> p a -> p b
    but wrap . f . unwrap ==>> p a -> pppppppp b !!!
 -}

module Boxed
    ( New(..)
    , Boxed(..)
    , WrapNew(..)
    , NamedNew(..)
    , liftN, liftN2, rliftN, rliftN2
    , appN
    , liftB, liftB2_, rliftB, rliftB2, rliftB2_
    )
where

import Control.Monad.State.Class as M
import Data.Monoid
import Control.Applicative
import Control.Monad

{-
instance New b => Functor b where
    fmap = liftN
instance New b => Applicative b where
    pure = wrap
    bf <*> bx = appN bf bx
-}


class Boxed o bo | bo -> o where
    -- cannot o->n since we will make many newtypes
    box :: o -> bo
    unbox :: bo -> o

liftB :: Boxed o bo => (o->o) -> bo -> bo
liftB f = box . f . unbox
rliftB :: Boxed o bo => (bo -> bo) -> o -> o
rliftB f = unbox . f . box

liftB2_ :: Boxed o bo => (o -> o -> c) -> bo -> bo -> c
liftB2_ f a b = f (unbox a) (unbox b)
rliftB2_ :: (Boxed o bo, Boxed o1 bo1)
    => (bo -> bo1 -> o2) -> o -> o1 -> o2
rliftB2_ f a b = f (box a) (box b)
rliftB2 :: (Boxed o bo, Boxed o1 bo1, Boxed o2 bo2)
    => (bo -> bo1 -> bo2) -> o -> o1 -> o2
rliftB2 f a b = unbox $ f (box a) (box b)



-- class (forall o. Boxed o (b o)) => New b
-- instance (forall o. Boxed o (b o)) => New b
{-
class New b where
    wrap :: o -> b o
    unwrap :: b o -> o
instance New b => Boxed o (b o) where
    box = wrap
    unbox = unwrap
-- but now we cannot define: instance Boxed (m a) (P b s m a)
-- Functional dependencies conflict between instance declarations
-- -}
class New b where
    wrap :: o -> b o
    unwrap :: b o -> o

liftN :: New b => (o -> o1) -> b o -> b o1
liftN f = wrap . f . unwrap
liftN2 :: New b => (o -> o1 -> o2) -> b o -> b o1 -> b o2
liftN2 f o o1 = wrap $ f (unwrap o) $ unwrap o1
rliftN :: New b => (b o -> b o1) -> o -> o1
rliftN f = unwrap . f . wrap
rliftN2 :: New b => (b o -> b o1 -> b o2) -> o -> o1 -> o2
rliftN2 f o o1 = unwrap . f (wrap o) $ wrap o1

appN :: New b => b (o -> o1) -> b o -> b o1
appN bf = liftN (unwrap bf)

newtype WrapNew b o = WrapNew (b o) -- deriving (Monad)

instance New b => New (WrapNew b) where
    wrap = WrapNew . wrap
    unwrap (WrapNew bo) = unwrap bo

instance New b => Boxed o (WrapNew b o) where
    box = wrap
    unbox = unwrap

newtype NamedNew name o = NamedNew { unNamedNew :: o }
instance New (NamedNew name) where
    wrap = NamedNew
    unwrap = unNamedNew














