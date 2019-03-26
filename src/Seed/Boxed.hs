{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeInType #-}

{-
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
-}

module Seed.Boxed
    ( Boxed(..)
    , New(..)
    , Label(..), ID
    , unlabel, label
    , replaceLabel, rightLabel, failLabel, relabel
    , mapGBox, mapGRBox
    , mapBox, mapRBox
    , opmapGBox, opmapGRBox
    , opmapbGBox, opmapbGRBox
    , opmapBox, opmapRBox
    , opmapbBox, opmapbRBox
    , mapNew, mapRNew
    , applyNew, applyRNew

    , unlabelP, constLabelP, proxyf2label, label2proxy -- , toProxy
    , Label2Name(..), Label2Data(..)
    )
where

import Control.Applicative
import Data.Proxy


type family Last x where
    Last (f a) = a
type family TFunc x where
    TFunc (f a) = f
class Boxed new where
    type BoxedFrom new
    type BoxedFrom new = Last new
    box :: BoxedFrom new -> new
    default box
        ::  ( f old ~ new, BoxedFrom new ~ old
            , old ~ Last new, f ~ TFunc new
            , New f) => BoxedFrom new -> new
    box = wrap
    unbox :: new -> BoxedFrom new
    default unbox
        ::  ( f old ~ new, BoxedFrom new ~ old
            , old ~ Last new, f ~ TFunc new
            , New f) => new -> BoxedFrom new
    unbox = unwrap
    {-# MINIMAL box, unbox #-}


mapGBox
    :: (Boxed new, old ~ BoxedFrom new
        , Boxed new2, old2 ~ BoxedFrom new2)
    => (old -> old2) -> (new -> new2)
mapGBox f = box . f . unbox
mapGRBox
    :: (Boxed new, old ~ BoxedFrom new
        , Boxed new2, old2 ~ BoxedFrom new2)
    => (new -> new2) -> (old -> old2)
mapGRBox f = unbox . f . box



mapBox
    :: (Boxed new, old ~ BoxedFrom new)
    => (old -> old) -> (new -> new)
mapBox = mapGBox
mapRBox
    :: (Boxed new, old ~ BoxedFrom new)
    => (new -> new) -> (old -> old)
mapRBox = mapGRBox




opmapGBox
    :: (Boxed new, old ~ BoxedFrom new
        , Boxed new2, old2 ~ BoxedFrom new2)
    => (old -> old2 -> a) -> (new -> new2 -> a)
opmapGRBox
    :: (Boxed new, old ~ BoxedFrom new
        , Boxed new2, old2 ~ BoxedFrom new2)
    => (new -> new2 -> a) -> (old -> old2 -> a)
opmapGBox op_old n1 n2 = unbox n1 `op_old` unbox n2
opmapGRBox op_new o1 o2 = box o1 `op_new` box o2


opmapbGBox
    :: (Boxed new, old ~ BoxedFrom new
        , Boxed new2, old2 ~ BoxedFrom new2
        , Boxed new3, old3 ~ BoxedFrom new3)
    => (old -> old2 -> old3) -> (new -> new2 -> new3)
opmapbGRBox
    :: (Boxed new, old ~ BoxedFrom new
        , Boxed new2, old2 ~ BoxedFrom new2
        , Boxed new3, old3 ~ BoxedFrom new3)
    => (new -> new2 -> new3) -> (old -> old2 -> old3)
opmapbGBox op_old n1 n2 = box $ unbox n1 `op_old` unbox n2
opmapbGRBox op_new o1 o2 = unbox $ box o1 `op_new` box o2



opmapBox
    :: (Boxed new, old ~ BoxedFrom new)
    => (old -> old -> a) -> (new -> new -> a)
opmapBox = opmapGBox
opmapRBox
    :: (Boxed new, old ~ BoxedFrom new)
    => (new -> new -> a) -> (old -> old -> a)
opmapRBox = opmapGRBox

opmapbBox
    :: (Boxed new, old ~ BoxedFrom new)
    => (old -> old -> old) -> (new -> new -> new)
opmapbBox f a = box . opmapBox f a
opmapbRBox
    :: (Boxed new, old ~ BoxedFrom new)
    => (new -> new -> new) -> (old -> old -> old)
opmapbRBox f a = unbox . opmapRBox f a


----------------------------------------------

class Applicative f => New f where
    -- for newtype
    -- type NewFrom a :: *
    wrap :: a -> f a
    wrap = pure
    unwrap :: f a -> a
    {-# MINIMAL unwrap #-}

mapNew :: New f => (a->b) -> f a -> f b
mapNew = fmap
mapRNew :: New f => (f a -> f b) -> a -> b
mapRNew fa2fb = unwrap . fa2fb . wrap

-- for <*>
-- (<*>) = applyNew
-- pure = wrap
applyNew :: New f => f (a->b) -> f a -> f b
applyNew = mapNew . unwrap
applyRNew :: New f => (f a -> f b) -> f (a->b)
applyRNew = wrap . mapRNew







----------------------------------------------
--type role ID representational
type ID = Label ()
type role Label phantom representational
newtype Label (name :: k) a = Label a
    deriving (Eq, Ord, Read, Show)
instance Boxed (Label name a) where
    type BoxedFrom (Label name a) = a
    box = Label
    unbox (Label a) = a
unlabel :: i -> Label i a -> a
unlabel _ = unbox
label :: i -> a -> Label i a
label i = Label

replaceLabel :: Applicative (f a) => Label a b -> f a b
replaceLabel = pure . unbox
failLabel :: Monad m => Label a String -> m a
failLabel = fail . unbox
rightLabel :: Label a b -> Either a b
rightLabel = replaceLabel -- return . unbox
relabel :: Label a c -> Label b c
relabel = box . unbox


{-
instance Functor (Label a) where
    fmap = mapNew
instance Applicative (Label a) where
    (<*>) = applyNew
    pure = wrap
-}

instance New (Label a) where
    wrap = box
    unwrap = unbox



-------------------------------
{-
instance {-# OVERLAPS #-} New f => Boxed (f a) where
    type BoxedFrom (f a) = a
    box = wrap
    unbox = unwrap
-}

instance {-# OVERLAPS #-} New f => Functor f where
    fmap = mapNew
instance {-# OVERLAPS #-} New f => Applicative f where
    (<*>) = applyNew
    pure = wrap



newtype XXX a b = XXX b
instance New (XXX a) where
    wrap = XXX
    unwrap (XXX b) = b
instance Boxed (XXX a b) where
    type BoxedFrom (XXX a b) = ()
    box = undefined
    unbox = undefined




--------------- Proxy

unlabelP :: proxy c -> Label c a -> a
unlabelP _ = unbox
constLabelP :: proxy c -> Label c a -> Label c a
constLabelP _ = id
proxyf2label :: (Proxy c -> a) -> Label c a
proxyf2label f = r where
    r = box . f $ label2proxy r
--proxy2label :: proxy c -> Label c a
--proxy2label _ = undefined
--label2proxy :: Label c a -> proxy c
--label2proxy = undefined
label2proxy :: Label c a -> Proxy c
label2proxy _ = Proxy



-------------
type family Label2Name x where
    Label2Name (Label n a) = n
type family Label2Data x where
    Label2Data (Label n a) = a

