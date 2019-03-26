{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Seed.AList
    ( module Seed.AList
    {-
    , Semigroup(..)
    , Boxed(..)
    , Monoid(..)
    , Explain(..), OpFrom(..)
    -}
    )
where



import Seed.Boxed
import Data.Semigroup
import Explain.ExplainBase

newtype AList a = AList ([a] -> [a])
instance Boxed (AList a) where
    type BoxedFrom (AList a) = [a] -> [a]
    box = AList
    unbox (AList a) = a
instance Semigroup (AList a) where
    AList a <> AList b = AList (a . b)
instance Monoid (AList a) where
    mappend = (<>)
    mempty = list2alist []
list2alist :: [a] -> AList a
list2alist = box . (++)
alist2list :: AList a -> [a]
alist2list (AList f) = f []

instance Explain [a] (AList a) where
    explain = from
instance Make [a] (AList a) where
    make = from
instance OpFrom [a] (AList a) where
    from = list2alist
instance OpFrom (AList a) [a] where
    from = alist2list




