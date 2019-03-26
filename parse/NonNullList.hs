{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module NonNullList
    ( NonNullList (..)
    , mkNN, nn_head, nn_tail
    , list2nonnull, unsafe_list2nonnull
    )
where

import Container
import Boxed
import Prelude hiding (lookup, foldr)
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable

-- (a, ls) <==> (a:ls)
newtype NonNullList a = NonNullList { unNonNullList :: (a, [a]) }
    deriving (Show, Read, Eq, Ord)
type NN = NonNullList

-- cannot alias data constructor!!
-- NN = NonNullList
-- mkNN a ls = NonNullList (a, ls)
mkNN = curry NonNullList
unNN = unNonNullList
instance Boxed (a, [a]) (NonNullList a) where
    box = NonNullList
    unbox = unNN


nn_head (NonNullList (a, _)) = a
nn_tail (NonNullList (_, ls)) = ls
list2nonnull (a:ls) = Just $ mkNN a ls
list2nonnull _ = Nothing

unsafe_list2nonnull (a:ls) = mkNN a ls
unsafe_list2nonnull _ = error "unsafe_list2nonnull []"



instance Applicative NonNullList where
    pure = return
    (<*>) = ap
instance Monad NonNullList where
    nn >>= f = unsafe_list2nonnull $ iter nn >>= iter . f
    return a = mkNN a []
instance Functor NonNullList where
    fmap f (NonNullList (h, ts)) = mkNN (f h) $ fmap f ts
instance Foldable NonNullList where
    foldr f b = foldr f b . iter
instance Traversable NonNullList where
    traverse f (NonNullList (h, ts)) = mkNN <$> f h <*> traverse f ts



instance Null a (NonNullList a) where
   null = const False
instance Container a (NonNullList a) where
instance Singleton a (NonNullList a) where
    singleton a = mkNN a []
instance Insert a (NonNullList a) where
    insert a (NonNullList (b, ls))= mkNN a (b:ls)
instance Iterable a (NonNullList a) where
    iter (NonNullList (a, ls)) = a:ls
instance AnyElem a (NonNullList a) where

instance IterLe a (NonNullList a)
instance UnsafeSized a (NonNullList a) where
    unsafe_len = (1 +) . unsafe_len . nn_tail
instance LenLe a (NonNullList a) where
    len_if_le n = fmap (1+) . len_if_le (n-1) . nn_tail

instance Seq a (NonNullList a) where
    safe_head = Just . nn_head
instance DynSeq a (NonNullList a) where
    (NonNullList (h, ts)) >< rhs = mkNN h $ ts ++ iter rhs
instance MapView Integer a (NonNullList a) where
    lookup i = lookup i . iter
    type MapView2KeySet (NonNullList a) = MapView2KeySetWrapper [a]
    key_set = key_set . iter



