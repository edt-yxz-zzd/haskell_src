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

module ListGt0
    ( ListGt0 (..)
    , mkNN, nn_head, nn_tail
    , list2nonnull, unsafe_list2nonnull
    , nonnull2list
    )
where

import Boxed
import Prelude hiding (lookup, foldr)
import Control.Applicative
import Control.Monad
import Data.Foldable as F
import Data.Traversable

-- (a, ls) <==> (a:ls)
newtype ListGt0 a = ListGt0 { unListGt0 :: (a, [a]) }
    deriving (Show, Read, Eq, Ord)
type NN = ListGt0

-- cannot alias data constructor!!
-- NN = ListGt0
-- mkNN a ls = ListGt0 (a, ls)
mkNN = curry ListGt0
unNN = unListGt0
instance Boxed (a, [a]) (ListGt0 a) where
    box = ListGt0
    unbox = unNN


nn_head (ListGt0 (a, _)) = a
nn_tail (ListGt0 (_, ls)) = ls
list2nonnull (a:ls) = Just $ mkNN a ls
list2nonnull _ = Nothing

unsafe_list2nonnull (a:ls) = mkNN a ls
unsafe_list2nonnull _ = error "unsafe_list2nonnull []"

nonnull2list :: ListGt0 a -> [a]
nonnull2list (ListGt0 (a, ls)) = a:ls
iter = nonnull2list


instance Functor ListGt0 where
    fmap f (ListGt0 (h, ts)) = mkNN (f h) $ fmap f ts
instance Applicative ListGt0 where
    pure = return
    (<*>) = ap
instance Monad ListGt0 where
    nn >>= f = unsafe_list2nonnull $ iter nn >>= iter . f
    return a = mkNN a []
--instance Foldable ListGt0 where
--    foldr f b = foldr f b . iter
instance Foldable ListGt0 where
    fold = F.fold . iter
    foldMap f = F.foldMap f . iter
    foldr f r = F.foldr f r . iter
    foldl f l = F.foldl f l . iter
    foldr1 f = F.foldr1 f . iter
    foldl1 f = F.foldl1 f . iter

instance Traversable ListGt0 where
    traverse f (ListGt0 (h, ts)) = mkNN <$> f h <*> traverse f ts



