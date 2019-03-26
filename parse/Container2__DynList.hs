{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}



module Container2__DynList
where

import Container2__base
import Container2__Set
import Container2__Map
import Container2__Buffer
import Ordering
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Foldable as F (toList, Foldable)

import ExplainEx
import OpDynTheOnlyValue
import XInt
import SeedUtils (unjust, lift2, take__ls)
import qualified Prelude as P
import Prelude hiding (lookup, null)
import Value_Just
import ListGt0
import Language.Haskell.TH
import Language.Haskell.Syntax
import SeedUtils__TH (def__instances)
import qualified IntegerSet__FiniteRange as IntS
import IntegerSet__FiniteRange (IntS)


class   ( Container a, Container c
        , Element a ~ Element c
        )
    => OpDynDropEx a c where
    -- drop_ex n a = c ==>>
    --      len c = max {0, len a - n}
    drop_ex :: UInt -> a -> c
    -- default drop_ex :: OpDynSplitAtEx a b c => UInt -> a -> c
class   ( Container a, Container b
        , Element a ~ Element b
        )
    => OpDynTakeEx a b where
    -- take_ex n a = b ==>>
    --      len b <= n
    --      len b < n --> len a = len b < n && b == a
    take_ex :: UInt -> a -> b
class   ( Container a, Container b, Container c
        , Element a ~ Element b
        , Element a ~ Element c
        , OpDynTakeEx a b
        , OpDynDropEx a c
        )
    => OpDynSplitAtEx a b c where
    -- splitAt_ex n a = (b,c) ==>>
    --      (b,c) == (take_ex n a, drop_ex n a)
    --      len b <= n && len b + len c == len a
    --      len b < n --> len c == 0 && len a == len b < n
    --
    splitAt_ex :: UInt -> a -> (b, c)
class OpDynTakeEx a a => OpDynTake a where
    take :: UInt -> a -> a
    take = take_ex
instance OpDynTakeEx a a => OpDynTake a
class OpDynDropEx a a => OpDynDrop a where
    drop :: UInt -> a -> a
    drop = drop_ex
instance OpDynDropEx a a => OpDynDrop a
class OpDynSplitAtEx a a a => OpDynSplitAt a where
    splitAt :: UInt -> a -> (a, a)
    splitAt = splitAt_ex
instance OpDynSplitAtEx a a a => OpDynSplitAt a


_gf_n_ls :: (i~UInt) -- (Integral i)
         => (Int -> [a]->b) -> (Integer -> [a]->b) -> i -> [a] -> b
_gf_n_ls fInt fi i = f (P.toInteger i) where
    f i | i <= P.toInteger (maxBound :: Int)
        = fInt (P.fromInteger i)
    f i = fi i

instance OpDynDropEx [a] [a] where
    drop_ex = _gf_n_ls P.drop L.genericDrop
instance OpDynTakeEx [a] [a] where
    take_ex = _gf_n_ls P.take L.genericTake
instance OpDynSplitAtEx [a] [a] [a] where
    splitAt_ex = _gf_n_ls P.splitAt L.genericSplitAt

    {-
    splitAt_ex n ls | n <= P.fromIntegral (maxBound :: Int)
        = splitAt (P.fromIntegral n) ls
    splitAt_ex n ls = f (toInteger n) ls where
        f n ls = genericSplitAt n ls
    splitAt_ex n ls = f (toInteger n) ls where
        f n ls | n == 0 = ([], ls)
        f n (h:ls) = let (hs, ts) = f (n-1) ls in (h:hs, ts)
        f n [] = ([], [])
    -}



