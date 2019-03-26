{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

usage:
    to refine class
    e.g.
        class Set st sts => NDFA sts st sym a where
            transition :: a -> sym -> st -> sts
        class (Singleton st sts, NDFA sts st sym a) => DFA sts st sym a

-}

module Container_class_Set where
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import qualified Data.List as L
import qualified Data.Map as M -- (Map)
import qualified Data.Set as S -- (Set)
import qualified Prelude as P
import qualified Data.Bits as B
import Data.Monoid
import Prelude hiding (lookup, null, take, length)
-- import Prelude (Eq, Ord, Num, Integral, Integer, Real)
import Data.Bits ((.&.))
import SeedUtils -- hiding (safe_head)
import Ordering
import Boxed
import qualified Data.Sequence as Seq

import Container_class_base
------------------  Set
-- Set - static set
-- DynSet - dynamic set

class (OpMember a s, OpNull a s, SetOrd s) => Set a s where
    -- No OpEmpty; but OpNull
    -- empty is a constructor;
    --      which should belong to DynSet or SetWithOpEmpty!!
    -- e.g. Singleton is a Set always contain one element
    --      but have an empty set in concept
    subset :: s -> s -> Bool
    subset = (|<=|)
    -- Eq s ??
    -- what if using [] as a set?
    --      compare without order and counts!
    --  [] can not be set
    --      unless we wrap it as a set!
    --  now I distinguish Ord and SetOrd
    --      so [] can be used as set directly
    proper_subset :: s -> s -> Bool
    proper_subset = (|<|)
    -- proper_subset a b = subset a b && not (subset b a)
    set_eq :: s -> s -> Bool
    set_eq = (|==|)

    {-
    set_eq a b = subset a b && subset b a
    (~<=~), (~=~), (~<~) :: s -> s -> Bool
    (~<=~) = subset
    (~=~) = set_eq
    (~<~) = proper_subset
    -}

-- class Container a s => OpSetDifference a s where
-- why not container??
--  e.g. Bool : /\ == &&
--      False = {}, True = {{}}
class OpSetDifference s where
    difference :: s -> s -> s
    (\\) :: s -> s -> s
    (\\) = difference
class OpSetIntersection s where
    intersection :: s -> s -> s
    (/\), (/-\) :: s -> s -> s
    (/\) = intersection
    (/-\) = intersection

    default intersection :: OpSetDifference s => s -> s -> s
    intersection a b = a \\ (a \\ b)
class OpSetIntersection s => OpSetBiasedIntersection s where
    (-/\) :: s -> s -> s -- left biased intersection
    (/\-) :: s -> s -> s -- right biased intersection
    (/\-) = flip (-/\)
    default (-/\) :: OpSetDifference s => s -> s -> s
    a -/\ b = a \\ (a\\b)
class OpSetUnion s where
    union :: s -> s -> s
    (\/), (\-/) :: s -> s -> s
    (\/) = union
    (\-/) = union
class OpSetUnion s => OpSetBiasedUnion s where
    (-\/) :: s -> s -> s -- left biased union
    (\/-) :: s -> s -> s -- right biased union
    (\/-) = flip (-\/)
    default (-\/) :: OpSetDifference s => s -> s -> s
    a -\/ b = a \/ (b\\a)

class (OpSetIntersection s, OpSetDifference s)
    => OpSetSep s where
    sep :: s -> s -> (s, s, s) -- (0\\1, 0&1, 1\\0)
    sep a b = (a\\b, a/\b, b\\a)
    (//\\) :: s -> s -> (s, s, s)
    (//\\) = sep

class (OpSetBiasedIntersection s, OpSetSep s)
    => OpSetBiasedSep s where
    (-//\\) :: s -> s -> (s, s, s)
    a -//\\ b = (a\\b, a-/\b, b\\a)
    (//\\-) :: s -> s -> (s, s, s)
    a //\\- b = (a\\b, a/\-b, b\\a)
class (OpSetSep s, OpSetUnion s)
    => SetOp s where
    -- any container (not neccesary set) may have these operations
    -- may not be container!!
class OpUniversal s where -- v.s. OpEmpty
    universal :: s
class OpIsUniversal s where -- v.s. OpNull
    is_universal :: s

class OpUniversal s => OpComplement s where
    complement :: s -> s
    -- (~) = complement -- no prefix op and (~) is not valid id
    default complement :: OpSetDifference s => s -> s
    complement = (universal \\)
class (OpSetBiasedSep s, SetOp s)
    => SetBiasedOp s where
class (Set a s, OpUniversal s) => UniversalSet a s where
class (Set a s, Buffer a s, SetOp s) => DynSet a s where
    unions :: [s] -> s
    unions = foldr union empty
class (DynSet a s, UniversalSet a s, OpComplement s)
    => DynUniversalSet a s where


