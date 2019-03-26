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



module Container2__DynSet
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


infixl 9 \\, |-|, \\~
infixr 3 /-\, //\\, -/\, /\-, -//\\, //\\-, /?\, /-\~, ~/-\
infixr 2 \-/, \+/, -\/, \/-, \-/~, ~\-/

class OpSetDisjoint s where
    (/?\), disjoint, not_disjoint :: s -> s -> Bool
    (/?\) = not_disjoint
    not_disjoint a = P.not . disjoint a
    default disjoint :: (OpNull s, OpSetIntersection s)
        => s -> s -> Bool
    disjoint a b = null $ a /-\ b

class OpSetSymmetricDifference s where
    -- a |-| b = (a \-/ b) \\ (a /-\ b) = (a\\b) \+/ (b\\a)
    (|-|), symmetric_difference :: s -> s -> s
    (|-|) = symmetric_difference
    default symmetric_difference
        :: (OpSetDifference s, OpSetAdd s) => s -> s -> s
    symmetric_difference a b = set_add (a\\b) (b\\a)
class OpSetDifference s where
    (\\), difference :: s -> s -> s
    (\\) = difference
    (\\~) :: s -> [s] -> s
    (\\~) = foldl (\\)
class OpSetIntersection s where
    (/-\), intersection :: s -> s -> s
    (/-\) = intersection

    default intersection :: OpSetDifference s => s -> s -> s
    intersection a b = a \\ (a \\ b)
    (/-\~) :: s -> [s] -> s
    (~/-\) :: [s] -> s -> s
    (/-\~) = foldl (/-\)
    (~/-\) = flip $ foldr (/-\)
class OpSetIntersection s => OpSetBiasedIntersection s where
    (-/\) :: s -> s -> s -- left biased intersection
    (/\-) :: s -> s -> s -- right biased intersection
    (/\-) = flip (-/\)
    default (-/\) :: OpSetDifference s => s -> s -> s
    a -/\ b = a \\ (a\\b)
class OpSetAdd s where
    -- union a b while a /-\ b == {}
    -- if a /-\ b /= {} then undefined behavior
    (\+/), set_add :: s -> s -> s
    (\+/) = set_add

class OpSetUnion s where
    (\-/), union :: s -> s -> s
    (\-/) = union
    default union :: (OpSetAdd s, OpSetDifference s) => s -> s -> s
    union a b = a \+/ (b\\a)
    (\-/~) :: s -> [s] -> s
    (~\-/) :: [s] -> s -> s
    (\-/~) = foldl (\-/)
    (~\-/) = flip $ foldr (\-/)
class OpSetUnion s => OpSetBiasedUnion s where
    (-\/) :: s -> s -> s -- left biased union
    (\/-) :: s -> s -> s -- right biased union
    (\/-) = flip (-\/)
    default (-\/) :: (OpSetAdd s, OpSetDifference s) => s -> s -> s
    a -\/ b = a \+/ (b\\a)

class (OpSetIntersection s, OpSetDifference s)
    => OpSetSep s where
    sep :: s -> s -> (s, s, s) -- (0\\1, 0&1, 1\\0)
    sep a b = (a\\b, a/-\b, b\\a)
    (//\\) :: s -> s -> (s, s, s)
    (//\\) = sep

class (OpSetBiasedIntersection s, OpSetSep s)
    => OpSetBiasedSep s where
    (-//\\) :: s -> s -> (s, s, s)
    a -//\\ b = (a\\b, a-/\b, b\\a)
    (//\\-) :: s -> s -> (s, s, s)
    a //\\- b = (a\\b, a/\-b, b\\a)
class OpDynUniversal s where -- v.s. OpEmpty
    universal :: s
class OpIsUniversal s where -- v.s. OpNull
    is_universal :: s -> Bool

class OpDynUniversal s => OpDynComplement s where
    complement :: s -> s
    -- (~) = complement -- no prefix op and (~) is not valid id
    default complement :: OpSetDifference s => s -> s
    complement = (universal \\)



def__instances [d|
    class   ( OpSetSep s, OpSetUnion s
            , OpSetSymmetricDifference s, OpSetDisjoint s)
        => SetOp s where
        -- any container (not neccesary set) may have these operations
        -- may not be container!!
    class (OpSetBiasedSep s, SetOp s)
        => SetBiasedOp s where
    |]
class (Set s, OpDynUniversal s) => UniversalSet s where
class (Set s, Buffer s, SetOp s) => DynSet s where
    unions :: [s] -> s
    unions = foldl union empty
class (DynSet s, UniversalSet s, OpDynComplement s, OpIsUniversal s)
    => DynUniversalSet s










--------------------------------

instance Ord a => OpSetDisjoint (D_Set a)
instance Ord a => OpSetSymmetricDifference (D_Set a)
instance Ord a => OpSetDifference (D_Set a) where
    difference = (S.\\)
instance Ord a => OpSetAdd (D_Set a) where
    set_add = (S.union)
instance Ord a => OpSetIntersection (D_Set a) where
    intersection = (S.intersection)
instance Ord a => OpSetBiasedIntersection (D_Set a) where
    (-/\) = S.intersection
instance Ord a => OpSetUnion (D_Set a) where
    union = S.union
instance Ord a => OpSetBiasedUnion (D_Set a) where
    (-\/) = S.union
instance Ord a => OpSetSep (D_Set a) where
instance Ord a => OpSetBiasedSep (D_Set a) where
instance Ord a => DynSet (D_Set a) where









instance OpSetDisjoint D_IntSet
instance OpSetSymmetricDifference D_IntSet
instance OpSetDifference D_IntSet where
    difference = (IS.\\)
instance OpSetAdd D_IntSet where
    set_add = (IS.union)
instance OpSetIntersection D_IntSet where
    intersection = (IS.intersection)
instance OpSetBiasedIntersection D_IntSet where
    (-/\) = IS.intersection
instance OpSetUnion D_IntSet where
    union = IS.union
instance OpSetBiasedUnion D_IntSet where
    (-\/) = IS.union
instance OpSetSep D_IntSet where
instance OpSetBiasedSep D_IntSet where
instance DynSet D_IntSet where









------------------------
--IntS

instance Container IntS where
    type Element IntS = Integer
instance SetConcept IntS
instance OpMember IntS where
    type MemberExResult IntS = Bool
    member_ex = IntS.member
    member = member_ex
instance SetEq IntS where
    (|==|) = (==)
instance SetOrd IntS where
    type SetCmpExResult IntS = POrd
    (|<|) = IntS.isProperSubsetOf
    (|<=|) = IntS.isSubsetOf
instance LinearElementsSetBase IntS where
    before _ = compare
        -- undefined behavior if e1 e2 not in a
instance ClockwiseElementsSetBase IntS where
    clockwise _ = default_clockwise__byOrd
        -- undefined behavior if e1 e2 e3 not in a



instance CountableContainer IntS
--instance FiniteContainer IntS
--instance SequenceConcept IntS
instance OpNull IntS where
    type NullExResult IntS = Bool
    null_ex = IntS.null
    null = null_ex
instance OpUnsafeLen IntS where
    unsafe_len = unjust . IntS.safe_len
instance OpLenIfLe IntS where
    len_if_le i s = case IntS.safe_len s of
        Just n -> if explain n <= i then Just n else Nothing
        _ -> Nothing
instance OpHasDuplicates IntS where
    type HasDuplicatesExResult IntS = Value_False
    has_duplicates _ = False
instance NoDuplicates IntS






instance OpAnyElem IntS where
    type AnyElemExResult IntS = Maybe Integer
instance OpIterLe IntS
instance Iterable IntS where
    iter = map fst . IntS.unorder_iter







instance OpFull IntS where
    type FullExResult IntS = Bool
    full_ex = IntS.full
    full = full_ex

instance OpDynEmpty IntS where
    empty = IntS.empty
instance OpDynInsert IntS where
    insert i = IntS.insert rng the_only_value where
        rng = IntS.range_singleton i
instance OpDynPopBase IntS where
    type PopsExResult IntS = [(Integer, IntS)]
    type PopsEx_RemainType IntS = Value_EmptySet Integer
    pops_ex = map f . IntS.unorder_pops where
        f ((i, _), s) = (i, s)
instance OpDynPop IntS where
    popls = pops_ex
instance OpDynSingleton IntS where
    singleton i = IntS.singleton (i, the_only_value)
instance OpDynAppend IntS where
    append = (IntS.unionBy const)








instance OpSetDisjoint IntS
instance OpSetSymmetricDifference IntS
instance OpSetDifference IntS where
    difference = (IntS.difference)
instance OpSetAdd IntS where
    set_add = (IntS.unionBy const)
instance OpSetIntersection IntS where
    intersection = (IntS.intersectionBy const)
instance OpSetBiasedIntersection IntS where
    (-/\) = IntS.intersectionBy const
instance OpSetUnion IntS where
    union = IntS.unionBy const
instance OpSetBiasedUnion IntS where
    (-\/) = IntS.unionBy const
instance OpSetSep IntS where
instance OpSetBiasedSep IntS where
instance DynSet IntS where



instance OpDynUniversal IntS where
    universal = IntS.universal the_only_value
instance OpIsUniversal IntS where
    is_universal = full
instance OpDynComplement IntS where
    complement = IntS.complement the_only_value
instance UniversalSet IntS
instance DynUniversalSet IntS












































































--}
--}
--}
