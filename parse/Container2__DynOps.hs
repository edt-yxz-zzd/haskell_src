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



module Container2__DynOps
where

import Container2__base
import Container2__Set
import Container2__Map
import Container2__Buffer
import Container2__DynSet
import Ordering
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Sequence as Seq
import Data.Maybe
import qualified Data.Foldable as F -- (toList, Foldable, foldMap, foldr)
import Data.Foldable (Foldable)
import Data.Monoid
import Data.Array.IArray as A

import ExplainEx
import OpDynTheOnlyValue
import XInt
import SeedUtils (unjust, lift2, take__ls, merge_lsls, merge_finite_lsls)
import qualified Prelude as P
import Prelude hiding (lookup, null, foldr, foldl)
import Value_Just
import ListGt0
import Language.Haskell.TH
import Language.Haskell.Syntax
import SeedUtils__TH (def__instances)
-- import qualified IntegerSet__FiniteRange as IntS
-- import IntegerSet__FiniteRange (IntS)





class OpDynMergeEx a b a => OpDynMerge b a where
    merge :: a -> (Element a -> b) -> a
    merge = merge_ex
class OpDynMerge a a => OpDynMergeS a where
    mergeS :: a -> (Element a -> a) -> a
    mergeS = merge
instance OpDynMergeEx a b a => OpDynMerge b a where
instance OpDynMerge a a => OpDynMergeS a where
class   ( Container a, Container b, Container c
        , Element b ~ Element c
        )
    => OpDynMergeEx a b c where
    -- merge_ex singleton v.s. (>>=) return
    -- merge_ex :: a -> (e -> Value_Just e) -> a ==>> merge_ex == id
    -- merge_ex a singleton == a
    merge_ex :: a -> (Element a -> b) -> c
    -- [a] -- should not use 'concat'
    -- FiniteList a -- concat
    default merge_ex :: (Monad m, a ~ m ea, b ~ m eb, c ~ b)
        => m ea -> (ea -> m eb) -> m eb
    merge_ex = (>>=)
{-
instance Monad m => Container (m a) where
    type Element (m a) = a
instance Monad m => OpDynMergeEx (m a) (m b) (m c) where
    merge_ex = (>>=)
-}


instance (Container a, ContainerEx e c, OpDynEmpty c)
    => OpDynMergeEx a (Value_EmptySet e) c where
    merge_ex _ _ = empty
instance (ContainerEx e b, ContainerEx e c, OpDynEmpty c)
    => OpDynMergeEx (Value_EmptySet e) b c where
    merge_ex _ _ = empty
instance OpDynMergeEx (Maybe a) (Maybe b) (Maybe b) where
    merge_ex = (>>=)
instance OpDynMergeEx ([] a) ([] b) ([] b) where
    merge_ex ls f = merge_lsls $ fmap f ls
instance OpDynMergeEx ([] a) (FiniteList b) ([] b) where
    merge_ex ls f = P.concat . map unFiniteList $ fmap f ls
instance OpDynMergeEx (FiniteList a) (FiniteList b) (FiniteList b) where
    merge_ex = (>>=)
instance OpDynMergeEx (FiniteList a) (FiniteList b) ([] b) where
    merge_ex ls = unFiniteList . merge_ex ls
instance OpDynMergeEx (FiniteList a) ([] b) ([] b) where
    merge_ex ls f = merge_finite_lsls . fmap f $ unFiniteList ls



{- Ord e!!!
instance Monad D_Set where
    a >>= f = S.fromList $ iter a >>= iter . f
    return = singleton
-}

instance Ord b => OpDynMergeEx (D_Set a) (D_Set b) (D_Set b) where
    merge_ex s f = S.fromList $ iter s >>= iter . f
instance Ord b => OpDynMergeEx (D_Set a) ([] b) (D_Set b) where
    merge_ex s f = S.fromList $ iter s >>= f
instance OpDynMergeEx (D_Set a) ([] b) ([] b) where
    merge_ex s f = iter s >>= f

instance OpSafeFrom [a] (ListGt0 a) where
    safe_from (a:ls) = Just $ ListGt0 (a, ls)
    safe_from _ = Nothing
    unsafe_from = unsafe_list2nonnull
instance OpDynMergeEx (ListGt0 a) (ListGt0 b) (ListGt0 b) where
    merge_ex ls f = unsafe_from $ (merge_ex (iter ls) (iter . f) :: [b])
{-
instance OpDynMergeEx (Maybe a) (Maybe b) (Maybe b) where
    merge_ex = (>>=)

--}







-------------------------------------
class Container a => OpFold a where
    fold :: Monoid (Element a) => a -> Element a
    default fold :: (OpFoldMap a, Monoid (Element a)) => a -> Element a
    fold = foldMap id
class OpFold a => OpFoldMap a where
    foldMap :: Monoid x => (Element a -> x) -> a -> x
    default foldMap :: (F.Foldable f, Monoid x, a ~ f e)
        => (e -> x) -> f e -> x
    foldMap = F.foldMap
class OpFoldMap a => OpFoldR a where
    foldr :: (Element a -> r -> r) -> r -> a -> r
    foldr1 :: (Element a ~ r) => (r -> r -> r) -> a -> r
    default foldr :: (F.Foldable f, a ~ f e)
        => (e -> r -> r) -> r -> f e -> r
    default foldr1 :: (F.Foldable f, a ~ f e, r ~ e)
        => (e -> r -> r) -> f e -> r
    foldr = F.foldr
    foldr1 = F.foldr1
class OpFoldMap a => OpFoldL a where
    foldl :: (l -> Element a -> l) -> l -> a -> l
    foldl1 :: (Element a ~ l) => (l -> l -> l) -> a -> l
    default foldl :: (F.Foldable f, a ~ f e)
        => (l -> e -> l) -> l -> f e -> l
    default foldl1 :: (F.Foldable f, a ~ f e, l ~ e)
        => (l -> e -> l) -> f e -> l
    foldl = F.foldl
    foldl1 = F.foldl1

instance Foldable FiniteList where
    fold = F.fold . iter
    foldMap f = F.foldMap f . iter
    foldr f r = F.foldr f r . iter
    foldl f l = F.foldl f l . iter
    foldr1 f = F.foldr1 f . iter
    foldl1 f = F.foldl1 f . iter

{-
instance Foldable ListGt0 where
    fold = F.fold . iter
    foldMap f = F.foldMap f . iter
    foldr f r = F.foldr f r . iter
    foldl f l = F.foldl f l . iter
    foldr1 f = F.foldr1 f . iter
    foldl1 f = F.foldl1 f . iter
-}
let
    -- k = varT $ mkName "k"
    -- _D_Map_k = [t| D_Map $k |] -- MapAsValues D_Map!!
    -- types_ = _D_Map_k : map conT con_names_
    types_ = map conT con_names_
    con_names_ =
        [ ''[], ''Maybe, ''D_Set, ''D_Seq
        , ''FiniteList, ''ListGt0]
    type_2decsQ t = [d|
        -- Foldable 2 OpFoldMap/R/L
        instance Foldable $t => OpFold ($t a)
        instance Foldable $t => OpFoldMap ($t a)
        instance Foldable $t => OpFoldR ($t a)
        instance Foldable $t => OpFoldL ($t a)
        |]
 in fmap P.concat $ mapM type_2decsQ types_

instance OpFold (MapAsValues (D_Map k v)) where
instance OpFoldMap (MapAsValues (D_Map k v)) where
    foldMap f = F.foldMap f . unMapAsValues
instance OpFoldR (MapAsValues (D_Map k v)) where
    foldr f x = F.foldr f x . unMapAsValues
    foldr1 f = F.foldr1 f . unMapAsValues
instance OpFoldL (MapAsValues (D_Map k v)) where
    foldl f x = F.foldl f x . unMapAsValues
    foldl1 f = F.foldl1 f . unMapAsValues



{-
    foldMap :: Monoid x => (Element a -> x) -> a -> x
    foldr :: (Element a -> r -> r) -> r -> a -> r
    foldr1 :: (Element a ~ r) => (r -> r -> r) -> a -> r
-}
foldr2foldMap :: (Monoid x, r~x)
    => ((e->r->r)->r->a->r) -> ((e->x) -> a -> x)
foldr2foldMap foldr f = foldr (mappend . f) mempty
foldr2foldr1 :: (m~Maybe r)
    => ((r->m->m)->m->a->m) -> ((r->r->r) -> a -> r)
foldr2foldr1 foldr f xs =
    fromMaybe (error "foldr1: empty structure") (foldr mf Nothing xs)
  where
    mf x (Just y) = Just (f x y)
    mf x Nothing = Just x
foldl2foldl1 :: (m~Maybe l)
    => ((m->l->m)->m->a->m) -> ((l->l->l) -> a -> l)
foldl2foldl1 foldl f xs =
    fromMaybe (error "foldl1: empty structure") (foldl mf Nothing xs)
  where
    mf (Just y) x = Just (f y x)
    mf Nothing x = Just x
foldMap__via_foldr :: (Monoid x, e~Element a, OpFoldR a)
    => ((e->x) -> a -> x)
foldMap__via_foldr = foldr2foldMap foldr
foldr1__via_foldr :: (OpFoldR a, r~Element a) => ((r->r->r) -> a -> r)
foldr1__via_foldr = foldr2foldr1 foldr
foldl1__via_foldl :: (OpFoldL a, l~Element a) => ((l->l->l) -> a -> l)
foldl1__via_foldl = foldl2foldl1 foldl
foldl__via_foldMap :: (OpFoldMap a, e~Element a)
    => (l -> e -> l) -> l -> a -> l
foldl__via_foldMap f z t =
    appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z

instance OpFold (MapAsValues (D_IntMap v)) where
instance OpFoldMap (MapAsValues (D_IntMap v)) where
    foldMap = foldMap__via_foldr
instance OpFoldR (MapAsValues (D_IntMap v)) where
    foldr f x = IM.foldr f x . unMapAsValues
    foldr1 = foldr1__via_foldr
instance OpFoldL (MapAsValues (D_IntMap v)) where
    foldl f x = IM.foldl f x . unMapAsValues
    foldl1 = foldl1__via_foldl


instance OpFold D_IntSet where
instance OpFoldMap D_IntSet where
    foldMap = foldMap__via_foldr
instance OpFoldR D_IntSet where
    foldr = IS.foldr
    foldr1 = foldr1__via_foldr
instance OpFoldL D_IntSet where
    foldl = IS.foldl
    foldl1 = foldl1__via_foldl








type D_Array = A.Array
instance Container (D_Array i e) where
    type Element (D_Array i e) = e
instance Ix i => OpFold (D_Array i e)
instance Ix i => OpFoldMap (D_Array i e)
instance Ix i => OpFoldR (D_Array i e)
instance Ix i => OpFoldL (D_Array i e)























---------------------------------
class (Container a, Container b) => OpDynMapC a b where
    mapC :: (Element a -> Element b) -> a -> b
let
    fnames = [''[], ''Maybe, ''D_Seq, ''FiniteList, ''ListGt0]
    ftypes = map conT fnames
    ftype2decsQ f = [d|
        instance OpDynMapC ($f a) ($f b) where
            mapC = fmap
        |]
 in fmap concat $ mapM ftype2decsQ ftypes
{-
instance Eq k => OpDynMapC (D_Map k a) (D_Map k b) where
    mapC f = M.mapWithKey g where
        g k a = let (k', b) = f (k, a) in if k == k' then b else
            error "k /= k' @ mapC (Map k a) (Map k b)"
-}

instance OpDynMapC (MapAsValues (D_Map k a)) (MapAsValues (D_Map k b)) where
    mapC f = MapAsValues . fmap f . unMapAsValues
instance Ord k2 => OpDynMapC (MapAsKeySet (D_Map k1 a))
                             (MapAsKeySet (D_Map k2 a)) where
    mapC f = MapAsKeySet . M.mapKeys f . unMapAsKeySet




instance OpDynMapC (MapAsValues (D_IntMap a)) (MapAsValues (D_IntMap b)) where
    mapC f = MapAsValues . fmap f . unMapAsValues
instance OpDynMapC  (MapAsKeySet (D_IntMap a))
                    (MapAsKeySet (D_IntMap a)) where
    mapC f = MapAsKeySet .
        -- M.mapKeys has a bug
        -- IM.fromList . map (\(k,v)->(f k,v)) . P.reverse . IM.toAscList
        IM.fromList . map (\(k,v)->(f k,v)) . IM.toAscList
        . unMapAsKeySet
instance Ord b => OpDynMapC (D_Set a) (D_Set b) where
    -- mapC = S.map
    mapC f = S.fromList . map f . iter
instance OpDynMapC D_IntSet D_IntSet where
    mapC = IS.map






































































{-
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
