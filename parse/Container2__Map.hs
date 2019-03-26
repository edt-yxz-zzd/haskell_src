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



module Container2__Map
where

import Container2__base
import Container2__Set
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
import Prelude hiding (lookup)
import Value_Just
import ListGt0
import Language.Haskell.TH
import Language.Haskell.Syntax


type D_Seq = Seq.Seq
type D_Map = M.Map
type D_IntMap = IM.IntMap
--type AMap k v = FiniteList (k, v)
newtype AMap k v = AMap (FiniteList (k, v))
    deriving (Eq, Ord, Read, Show)
newtype MapAsKeySet m = MapAsKeySet m
unMapAsKeySet :: MapAsKeySet m -> m
unMapAsKeySet (MapAsKeySet m) = m
newtype MapAsValues m = MapAsValues m
unMapAsValues :: MapAsValues m -> m
unMapAsValues (MapAsValues m) = m
newtype MapAsItems m = MapAsItems m
unMapAsItems :: MapAsItems m -> m
unMapAsItems (MapAsItems m) = m

-- newtype ListGt0 a = ListGt0 (a, [a])



class   ( OpNull a
        , Optional (AnyElemExResult a)
        , Element a ~ Element (AnyElemExResult a)
        )
    => OpAnyElem a where
    any_elem :: a -> Maybe (Element a)
    type AnyElemExResult a :: *
    any_elem_ex :: a -> AnyElemExResult a
    default any_elem_ex :: OpIterLe a => a -> Maybe (Element a)

    any_elem = unoptional . any_elem_ex
    any_elem_ex = listToMaybe . iter_le 1


class   ( MappingConcept a
        , SetBase (KeySet a), KeyType a ~ Element (KeySet a)
        , ContainerEx (ValueType a) (AllValues a)
        , ContainerEx (ValueType a) (Key2ValuesResult a)
        , Optional (LookUpExResult a)
        , ValueType a ~ Element (LookUpExResult a)
        )
    => MapView a where
    type KeySet a :: *
    type AllValues a :: *
    type Key2ValuesResult a :: *
    type LookUpExResult a :: *
    getKeySet :: a -> KeySet a
    getAllValues :: a -> AllValues a
    key2values :: KeyType a -> a -> Key2ValuesResult a
    lookup_ex :: KeyType a -> a -> LookUpExResult a
    lookup :: KeyType a -> a -> Maybe (ValueType a)
    unsafe_lookup :: KeyType a -> a -> ValueType a
    (!) :: a -> KeyType a -> ValueType a

    lookup k = unoptional . lookup_ex k
    unsafe_lookup k = unjust . lookup k
    (!) = flip unsafe_lookup

class (MapView a, Key2ValuesResult a ~ LookUpExResult a)
    => Map a
class (Map a, Singleton (LookUpExResult a))
    => Mapping a where
    key2value :: KeyType a -> a -> ValueType a
    (@@) :: a -> KeyType a -> ValueType a
    key2value k = unsingleton . lookup_ex k
    (@@) = flip key2value
instance (MapView a, Key2ValuesResult a ~ LookUpExResult a)
    => Map a
instance (Map a, Singleton (LookUpExResult a))
    => Mapping a where



class (OpAnyElem a, OpLenIfLe a) => OpIterLe a where
    -- allow infinite not only countable infinite
    iter_le :: Integer -> a -> [Element a]
        -- iter_le n a = ls ==>> assert len ls <= n && |a| >= len ls
        -- if len ls < n ==>> |a| == len ls
    default iter_le :: Iterable a => Integer -> a -> [Element a]
    iter_le n = take__ls n . iter
class (CountableContainer a, OpIterLe a) => Iterable a where
    -- OpFrom a [Element a]??
    iter :: a -> [Element a]
    default iter :: (F.Foldable f, f (Element a) ~ a) => a -> [Element a]
    iter = F.toList

class (Sized a, Iterable a) => OpIterAsSeq a where
    iterAsSeq :: a -> D_Seq (Element a)
    iterAsSeq = Seq.fromList . iter
class   ( SequenceConcept a, Explain [Element a] a, Iterable a
        , Map a, Natural (KeyType a), ValueType a ~ Element a
        -- , NaturalSet_From0To (KeySet a) -- explain will dead if inf
        -- , TotalSetOrd (KeySet a)
        )
    => Sequence a where
    -- allow countable infinite
    -- [0,?)
    -- [Int i,j] [0 <= i < j][exist seq[j]] ==>> [exist seq[i]]

class   ( Sized a, Sequence a, Explain (D_Seq (Element a)) a
        , OpIterAsSeq a
        , TotalSetOrd (KeySet a)
        --, NaturalSet_From0To (KeySet a)
        )
    => Seq a








------------------------------------
amap2list :: AMap k v -> [(k,v)]
finite_list2amap :: [(k,v)] -> AMap k v
amap2list (AMap (FiniteList ls)) = ls
finite_list2amap ls = AMap (FiniteList ls)
instance Container (AMap k v) where
    type Element (AMap k v) = (k, v)
instance Eq (k,v) => SetConcept (AMap k v)
instance Eq (k,v) => OpMember (AMap k v) where
    type MemberExResult (AMap k v) = Bool
    member_ex k = P.elem k . amap2list
    member = member_ex
instance Eq (k,v) => SetEq (AMap k v) where
    a |==| b = a |<=| b && b |<=| a
instance Eq (k,v) => SetOrd (AMap k v) where
    type SetCmpExResult (AMap k v) = POrd
    AMap (FiniteList a) |<=| AMap (FiniteList b) = a L.\\ b == []


instance CountableContainer (AMap k v)
instance FiniteContainer (AMap k v)
instance SequenceConcept (AMap k v)
instance OpNull (AMap k v) where
    type NullExResult (AMap k v) = Bool
    null_ex = P.null . amap2list
    null = null_ex
instance OpUnsafeLen (AMap k v)
instance OpLenIfLe (AMap k v) where
    len_if_le i = len_if_le i . amap2list
instance Sized (AMap k v) where
    type LenExResult (AMap k v) = UInt
    len_ex = unsafe_len . amap2list
    len = len_ex
instance Eq (k, v) => OpHasDuplicates (AMap k v) where
    type HasDuplicatesExResult (AMap k v) = Bool
    has_duplicates = has_duplicates_ex
    has_duplicates_ex (AMap fs) = has_duplicates_ex fs









----------------------------------------

instance Iterable (Maybe a)
instance Iterable (Value_Just a) where
    iter (Value_Just a) = [a]
instance Iterable (Value_EmptySet a) where
    iter = const []
instance Iterable (D_Set a) where
    iter = S.toAscList
instance Iterable D_IntSet where
    iter = IS.toAscList
instance Iterable (D_Seq a) where
    iter = F.toList
instance Iterable (FiniteList a) where
    iter = unFiniteList
instance Iterable (AMap k v) where
    iter = amap2list
instance Iterable [a] where
    iter = id

instance OpIterLe (Maybe a)
instance OpIterLe (Value_Just a)
instance OpIterLe (Value_EmptySet a)
instance OpIterLe (D_Set a)
instance OpIterLe D_IntSet
instance OpIterLe (D_Seq a)
instance OpIterLe (FiniteList a)
instance OpIterLe (AMap k v)
instance OpIterLe [a]

instance OpIterAsSeq (Maybe a)
instance OpIterAsSeq (Value_Just a)
instance OpIterAsSeq (Value_EmptySet a)
instance OpIterAsSeq (D_Set a)
instance OpIterAsSeq D_IntSet
instance OpIterAsSeq (D_Seq a) where
    iterAsSeq = id
instance OpIterAsSeq (FiniteList a)
instance OpIterAsSeq (AMap k v)

instance OpAnyElem (Maybe a) where
    type AnyElemExResult (Maybe a) = Maybe a
    any_elem_ex = id
    any_elem = any_elem_ex
instance OpAnyElem [a] where
    type AnyElemExResult [a] = Maybe a
    any_elem_ex = listToMaybe
    any_elem = any_elem_ex
instance OpAnyElem (Value_Just a) where
    type AnyElemExResult (Value_Just a) = Value_Just a
    any_elem_ex = id
    -- any_elem (Value_Just a) = Just a
    any_elem = unoptional
instance OpAnyElem (Value_EmptySet a) where
    type AnyElemExResult (Value_EmptySet a) = Value_EmptySet a
    any_elem_ex = id
    any_elem = unoptional
instance OpAnyElem (D_Set a) where
    type AnyElemExResult (D_Set a) = Maybe a
    any_elem_ex = fmap fst . S.minView
    any_elem = any_elem_ex
instance OpAnyElem D_IntSet where
    type AnyElemExResult D_IntSet = Maybe Int
    any_elem_ex = fmap fst . IS.minView
    any_elem = any_elem_ex
instance OpAnyElem (D_Seq a) where
    type AnyElemExResult (D_Seq a) = Maybe a
    any_elem_ex = any_elem . F.toList
    any_elem = any_elem_ex
instance OpAnyElem (FiniteList a) where
    type AnyElemExResult (FiniteList a) = Maybe a
    any_elem_ex = any_elem . unFiniteList
    any_elem = any_elem_ex
instance OpAnyElem (AMap k v) where
    type AnyElemExResult (AMap k v) = Maybe (k,v)
    any_elem_ex = any_elem . amap2list
    any_elem = any_elem_ex


{-
instance (OpDynZero a, Natural a) => OpAnyElem (UIntSet_Lt a) where
    type AnyElemExResult (UIntSet_Lt a) = Maybe a
    any_elem_ex a = if null a then Nothing else Just zero
    any_elem = any_elem_ex
-}















instance MappingConcept m => Container (MapAsValues m) where
    type Element (MapAsValues m) = ValueType m
instance MappingConcept m => Container (MapAsItems m) where
    type Element (MapAsItems m) = (KeyType m, ValueType m)
-------------------------------

instance MappingConcept m => Container (MapAsKeySet m) where
    type Element (MapAsKeySet m) = KeyType m
instance MappingConcept m => SetConcept (MapAsKeySet m)
instance MapView m => OpMember (MapAsKeySet m) where
    type MemberExResult (MapAsKeySet m) = Bool
    member_ex k = maybe False (const True) . lookup k . unMapAsKeySet
    member = member_ex
instance Ord k => SetEq (MapAsKeySet (D_Map k v)) where
    a@(MapAsKeySet a') |==| b@(MapAsKeySet b')
        = M.size a' == M.size b' && a |<=| b
        -- && not (a |<| b) -- b |<=| a

_True2 :: a -> b -> Bool
_True2 _ _ = True
instance Ord k => SetOrd (MapAsKeySet (D_Map k v)) where
    type SetCmpExResult (MapAsKeySet (D_Map k v)) = POrd
    (|<|) = lift2 unMapAsKeySet $ M.isProperSubmapOfBy _True2
    (|<=|) = lift2 unMapAsKeySet $ M.isSubmapOfBy _True2
instance Ord k => LinearElementsSetBase (MapAsKeySet (D_Map k v)) where
    before _ = compare
        -- undefined behavior if e1 e2 not in a
instance Ord k => ClockwiseElementsSetBase (MapAsKeySet (D_Map k v)) where
    clockwise _ = default_clockwise__byOrd
        -- undefined behavior if e1 e2 e3 not in a

instance Ord k => CountableContainer (MapAsKeySet (D_Map k v))
instance Ord k => FiniteContainer (MapAsKeySet (D_Map k v))
instance Ord k => SequenceConcept (MapAsKeySet (D_Map k v))
instance Ord k => OpNull (MapAsKeySet (D_Map k v)) where
    type NullExResult (MapAsKeySet (D_Map k v)) = Bool
    null_ex = M.null . unMapAsKeySet
    null = null_ex
instance Ord k => OpUnsafeLen (MapAsKeySet (D_Map k v))
instance Ord k => OpLenIfLe (MapAsKeySet (D_Map k v))
instance Ord k => Sized (MapAsKeySet (D_Map k v)) where
    type LenExResult (MapAsKeySet (D_Map k v)) = UInt
    len_ex = unsafe_from . P.toInteger . M.size . unMapAsKeySet
    len = len_ex
instance Ord k => OpHasDuplicates (MapAsKeySet (D_Map k v)) where
    type HasDuplicatesExResult (MapAsKeySet (D_Map k v)) = Value_False
    has_duplicates _ = False
instance Ord k => NoDuplicates (MapAsKeySet (D_Map k v))




----------------------------------------
instance MappingConcept (D_Map k v) where
    type KeyType (D_Map k v) = k
    type ValueType (D_Map k v) = v
instance Ord k => MapView (D_Map k v) where
    type KeySet (D_Map k v) = MapAsKeySet (D_Map k v)
    type AllValues (D_Map k v) = [v]
    type Key2ValuesResult (D_Map k v) = Maybe v
    type LookUpExResult (D_Map k v) = Maybe v
    getKeySet = MapAsKeySet
    getAllValues = M.elems
    key2values = M.lookup
    lookup_ex = key2values
    lookup = lookup_ex




instance MappingConcept (D_IntMap v) where
    type KeyType (D_IntMap v) = Int
    type ValueType (D_IntMap v) = v
instance MapView (D_IntMap v) where
    type KeySet (D_IntMap v) = MapAsKeySet (D_IntMap v)
    type AllValues (D_IntMap v) = [v]
    type Key2ValuesResult (D_IntMap v) = Maybe v
    type LookUpExResult (D_IntMap v) = Maybe v
    getKeySet = MapAsKeySet
    getAllValues = IM.elems
    key2values = IM.lookup
    lookup_ex = key2values
    lookup = lookup_ex











-- not Seq (FiniteList a) if type AMap k v = FiniteList (k,v)
-- now "newtype AMap", AMap is not Seq
instance MappingConcept (AMap k v) where
    type KeyType (AMap k v) = k
    type ValueType (AMap k v) = v
instance Eq k => MapView (AMap k v) where
    type KeySet (AMap k v) = MapAsKeySet (AMap k v)
    type AllValues (AMap k v) = FiniteList v
    type Key2ValuesResult (AMap k v) = FiniteList v
    type LookUpExResult (AMap k v) = Maybe v
    getKeySet = MapAsKeySet
    getAllValues = FiniteList . map snd . iter
    key2values k = FiniteList . map snd . filter ((k==) . fst) . iter
    lookup_ex k = P.lookup k . iter
    lookup = lookup_ex


instance MappingConcept [a] where
    type KeyType [a] = UInt
    type ValueType [a] = a
instance MapView [a] where
    type KeySet [a] = MapAsKeySet [a]
    type AllValues [a] = [a]
    type Key2ValuesResult [a] = Maybe a
    type LookUpExResult [a] = Maybe a
    getKeySet = MapAsKeySet
    getAllValues = id
    key2values i ls = if len_le (P.toInteger i) ls then Nothing
                      else Just $ ls L.!! i'
        where i' = P.fromIntegral i
    lookup_ex = key2values
    lookup = lookup_ex

instance SequenceConcept [a]
instance Sequence [a]


instance Container (D_Seq a) where
    type Element (D_Seq a) = a

instance CountableContainer (D_Seq a)
instance FiniteContainer (D_Seq a)
instance OpNull (D_Seq a) where
    type NullExResult (D_Seq a) = Bool
    null_ex = Seq.null
    null = null_ex
instance OpUnsafeLen (D_Seq a)
instance OpLenIfLe (D_Seq a)
instance Sized (D_Seq a) where
    type LenExResult (D_Seq a) = UInt
    len_ex = unsafe_from . P.toInteger . Seq.length
    len = len_ex
--instance SetConcept (D_Seq a)
--instance OpHasDuplicates (D_Seq a) where
--instance NoDuplicates (D_Seq a)
instance SequenceConcept (D_Seq a)
instance Eq a => OpMember (D_Seq a) where
    type MemberExResult (D_Seq a) = Bool
    member_ex e a = Seq.elemIndexL e a /= Nothing

let a = varT $ mkName "a"
    _List_a = [t| [] $a |]
    _List_Int = [t| [Int] |]
    _D_Seq_a = [t| D_Seq $a |]
    _D_Seq_Int = [t| D_Seq Int |]

    _Maybe_a = [t| Maybe $a |]
    _Value_Just_a = [t| Value_Just $a |]
    _Value_EmptySet_a = [t| Value_EmptySet $a |]
    _D_Set_a = [t| D_Set $a |]
    _D_IntSet = [t| D_IntSet |]
    _D_FL_a = [t| FiniteList $a |]
    types = [ _Maybe_a, _Value_Just_a, _Value_EmptySet_a, _D_FL_a
            , _D_Set_a] -- , _D_IntSet_a]
    expr__ls = [e|iter|]
    expr__seq = [e|iterAsSeq|]
    args__ls =
            (_List_Int, _D_IntSet, expr__ls) :
            map (\t->(_List_a, t, expr__ls)) (_D_Seq_a : types) ++
            (_D_Seq_Int, _D_IntSet, expr__seq) :
            map (\t->(_D_Seq_a, t, expr__seq)) types
    decssQ__ls = mapM (\(a,b,c)->instExplain a b c) args__ls
    decsQ__ls = fmap concat decssQ__ls
 in decsQ__ls


instance MappingConcept (D_Seq v) where
    type KeyType (D_Seq v) = UInt
    type ValueType (D_Seq v) = v
instance MapView (D_Seq v) where
    type KeySet (D_Seq v) = Value_FiniteNaturalSet_Lt UInt
    type AllValues (D_Seq v) = D_Seq v
    type Key2ValuesResult (D_Seq v) = Maybe v
    type LookUpExResult (D_Seq v) = Maybe v
    getKeySet = Value_FiniteNaturalSet_Lt . len
    getAllValues = id
    key2values i a = if i < len a
        then Just . Seq.index a $ P.fromIntegral i else Nothing
    lookup_ex = key2values
    lookup = lookup_ex



--instance SequenceConcept (D_Seq a)
instance Sequence (D_Seq a)
instance Seq (D_Seq a)



instance MappingConcept (FiniteList v) where
    type KeyType (FiniteList v) = UInt
    type ValueType (FiniteList v) = v
instance MapView (FiniteList v) where
    type KeySet (FiniteList v) = Value_FiniteNaturalSet_Lt UInt
    type AllValues (FiniteList v) = FiniteList v
    type Key2ValuesResult (FiniteList v) = Maybe v
    type LookUpExResult (FiniteList v) = Maybe v
    getKeySet = Value_FiniteNaturalSet_Lt . len
    getAllValues = id
    key2values i (FiniteList ls) = if len_le (explain i) ls
        then Nothing else Just $ ls P.!! P.fromIntegral i
    lookup_ex = key2values
    lookup = lookup_ex



--instance SequenceConcept (FiniteList a)
instance Sequence (FiniteList a)
instance Seq (FiniteList a)





{- no S.at??
instance MappingConcept (D_Set v) where
    type KeyType (D_Set v) = UInt
    type ValueType (D_Set v) = v
instance MapView (D_Set v) where
    type KeySet (D_Set v) = Value_FiniteNaturalSet_Lt UInt
    type AllValues (D_Set v) = D_Set v
    type Key2ValuesResult (D_Set v) = Maybe v
    type LookUpExResult (D_Set v) = Maybe v
    getKeySet = Value_FiniteNaturalSet_Lt . len
    getAllValues = id
    key2values i a = if i < len a
        then Just . S.at a $ P.fromIntegral i else Nothing
    lookup_ex = key2values
    lookup = lookup_ex
--instance SequenceConcept (D_Set a)
instance Sequence (D_Set a)
instance Seq (D_Set a)
-}


instance MappingConcept (Maybe v) where
    type KeyType (Maybe v) = Value_ZeroOrOne
    type ValueType (Maybe v) = v
instance MapView (Maybe v) where
    type KeySet (Maybe v) = Value_FiniteNaturalSet_Lt Value_ZeroOrOne
    type AllValues (Maybe v) = Maybe v
    type Key2ValuesResult (Maybe v) = Maybe v
    type LookUpExResult (Maybe v) = Maybe v
    getKeySet = Value_FiniteNaturalSet_Lt . len_ex
    getAllValues = id
    key2values (Value_ZeroOrOne False) = id
    key2values _ = const Nothing
    lookup_ex = key2values
    lookup = lookup_ex
--instance SequenceConcept (Maybe a)
instance Sequence (Maybe a)
instance Seq (Maybe a)





instance MappingConcept (Value_EmptySet v) where
    type KeyType (Value_EmptySet v) = Value_Zero
    type ValueType (Value_EmptySet v) = v
instance MapView (Value_EmptySet v) where
    type KeySet (Value_EmptySet v) = Value_FiniteNaturalSet_Lt Value_Zero
    type AllValues (Value_EmptySet v) = Value_EmptySet v
    type Key2ValuesResult (Value_EmptySet v) = Value_EmptySet v
    type LookUpExResult (Value_EmptySet v) = Value_EmptySet v
    getKeySet = Value_FiniteNaturalSet_Lt . len_ex
    getAllValues = id
    key2values _ = id
    lookup_ex = key2values
    lookup _ = const Nothing
--instance SequenceConcept (Value_EmptySet a)
instance Sequence (Value_EmptySet a)
instance Seq (Value_EmptySet a)






instance MappingConcept (Value_Just v) where
    type KeyType (Value_Just v) = Value_Zero
    type ValueType (Value_Just v) = v
instance MapView (Value_Just v) where
    type KeySet (Value_Just v) = Value_FiniteNaturalSet_Lt Value_Zero
    type AllValues (Value_Just v) = Value_Just v
    type Key2ValuesResult (Value_Just v) = Value_Just v
    type LookUpExResult (Value_Just v) = Value_Just v
    getKeySet _ = Value_FiniteNaturalSet_Lt the_only_value
    getAllValues = id
    key2values _ = id
    lookup_ex = key2values
    lookup _ = unoptional
--instance SequenceConcept (Value_Just a)
instance Sequence (Value_Just a)
instance Seq (Value_Just a)











-------------------------
instance Container (ListGt0 a) where
    type Element (ListGt0 a) = a
instance CountableContainer (ListGt0 a)
instance OpNull (ListGt0 a) where
    type NullExResult (ListGt0 a) = Value_False
    null_ex _ = the_only_value
    null _ = False
instance OpUnsafeLen (ListGt0 a) where
    unsafe_len = unsafe_len . nonnull2list
instance OpLenIfLe (ListGt0 a) where
    len_if_le i = len_if_le i . nonnull2list
instance SequenceConcept (ListGt0 a)


instance OpAnyElem (ListGt0 a) where
    type AnyElemExResult (ListGt0 a) = Value_Just a
    any_elem_ex = Value_Just . nn_head
    any_elem = Just . nn_head

instance MappingConcept (ListGt0 a) where
    type KeyType (ListGt0 a) = PInt
    type ValueType (ListGt0 a) = a
instance MapView (ListGt0 a) where
    type KeySet (ListGt0 a) = MapAsKeySet (ListGt0 a)
    type AllValues (ListGt0 a) = (ListGt0 a)
    type Key2ValuesResult (ListGt0 a) = Maybe a
    type LookUpExResult (ListGt0 a) = Maybe a
    getKeySet = MapAsKeySet
    getAllValues = id
    key2values i = key2values (explain i) . nonnull2list
    lookup_ex = key2values
    lookup = lookup_ex

instance OpIterLe (ListGt0 a)
instance Iterable (ListGt0 a) where
    iter = nonnull2list
let a = varT $ mkName "a" in
    instExplain [t|[$a]|] [t|ListGt0 $a|] [e|iter|]
--instance SequenceConcept (ListGt0 a)
instance Sequence (ListGt0 a)





