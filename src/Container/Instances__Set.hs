{-# LANGUAGE TypeFamilies #-}

module Container.Instances__Set ()
where

import Container.IContainer
import Container.IStream
import Container.OpIter
--import Container.OpAnyElem
import Container.OpEmpty
import Container.OpLen
import Container.OpPop
import Container.OpInsert
import Container.SetOps
import Container.IDynSet
import Container.ISet
import Container.OpMember
import Container.OpSingleton
import Container.OpDiscard
import Explain.PartialOrd
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

instance Ord a => CountableContainer (Set a) where
instance Ord a => FiniteContainer (Set a) where
instance Ord a => SequenceConcept (Set a)
    -- !!!
instance Ord a => SetConcept (Set a)
instance Ord a => IContainer (Set a) where
    type Element (Set a) = a


--instance Ord a => IStream (Set a) where

instance Ord a => OpIsEmpty (Set a) where
    is_empty = S.null
instance Ord a => OpAnyElem (Set a) where
instance Ord a => OpUnsafeLen (Set a) where
instance Ord a => OpLenIfLe (Set a) where
instance Ord a => OpLen (Set a) where
    len = fromIntegral . S.size
instance Ord a => OpIterLe (Set a) where
instance Ord a => OpIter (Set a) where

instance Ord a => OpPop (Set a) where
    pop = S.minView
instance Ord a => OpInsert (Set a) where
    insert = S.insert
instance Ord a => OpEmpty (Set a) where
    empty = S.empty
instance Ord a => OpIsFull (Set a) where
    is_full _ = False
instance Ord a => OpSingleton (Set a) where
    singleton = S.singleton



-----
--type D_IntSet = IS.IntSet

instance Ord a => OpSetDisjoint (Set a)
instance Ord a => OpSetSymmetricDifference (Set a)
instance Ord a => OpSetDifference (Set a) where
    difference = (S.\\)
instance Ord a => OpSetAdd (Set a) where
    set_add = (S.union)
instance Ord a => OpSetIntersection (Set a) where
    intersection = (S.intersection)
instance Ord a => OpSetBiasedIntersection (Set a) where
    (-/\) = S.intersection
instance Ord a => OpSetUnion (Set a) where
    union = S.union
instance Ord a => OpSetBiasedUnion (Set a) where
    (-\/) = S.union
instance Ord a => OpSetSep (Set a) where
instance Ord a => OpSetBiasedSep (Set a) where



------
instance WithKeyType (Set a) where
    type KeyType (Set a) = a
instance Ord a => OpMember (Set a) where
    member = S.member
instance Eq a => SetEq (Set a) where
    (|==|) = (==)
instance Ord a => SetOrdEx (Set a) where
    type SetCmpExResult (Set a) = PartialOrdering
    set_compare_ex = set_compare
instance Ord a => SetOrd (Set a) where
    (|<|) = S.isProperSubsetOf
    (|<=|) = S.isSubsetOf
instance Ord a => ILinearElementsSetBase (Set a) where
    before _ = compare
        -- undefined behavior if e1 e2 not in a
instance Ord a => IClockwiseElementsSetBase (Set a) where
    clockwise _ = default_clockwise__byOrd
        -- undefined behavior if e1 e2 e3 not in a
{-
instance OpHasDuplicates (Set a) where
    type HasDuplicatesExResult (Set a) = Value_False
    has_duplicates _ = False
instance NoDuplicates (Set a)
-}


instance Ord a => OpDiscard (Set a) where
    discard_le1 = S.delete
    discard_le1_ex e a = r where
        a' = discard_le1 e a
        r = if S.size a' < S.size a then Just a' else Nothing



f :: IDynSet s => s -> ()
f _ = ()
a = f $ S.singleton 'c'

