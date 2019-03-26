{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}


module Container2__Set
where

import Container2__base
import Ordering
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.List as L

import ExplainEx as E
import OpDynTheOnlyValue
import XInt
import SeedUtils (unjust)
import Prelude as P
import Value_Just
import Language.Haskell.TH
import Language.Haskell.Syntax


-- should be finite long, though not check...
newtype FiniteList a = FiniteList [a]
    deriving (Eq, Ord, Read, Show, Monad, Functor)
unFiniteList :: FiniteList a -> [a]
unFiniteList (FiniteList ls) = ls



{- but [a] can be infinite long!
instance Eq a => SetConcept [a]
instance Eq a => SetEq [a] where
    a |==| b = a |<=| b && b |<=| a
instance SetOrd [a]-}

class (OpMember a, SetConcept a) => SetBase a where
    -- no Eq a, it should present in instance decl
    -- Eq a in not necessary for OpMember
    --      e.g. EmptySet or Set of one value type
    -- allow duplicates; but they donot affect |==|
    (|<-|) :: Element a -> a -> Bool -- member
    (|<-|) = member -- in
class (SetBase a, SetOrd a) => Set a
class (Set a, TotalSetOrd a) => TotalOrdSet a
    -- cannot support SetDiff in general
    -- e.g. KeySetType of Seq : {{}, {0}, {0,1}, ...}
class (Set a, LinearElementsSetBase a) => LinearElementsSet a
class (SetBase a, Eq (Element a)) => LinearElementsSetBase a where
    -- TotalOrdSet : set are ordered
    -- LinearElementsSet : elements per set are ordered
    --                      but diff sets may diff order
    --                      used as topological sort result
    --                      no (Ord a)
    -- preconditions: e in a
    before :: a -> Element a -> Element a -> P.Ordering
class (Set a, ClockwiseElementsSetBase a) => ClockwiseElementsSet a
class (SetBase a, Eq (Element a)) => ClockwiseElementsSetBase a where
    -- elements per set are ordered in a cycle
    -- but diff sets may diff order
    -- used as value type of planar embedding
    -- no (Ord a)
    clockwise :: a -> Element a -> Element a -> Element a -> P.Ordering
        -- LT - clockwise
        -- EQ - |{a,b,c}| < 3 - both clockwise and anti-clockwise
        -- GT - anti-clockwise

    default clockwise :: LinearElementsSet a
        => a -> Element a -> Element a -> Element a -> P.Ordering
    clockwise = default_clockwise__byOrd_ex . before
    {-
    clockwise c e1 e2 e3 =
        -- undefined behavior if e1 e2 e3 not in a
        if e1 == e2 || e2 == e3 || e3 == e1 then P.EQ else
        if e1 < e2 then if e3 < e1 || e2 < e3 then P.LT else P.GT else
        if e1 > e2 then if e3 > e1 || e2 > e3 then P.GT else P.LT else
        error "logic error @clockwise"
      where
        e1 == e2 = before c e1 e2 P.== P.EQ
        e1 < e2 = before c e1 e2 P.== P.LT
        e1 > e2 = before c e1 e2 P.== P.GT
        infix 4 ==, <, >
    -}

instance (OpMember a, SetConcept a) => SetBase a
instance (SetBase a, SetOrd a) => Set a
instance (Set a, TotalSetOrd a) => TotalOrdSet a
instance (Set a, LinearElementsSetBase a) => LinearElementsSet a
instance (Set a, ClockwiseElementsSetBase a) => ClockwiseElementsSet a


default_clockwise__byOrd_ex
    :: (a -> a -> P.Ordering) -> (a -> a -> a -> P.Ordering)
default_clockwise__byOrd_ex compare e1 e2 e3 =
        -- undefined behavior if e1 e2 e3 not in a
        if e1 == e2 || e2 == e3 || e3 == e1 then P.EQ else
        if e1 < e2 then if e3 < e1 || e2 < e3 then P.LT else P.GT else
        if e1 > e2 then if e3 > e1 || e2 > e3 then P.GT else P.LT else
        error "logic error @default_clockwise__byOrd"
      where
        e1 == e2 = compare e1 e2 P.== P.EQ
        e1 < e2 = compare e1 e2 P.== P.LT
        e1 > e2 = compare e1 e2 P.== P.GT
        infix 4 ==, <, >
default_clockwise__byOrd :: Ord a => a -> a -> a -> P.Ordering
default_clockwise__byOrd = default_clockwise__byOrd_ex compare
















data Value_NaturalSet_Lt a = Value_NaturalSet_Lt a -- {x | 0 <= x < a}
    deriving (Eq, Ord, Read, Show)
data Value_FiniteNaturalSet_Lt a = Value_FiniteNaturalSet_Lt a
    deriving (Eq, Ord, Read, Show)
newtype MaybeInf a = MaybeInf (Maybe a) -- Nothing is Inf
    deriving (Eq, Read, Show, Monad, Functor)
instance Ord a => Ord (MaybeInf a) where
    MaybeInf (Just a) < MaybeInf (Just b) = a < b
    MaybeInf Nothing < _ = False
    _ < _ = True
    MaybeInf (Just a) `compare` MaybeInf (Just b) = a `compare` b
    MaybeInf Nothing `compare` MaybeInf Nothing = P.EQ
    MaybeInf Nothing `compare` _ = P.GT
    _ `compare` _ = P.LT


{-
data LeftBound a = LeftExcludeNegOO | LeftInclude a | LeftExclude a
data RightBound a = RightExclude a | RightInclude a | RightExcludePosOO
data Value_LeftInclude_0
data Value_RangeSet elem left_bound right_bound
    = Value_RangeSet left_bound right_bound

instOpDynTheOnlyValue [t| Value_LeftInclude_0 |]





class   ( Ord a, Explain (LeftBound a) lb
        , OrdCmpResult (LeftBoundCmpExResult lb)
        )
    => OpLeftBoundCmp a lb | lb -> a where
    type LeftBoundCmpExResult lb :: *
    left_bound_cmp_ex :: lb -> a -> LeftBoundCmpExResult lb
    left_bound_cmp :: lb -> a -> P.Ordering
    left_bound_cmp lb = explain . left_bound_cmp_ex lb
    default left_bound_cmp_ex :: lb -> a -> P.Ordering -- no EQ??
    left_bound_cmp_ex lb a = f (explain lb) a where
        f (LeftExcludeNegOO) a = P.LT
        f (LeftInclude b) a = -- compare b a
            if b <= a then P.LT else P.GT
        f (LeftExclude b) a = -- case compare b a of
            if b < a then P.LT else P.GT
class   ( Ord a, Explain (RightBound a) rb
        , OrdCmpResult (RightBoundCmpExResult rb)
        )
    => OpRightBoundCmp a rb | rb -> a where
    type RightBoundCmpExResult rb :: *
    right_bound_cmp_ex :: rb -> a -> RightBoundCmpExResult rb
    right_bound_cmp :: rb -> a -> P.Ordering
    right_bound_cmp rb = explain . right_bound_cmp_ex rb
    default right_bound_cmp_ex :: rb -> a -> P.Ordering
    right_bound_cmp_ex rb a = f (explain rb) a where
        f (RightExcludePosOO) a = P.GT
        f (RightInclude b) a = -- compare b a
            if a <= b then P.GT else P.LT
        f (RightExclude b) a =
            if a < b then P.GT else P.LT
class   ( OpLeftBoundCmp a lb, OpRightBoundCmp a rb
        , OrdCmpResult (LeftRightBoundCmpExResult lb rb)
        )
    => OpLeftRightBoundCmp a lb rb where
    -- [1, 0), [1,0] - LT
    -- [1, 1), (1,1] - EQ
    -- [1, 2), [1,1] - GT -- {1} not null
    type LeftRightBoundCmpExResult lb rb :: *
    left_right_bound_cmp_ex
        :: lb -> rb -> LeftRightBoundCmpExResult lb rb
    left_right_bound_cmp :: lb -> rb -> P.Ordering
    left_right_bound_cmp lb = explain . left_right_bound_cmp_ex lb
class   ( Set c, Ord (Element c)
        , Explain (LeftBound (Element c)) (LeftBoundType c)
        , Explain (RightBound (Element c)) (RightBoundType c)
        , Explain
            (Value_RangeSet (Element c)
                            (LeftBoundType c) (RightBoundType c))
            c
        , OpLeftBoundCmp (Element c) (LeftBoundType c)
        , OpRightBoundCmp (Element c) (RightBoundType c)
        , OpNull c
        -- (1,2) is null if [Integral a] and not null if [Real a]
        -- but [Integral a] ==>> [Real a]
        -- so, whether null is a property of THIS set
        -- sometimes, we may have a limit set of values to represent
        --      invisible values between them
        ) => RangeSet c where
    type LeftBoundType c :: *
    type RightBoundType c :: *
    left_bound :: c -> LeftBoundType c
    right_bound :: c -> RightBoundType c
    bounds :: c -> (LeftBoundType c, RightBoundType c)

    left_bound = fst . bounds
    right_bound = snd . bounds
    bounds c =
      let Value_RangeSet lb rb = explain c :: Value_RangeSet
            (Element c) (LeftBoundType c) (RightBoundType c)
      in  (lb, rb)

    -- c == {a | left_bound c <= a <= right_bound c}


class OpDynZero a where
    zero :: a
instance OpDynZero UInt where
    zero = 0
instance OpLeftBoundCmp UInt (LeftBound UInt) where
    type LeftBoundCmpExResult (LeftBound UInt) = P.Ordering
instance OpRightBoundCmp UInt (RightBound UInt) where
    type RightBoundCmpExResult (RightBound UInt) = P.Ordering

{-
instExplain
    [t|(Value_RangeSet UInt (LeftBound UInt) (RightBound UInt))|]
    [t|(UIntSet_Lt UInt)|]
    [e| \(Value_NaturalSet_Lt may_inf) ->
        (LeftInclude $ Just 0, RightExclude $ may_inf) |]
-}
instance (Natural u, OpDynZero u)
    => OpSafeFrom   (UIntSet_Lt u)
                    (Value_RangeSet u (LeftBound u) (RightBound u))
instance (Natural u, OpDynZero u)
    => OpFrom   (UIntSet_Lt u)
                (Value_RangeSet u (LeftBound u) (RightBound u)) where
    from (Value_NaturalSet_Lt may_inf) =
        Value_RangeSet (LeftInclude zero) $ case may_inf of
            MaybeInf (Just u) -> (RightExclude $ u)
            _ -> RightExcludePosOO
instance (Natural u, OpDynZero u)
    => Explain  (Value_RangeSet u (LeftBound u) (RightBound u))
                (UIntSet_Lt u) where
instance (Natural u, OpDynZero u) => OpNull (UIntSet_Lt u) where
    type NullExResult (UIntSet_Lt u) = Bool
    null = null_ex
    null_ex s =
      case explain s :: Value_RangeSet u (LeftBound u) (RightBound u) of
        Value_RangeSet (LeftInclude a) (RightExclude b) -> P.not $ a < b
        Value_RangeSet (LeftExclude a) (RightInclude b) -> P.not $ a < b
        Value_RangeSet (LeftInclude a) (RightInclude b) -> b < a
        Value_RangeSet (LeftExclude a) (RightExclude b) ->
            P.not (a < b) || (E.toInteger b - E.toInteger a < 2)
        _ -> False

instance( Natural u, OpDynZero u
        , OpLeftBoundCmp u (LeftBound u)
        , OpRightBoundCmp u (RightBound u)
        )
    => RangeSet (UIntSet_Lt u) where
    type LeftBoundType (UIntSet_Lt u) = LeftBound u
    type RightBoundType (UIntSet_Lt u) = RightBound u

instance Natural u => OpSafeFrom (UIntSet_Lt u)
    (Value_RangeSet UInt Value_LeftInclude_0 (RightBound u))
instance Natural u => Explain (Value_RangeSet UInt Value_LeftInclude_0 (RightBound u))
    (UIntSet_Lt u)
instance Natural u => OpFrom (UIntSet_Lt u)
    (Value_RangeSet UInt Value_LeftInclude_0 (RightBound u)) where
    from (Value_NaturalSet_Lt (MaybeInf (Just u))) =
            Value_RangeSet the_only_value (RightExclude u)
    from _ = Value_RangeSet the_only_value RightExcludePosOO

instance Natural u => OpSafeFrom (UIntSet_Lt u) (UIntSet_Lt UInt)
instance Natural u => OpFrom (UIntSet_Lt u) (UIntSet_Lt UInt) where
    from (Value_NaturalSet_Lt m) = Value_NaturalSet_Lt $ fmap explain m
instance Natural u => Explain (UIntSet_Lt UInt) (UIntSet_Lt u)



instance Natural u => OpSafeFrom (RightBound u) (RightBound UInt)
instance Natural u => OpFrom (RightBound u) (RightBound UInt) where
    from (RightExcludePosOO) = RightExcludePosOO
    from (RightExclude u) = RightExclude $ explain u
    from (RightInclude u) = RightInclude $ explain u
instance Natural u => Explain (RightBound UInt) (RightBound u)

instance( Natural u, OpDynZero u
        , OpLeftBoundCmp u (LeftBound u)
        , OpRightBoundCmp u (RightBound u)
        , Explain (RightBound UInt) (RightBoundType (UIntSet_Lt u))
        )
    => NaturalSet_From0To (UIntSet_Lt u) where
instance NaturalSet_From0To (UIntSet_Lt UInt)
class   ( RangeSet c, Natural (Element c)
        , Explain (RightBound UInt) (RightBoundType c)
        , Explain
            (Value_RangeSet UInt Value_LeftInclude_0 (RightBoundType c))
            c
        , TotalOrdSet c
        , Explain (Value_NaturalSet_Lt (MaybeInf UInt)) c
        )
    => NaturalSet_From0To c where
    -- useless
    --  e.g. [a] inf length => explain not return
    maybe_min_natural_excluded :: c -> Maybe UInt
    maybe_min_natural_excluded c = case explain c of
        Value_NaturalSet_Lt (MaybeInf m) -> m
    {-
      case explain $ right_bound c :: RightBound (Element c) of
        RightInclude a -> Just $ explain a + 1
        RightExclude a -> Just $ explain a
        RightExcludePosOO -> Nothing
    -}

class (NaturalSet_From0To c, Sized c, Explain (Value_NaturalSet_Lt UInt) c)
    => FiniteNaturalSet_From0To c where
    min_natural_excluded :: c -> UInt
    min_natural_excluded c = case explain c of
        Value_NaturalSet_Lt m -> m

-}

























































































---------------------------
-- S.Set, IS.IntSet
-- , Value_NaturalSet_Lt (MaybeInf u), Value_FiniteNaturalSet_Lt
type D_Set = S.Set
type D_IntSet = IS.IntSet
type UIntSet_Lt u = Value_NaturalSet_Lt (MaybeInf u)





instance Container (D_Set a) where
    type Element (D_Set a) = a
instance SetConcept (D_Set a)
instance Ord a => OpMember (D_Set a) where
    type MemberExResult (D_Set a) = Bool
    member_ex = S.member
    member = member_ex
instance Eq a => SetEq (D_Set a) where
    (|==|) = (==)
instance Ord a => SetOrd (D_Set a) where
    type SetCmpExResult (D_Set a) = POrd
    (|<|) = S.isProperSubsetOf
    (|<=|) = S.isSubsetOf
instance Ord a => LinearElementsSetBase (D_Set a) where
    before _ = compare
        -- undefined behavior if e1 e2 not in a
instance Ord a => ClockwiseElementsSetBase (D_Set a) where
    clockwise _ = default_clockwise__byOrd
        -- undefined behavior if e1 e2 e3 not in a

instance CountableContainer (D_Set a)
instance FiniteContainer (D_Set a)
instance SequenceConcept (D_Set a)
instance OpNull (D_Set a) where
    type NullExResult (D_Set a) = Bool
    null_ex = S.null
    null = null_ex
instance OpUnsafeLen (D_Set a)
instance OpLenIfLe (D_Set a)
instance Sized (D_Set a) where
    type LenExResult (D_Set a) = UInt
    len_ex = unsafe_from . P.toInteger . S.size
    len = len_ex
instance OpHasDuplicates (D_Set a) where
    type HasDuplicatesExResult (D_Set a) = Value_False
    has_duplicates _ = False
instance NoDuplicates (D_Set a)










instance Container D_IntSet where
    type Element D_IntSet = Int
instance SetConcept D_IntSet
instance OpMember D_IntSet where
    type MemberExResult D_IntSet = Bool
    member_ex = IS.member
    member = member_ex
instance SetEq D_IntSet where
    (|==|) = (==)
instance SetOrd D_IntSet where
    type SetCmpExResult D_IntSet = POrd
    (|<|) = IS.isProperSubsetOf
    (|<=|) = IS.isSubsetOf
instance LinearElementsSetBase D_IntSet where
    before _ = compare
        -- undefined behavior if e1 e2 not in a
instance ClockwiseElementsSetBase D_IntSet where
    clockwise _ = default_clockwise__byOrd
        -- undefined behavior if e1 e2 e3 not in a



instance CountableContainer D_IntSet
instance FiniteContainer D_IntSet
instance SequenceConcept D_IntSet
instance OpNull D_IntSet where
    type NullExResult D_IntSet = Bool
    null_ex = IS.null
    null = null_ex
instance OpUnsafeLen D_IntSet
instance OpLenIfLe D_IntSet
instance Sized D_IntSet where
    type LenExResult D_IntSet = UInt
    len_ex = unsafe_from . P.toInteger . IS.size
    len = len_ex
instance OpHasDuplicates D_IntSet where
    type HasDuplicatesExResult D_IntSet = Value_False
    has_duplicates _ = False
instance NoDuplicates D_IntSet

















instance Natural a => Container (UIntSet_Lt a) where
    type Element (UIntSet_Lt a) = a
instance Natural a => SetConcept (UIntSet_Lt a)
instance Natural a => OpMember (UIntSet_Lt a) where
    type MemberExResult (UIntSet_Lt a) = Bool
    member_ex u (Value_NaturalSet_Lt (MaybeInf (Just v))) = u < v
    member_ex _ _ = True
    member = member_ex
instance Natural a => EqIsSetEq (UIntSet_Lt a)
instance Natural a => SetEq (UIntSet_Lt a)
instance Natural a => OrdIsTotalSetOrd (UIntSet_Lt a)
instance Natural a => SetOrd (UIntSet_Lt a) where
    type SetCmpExResult (UIntSet_Lt a) = P.Ordering
    (|<|) = (<)
    (|<=|) = (<=)
    set_compare_ex = compare
    set_compare a = ord2pord . compare a
instance Natural a => LinearElementsSetBase (UIntSet_Lt a) where
    before _ = compare
        -- undefined behavior if e1 e2 not in a
instance Natural a => ClockwiseElementsSetBase (UIntSet_Lt a) where
    clockwise _ = default_clockwise__byOrd
        -- undefined behavior if e1 e2 e3 not in a



instance Natural a => CountableContainer (UIntSet_Lt a)
instance Natural a => SequenceConcept (UIntSet_Lt a)
instance Natural a => OpNull (UIntSet_Lt a) where
    type NullExResult (UIntSet_Lt a) = Bool
    null_ex (Value_NaturalSet_Lt (MaybeInf (Just u))) = E.toInteger u == 0
    null_ex _ = False
    null = null_ex
instance Natural a => OpUnsafeLen (UIntSet_Lt a) where
    unsafe_len (Value_NaturalSet_Lt (MaybeInf (Just u))) = explain u
    unsafe_len _ = error "inf len @ unsafe_len"
instance Natural a => OpLenIfLe (UIntSet_Lt a) where
    len_if_le i a = let n = unsafe_len a in
        if explain n <= i then Just n else Nothing
instance Natural a => OpHasDuplicates (UIntSet_Lt a) where
    type HasDuplicatesExResult (UIntSet_Lt a) = Value_False
    has_duplicates _ = False
instance Natural a => NoDuplicates (UIntSet_Lt a)














-- type FNS_Lt a = Value_FiniteNaturalSet_Lt a
instance Natural a => Container (Value_FiniteNaturalSet_Lt a) where
    type Element (Value_FiniteNaturalSet_Lt a) = a
instance Natural a => SetConcept (Value_FiniteNaturalSet_Lt a)
instance Natural a => OpMember (Value_FiniteNaturalSet_Lt a) where
    type MemberExResult (Value_FiniteNaturalSet_Lt a) = Bool
    member_ex a (Value_FiniteNaturalSet_Lt b) = a < b
    member = member_ex
instance Natural a => SetEq (Value_FiniteNaturalSet_Lt a) where
    (|==|) = (==)
instance Natural a => SetOrd (Value_FiniteNaturalSet_Lt a) where
    type SetCmpExResult (Value_FiniteNaturalSet_Lt a) = P.Ordering
    (|<|) = (<)
    (|<=|) = (<=)
    set_compare_ex = compare
    set_compare a = ord2pord . compare a
instance Natural a => LinearElementsSetBase (Value_FiniteNaturalSet_Lt a) where
    before _ = compare
        -- undefined behavior if e1 e2 not in a
instance Natural a => ClockwiseElementsSetBase (Value_FiniteNaturalSet_Lt a) where
    clockwise _ = default_clockwise__byOrd
        -- undefined behavior if e1 e2 e3 not in a

instance Natural a => CountableContainer (Value_FiniteNaturalSet_Lt a)
instance Natural a => FiniteContainer (Value_FiniteNaturalSet_Lt a)
instance Natural a => SequenceConcept (Value_FiniteNaturalSet_Lt a)
instance Natural a => OpNull (Value_FiniteNaturalSet_Lt a) where
    type NullExResult (Value_FiniteNaturalSet_Lt a) = Bool
    null_ex (Value_FiniteNaturalSet_Lt a) = E.toInteger a == 0
    null = null_ex
instance Natural a => OpUnsafeLen (Value_FiniteNaturalSet_Lt a)
instance Natural a => OpLenIfLe (Value_FiniteNaturalSet_Lt a)
instance Natural a => Sized (Value_FiniteNaturalSet_Lt a) where
    type LenExResult (Value_FiniteNaturalSet_Lt a) = a
    len_ex (Value_FiniteNaturalSet_Lt a) = a
    len = explain . len_ex
instance Natural a => OpHasDuplicates (Value_FiniteNaturalSet_Lt a) where
    type HasDuplicatesExResult (Value_FiniteNaturalSet_Lt a) = Value_False
    has_duplicates _ = False
instance Natural a => NoDuplicates (Value_FiniteNaturalSet_Lt a)















-- _liftFL f (FiniteList ls) = FiniteList (f ls)
instance Container (FiniteList a) where
    type Element (FiniteList a) = a
instance Eq a => SetConcept (FiniteList a)
instance Eq a => OpMember (FiniteList a) where
    type MemberExResult (FiniteList a) = Bool
    member_ex k = P.elem k . unFiniteList
    member = member_ex
instance Eq a => SetEq (FiniteList a) where
    a |==| b = a |<=| b && b |<=| a
instance Eq a => SetOrd (FiniteList a) where
    type SetCmpExResult (FiniteList a) = POrd
    FiniteList a |<=| FiniteList b = a L.\\ b == []


instance CountableContainer (FiniteList a)
instance FiniteContainer (FiniteList a)
instance SequenceConcept (FiniteList a)
instance OpNull (FiniteList a) where
    type NullExResult (FiniteList a) = Bool
    null_ex = P.null . unFiniteList
    null = null_ex
instance OpUnsafeLen (FiniteList a)
instance OpLenIfLe (FiniteList a) where
    len_if_le i (FiniteList ls) = len_if_le i ls
instance Sized (FiniteList a) where
    type LenExResult (FiniteList a) = UInt
    len_ex = unsafe_len . unFiniteList
    len = len_ex
instance Eq a => OpHasDuplicates (FiniteList a) where
    type HasDuplicatesExResult (FiniteList a) = Bool
    has_duplicates = has_duplicates_ex
    has_duplicates_ex (FiniteList ls) = f ls where
        f (a : ls) = if P.elem a ls then True else f ls
        f [] = False



--}
--}
--}
--}
--}
--}
