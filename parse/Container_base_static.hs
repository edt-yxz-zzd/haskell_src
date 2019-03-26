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

module Container_base_static where
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import qualified Data.List as L
import qualified Data.Map as M -- (Map)
import qualified Data.Set as S -- (Set)
import qualified Prelude as P
import qualified Data.Bits as B
import Data.Monoid
import Group
-- import Prelude hiding (lookup, null, take, length, fromInteger)
import Prelude (Eq(..), Ord(..), Maybe(..), Bool(..), Monad(..)
                , (.), ($), Integer, error, not)
import Prelude ((-), (+))
import Data.Bits ((.&.))
import SeedUtils -- hiding (safe_head)
import Ordering
import Boxed
import Explain
import ExplainEx







{-
Empty a c
    [forall (x::c) : len x == 0]
OpDynEmpty a c
    [len empty == 0]
    empty :: c
OpAdd a
    [(a+b)+c == a+(b+c)]
    (+) :: a -> a -> a


Empty a c ::= Empty a = {c | Empty a c} = { {}, {{}} }
    i.e. any c object must be empty container
    i.e. c has no object or has only one - the empty
    [Xxx a c] <==> [c in (Xxx a)]
OpDynEmpty a c ::= OpDynEmpty a = {c | {} in c}
    [Empty a e][OpDynEmpty a c]
    <==> [e in (Empty a)][c in (OpDynEmpty a)]
        ==>> e |<=| c
        ==>> \/~ (Empty a) |<=| c
        ==>> \/~ (Empty a) |<=| /\~ (OpDynEmpty a) |<=| c
    [OpDynXxx a c] ==>> Xxx a |<=| c

OpAdd a ::= {a | a with +}
    ????





class IXxx ... c where -- I : input
    type Result c :: * -- not c
    f :: ... c ... -> Result c
    -- to refine
    -- by subclass IXxx or refine output Result
class OXxx ... c where
    g :: ..not c.. -> c -- O : output
    -- constructor of c
class IOXxx ... c where
    h :: ... c... -> c
    -- transform/combine...

instance IXxx ... C where
    -- C |<=| assume(IXxx)
instance OXxx ... C where
    -- generate(OXxx) |<=| C
instance IOXxx ... C where
    -- ?? Cat??





-- not a -> a -> a
-- divisor cannot be 0; but result could!
Int // NonZero = Int
PInt // Pint = Int



properties/relation:
    I??? c
    1) Xxx c
        c |<=| Xxx
    2) OpFff c
        c |<=| OpFff
methods:
    IO??? c
    3) XxxOp c
constructors:
    4) OpDynYyy c
        Yyy |<=| c
axioms = constructors + methods + constructors:
    5) Xxx c, XxxOp c, OpDynYyy => DynXxx c
        Yyy |<=| c |<=| DynXxx |<=| /\~ [Xxx, XxxOp]



-}


{-
class DynOpMul a where
    (*) :: a -> a -> a
class DynOpDiv a where
    (/) :: a -> a -> a
class DynOpAdd a where
    (+) :: a -> a -> a
class (SafeDynOpSub a, Singleton a (SafeDynOpSubResult a))
    => DynOpSub a where
    (-) :: a -> a -> a
class Optional a (SafeDynOpSubResult a) => SafeDynOpSub a where
    type SafeDynOpSubResult a :: *
    safe_sub :: a -> a -> SafeDynOpSubResult a
class SafeDynOpAbs a => DynOpAbs a where
    abs :: a -> a
class SafeDynOpAbs a where
    safe_abs :: a -> Maybe a
class SafeDynOpNeg a => DynOpNeg a where
    neg :: a -> a
class SafeDynOpNeg a where
    safe_neg :: a -> Maybe a

class DynOpFloorDiv a where
    floordiv :: a -> a -> Maybe a
    (//) :: a -> a -> a
    (//) = unjust . floordiv
class DynOpMod a where
    mod :: a -> a -> Maybe a
    (%) :: a -> a -> a
    (%) = unjust . mod
class (DynOpAdd a, DynOpMul a, DynOpMod a, DynOpFloorDiv a)
    --, Optional (a, a) r)
    => DynOpDivMod a where
    divmod :: a -> a -> Maybe (a, a)
    divmod a b = a%b >>= (,) (a//b)
    -- a `divmod` b = (c, d)
    -- c == a // d
    -- d == a % b
    -- b * c + d == a
class DynOpSucc a where
    succ :: a -> a
class DynOpPrev a where
    prev :: a -> a
class DynOpZero a where
    zero :: a
class DynOpOne a where
    one :: a
class DynOpNegOne a where
    neg_one :: a

class DynOpMinBound a where
    min_bound :: a
-}

{-
class DynOpSuccEx a b where
    -- no "| a -> b"
    succ_ex :: a -> b
-}







class Container a c | c -> a where
{-
class ContainerEx c where
    type Element c :: *
class (ContainerEx c, a~Element c) => Container a c where
--instance Container (Element c) c => ContainerEx c where
instance Container a c => ContainerEx c where
    type Element c = a
-}

{-
class Container a c | c -> a where
class Container (Element c) c => ContainerEx c where
    type Element c :: *
instance Container a c => ContainerEx c where
    type Element c = a
-}

class Container a c => Countable a c where
class Countable a c => Finite a c where
{-
class (Finite a c, Natural (SizedResult c)) => Sized a c where
    type SizedResult c :: *
    len :: c -> SizedResult c
    -- why not Integral? because they are not DynIntegral!
-}

-- SetBase without Eq a
class   ( Optional a c
        , Set a c
        , NaturalEq0 (SizedResult c)
        , Boolean_True (NullResult c)
        , Boolean_False (MemberResult c)
        , Explain Value_EmptySet c)
    => Empty a c where
    -- always empty
    -- len _ = 0
    -- Empty a c, OpEmpty a c, DynSet a c => DynEmpty a c

class   ( Optional a c
        , NaturalEq1 (SizedResult c), NonNull a c
        , Explain (Value_Just a) c)
    => Singleton a c where
        -- no Set a c, because "member" may require Eq
    -- len _ = 1
    unsingleton :: c -> a
    unsingleton = unValue_Just . explain
class   ( Sized a c, NoDuplicates a c
        --, Seq a c -- Cycle in class declaration (via superclasses)
        , NaturalLe1 (SizedResult c)
        , Explain (Maybe a) c)
    => Optional a c where
    -- len _ = 0 or 1
    unoptional :: c -> Maybe a
    unoptional = explain
    unsafe_unoptional :: c -> a
    unsafe_unoptional = unjust . unoptional





------ Set
class   ( Container a c, OpHasDuplicates a c
        , Boolean_False (HasDuplicatesResult c))
    => NoDuplicates a c
class (Container a c, Boolean (HasDuplicatesResult c))
    => OpHasDuplicates a c where
    type HasDuplicatesResult c :: *
    has_duplicates :: c -> HasDuplicatesResult c
class (Container a c, OpMember a c, SetOrd c) => Set a c where
    -- no Eq a, it should present in instance decl
    -- Eq a in not necessary for OpMember
    --      e.g. EmptySet or Set of one value type
    -- allow duplicates; but they donot affect |==|
    (|<-|) :: a -> c -> Bool -- MemberResult c
    (|<-|) = member -- in
class (Set a c, TotalOrd c) => TotalOrdSet a c
    -- cannot support SetDiff in general
    -- e.g. KeySetType of Seq : {{}, {0}, {0,1}, ...}
class (Set a c, Eq a) => LinearElementsSet a c where
    -- TotalOrdSet : set are ordered
    -- LinearElementsSet : elements per set are ordered
    --                      but diff sets may diff order
    --                      used as topological sort result
    --                      no (Ord a)
    before :: c -> a -> a -> P.Ordering
class (Set a c, Eq a) => ClockwiseElementsSet a c where
    -- elements per set are ordered in a cycle
    -- but diff sets may diff order
    -- used as value type of planar embedding
    -- no (Ord a)
    clockwise :: c -> a -> a -> a -> P.Ordering
        -- LT - clockwise
        -- EQ - |{a,b,c}| < 3 - both clockwise and anti-clockwise
        -- GT - anti-clockwise

data LeftBound a = LeftLimitBeyond | LeftInclude a | LeftLimit a
data RightBound a = RightLimit a | RightInclude a | RightLimitBeyond
data Value_LeftInclude_0



class   ( Ord a, Explain (LeftBound a) lb
        , OrdCmpResult (LeftBoundCmpResult lb)
        )
    => OpLeftBoundCmp a lb | lb -> a where
    type LeftBoundCmpResult lb :: *
    left_bound_cmp :: lb -> a -> LeftBoundCmpResult lb
class   ( Ord a, Explain (RightBound a) rb
        , OrdCmpResult (RightBoundCmpResult rb)
        )
    => OpRightBoundCmp a rb | rb -> a where
    type RightBoundCmpResult rb :: *
    right_bound_cmp :: rb -> a -> RightBoundCmpResult rb

class   ( OpLeftBoundCmp a lb, OpRightBoundCmp a rb
        , OrdCmpResult (LeftRightBoundCmpResult lb rb)
        )
    => OpLeftRightBoundCmp a lb rb where
    type LeftRightBoundCmpResult lb rb :: *
    left_right_bound_cmp :: lb -> rb -> LeftRightBoundCmpResult lb rb
class   ( Set a c, Ord a
        , Explain (LeftBound a) (LeftBoundType c)
        , Explain (RightBound a) (RightBoundType c)
        , Explain (LeftBoundType c, RightBoundType c) c
        , OpLeftBoundCmp a (LeftBoundType c)
        , OpRightBoundCmp a (RightBoundType c)
        , OpNull a c
        -- (1,2) is null if [Integral a] and not null if [Real a]
        -- but [Integral a] ==>> [Real a]
        -- so, whether null is a property of THIS set
        -- sometimes, we may have a limit set of values to represent
        --      invisible values between them
        ) => RangeSet a c where
    type LeftBoundType c :: *
    type RightBoundType c :: *
    left_bound :: c -> LeftBoundType c
    right_bound :: c -> RightBoundType c

    -- c == {a | left_bound c <= a <= right_bound c}


class   ( RangeSet a c, Natural a
        , Explain Value_LeftInclude_0 (LeftBoundType c)
        , TotalOrdSet a c
        )
    => NaturalAsSet a c where




{-
Cycle in class declaration (via superclasses):
  MapView -> Optional -> Seq -> MapView
-}

-- Why MapView is not Container?
-- Map k v c ==>> Container (k, v) c
-- Set k c ==>> Map k () c ==>> Container (k, ()) c
-- Seq a c ==>> Map Int a c ==>> Container a c && Container (Int, a) c
class   ( Set (KeyType c) (KeySetType c)
        , Container (ValueType c) (ValuesType c)
        -- , Optional v (LookupResult c))
        , OpAnyElem (ValueType c) (LookupResultEx c)
        , Optional (ValueType c) (LookupResult c)
        )
    => MapView c where
    -- not a container
    type KeySetType c :: *
    type KeyType c :: *
    type ValueType c :: *
    type ValuesType c :: *
    type LookupResultEx c :: *
    type LookupResult c :: *
    key_set :: c -> KeySetType c
    values :: c -> ValuesType c
    lookup_ex :: k -> c -> LookupResultEx c
    lookup :: k -> c -> LookupResult c
    unsafe_lookup, (!) :: k -> c -> ValueType c
    unsafe_lookup k = unsafe_unoptional . lookup k
    (!) = unsafe_lookup
    -- infinite keys
    -- key to value
    --  1-1: each key map to one value
    --  1-0/1: maybe value
    --  1-many:...
infixl 9 ! -- as Map.!


class (MapView c, LookupResultEx c~LookupResult c)
    => OptionalMapView c
class (OptionalMapView c, Singleton (ValueType c) (LookupResult c))
    => Mapping c where
    mapping :: c -> KeyType c -> ValueType c
    mapping = P.flip unsafe_lookup

class   ( Countable a c, Explain [a] c
        , OptionalMapView c
        , Natural (KeyType c)
        , ValueType c ~ a
        , NaturalAsSet (KeyType c) (KeySetType c)
        )
    => Seq a c where
    -- countable infinite long




















class (OpNull a c, Boolean_False (NullResult c))
    => NonNull a c

------------- Op
class (Container a c, Boolean (NullResult c))
    => OpNull a c where
    type NullResult c :: *
    null_ex :: c -> NullResult c
    default null_ex :: OpDynTheOnlyValue (NullResult c)
        => c -> NullResult c
    null_ex _ = the_only_value
    null :: c -> Bool
    null = explain . null_ex
    not_null :: c -> Bool
    not_null = P.not . null

class (Container a c, Boolean (MemberResult c))
    -- no (Eq a)!
    -- e.g. for Empty/Relation (a, a) ...
    => OpMember a c where
    member :: a -> c -> Bool
    member_ex :: a -> c -> MemberResult c
    type MemberResult c :: *
    member a = explain . member_ex a







----------------- Sized

class OpValid c where
    valid :: c -> Bool


class Container a c => OpIsFull a c where
    -- e.g. query whether the buffer is full
    --      since further insert will be nop
    --      stop insert
    is_full :: c -> Bool


class OpNull a c => OpAnyElem a c where
    any_elem :: c -> Maybe a
    default any_elem :: Iterable a c => c -> Maybe a
    any_elem = safe_head_iter
class (OpIterLe a c, Countable a c) => Iterable a c where
    -- exhaust all values, not allow infinite except countable infinite
    iter :: c -> [a]



class Container a c => OpSafeUnSingleton a c where
    -- c is not necessary OpSingleton
    -- when is singleton, return Just
    safe_unsingleton :: c -> Maybe a
    default safe_unsingleton :: Iterable a c => c -> Maybe a
    -- safe_unsingleton = safe_unsingleton . iter
    safe_unsingleton it = case iter it of
        [a] -> Just a
        _ -> Nothing
    unsafe_unsingleton :: c -> a
    unsafe_unsingleton c = -- only_elem
        case safe_unsingleton c of
            Just a -> a
            Nothing -> error "unsafe_unsingleton"


class OpNull a c => UnsafeSized a c where
    -- i.e. countable size; like [a]
    -- may not return
    unsafe_len, length :: c -> Integer
    -- default unsafe_len :: OpLenLe a c => c -> Integer
    default unsafe_len :: Sized a c => c -> Integer
    unsafe_len = len
    length = unsafe_len

class UnsafeSized a c => OpLenLe a c where
    len_if_le :: Integer -> c -> Maybe Integer
    default len_if_le :: Iterable a c => Integer -> c -> Maybe Integer
    -- when c is not [a]
    -- len_if_le n = len_if_le n . iter
    len_if_le n = len_if_le__ls n . iter

    len_le, len_eq, len_lt :: Integer -> c -> Bool
    len_le n = test . len_if_le n
    len_lt n = test . len_if_le (n-1)
    len_eq n ls = len_if_le n ls == Just n



class   ( Finite a c, Natural (SizedResult c)
        , UnsafeSized a c, OpLenLe a c
        )
    => Sized a c where
    type SizedResult c :: *
    len_ex :: c -> SizedResult c
    ulen :: c -> UInt
    ulen = explain . len_ex
    len :: c -> Integer
    len = explain . ulen
    -- why not Integral? because they are not DynIntegral!
{-
class (UnsafeSized a c, OpLenLe a c, Finite a c)
    => Sized a c where
    -- i.e. finite size; not like [a]
    len :: c -> Integer
-}

class (OpAnyElem a c, OpSafeUnSingleton a c, OpLenLe a c)
    => OpIterLe a c where
    -- allow infinite (not only countable)
    -- but error when fails
    iter_le, iter_eq :: Integer -> c -> [a]
    default iter_le :: Iterable a c => Integer -> c -> [a]
    default iter_eq :: Iterable a c => Integer -> c -> [a]
    iter_le n c | len_le__ls n ls = ls where ls = iter c
    iter_le _ _ = error "iter_le n c while not len_le n c"
    iter_eq n c | len_eq__ls n ls = ls where ls = iter c
    iter_eq _ _ = error "iter_eq n c while not len_eq n c"

    only_elem :: c -> a
    only_elem = only_head . iter_eq 1
    maybe_elem :: c -> Maybe a
    maybe_elem = listToMaybe . iter_le 1









class (OpMember a c, Optional a (ChooseResult c))
    => OpChoose a c where
    -- useful if a == b but a is not b
    choose :: a -> c -> Maybe a -- some a Eq member
    choose_ex :: a -> c -> ChooseResult c
    type ChooseResult c :: *

class (OpChoose a c, Iterable a (ChoosesResult c))
    => OpChooses a c where
    chooses :: a -> c -> [a]
    chooses_ex :: a -> c -> ChoosesResult c
    type ChoosesResult c :: *
    chooses a = iter . chooses_ex a


safe_head_iter :: Iterable a c => c -> Maybe a
safe_head_iter = listToMaybe . iter


take__ls :: Integer -> [a] -> [a]
take__ls n = P.take (fromInteger n)
init_le__ls = take__ls
safe_init_eq__ls i ls =
    justif (not $ len_le__ls (i-1) ls) $ take__ls i ls
take, init_le_iter :: Iterable a c => Integer -> c -> [a]
take n = take__ls n . iter -- take op on it instead of seq
init_le_iter = take
    -- init should op on seq, _iter means call iter first
safe_init_eq_iter :: Iterable a c => Integer -> c -> Maybe [a]
-- safe_init_eq_iter n = jcheck (len_eq n) . take n
safe_init_eq_iter i = safe_init_eq__ls i . iter

len_eq__ls n ls = len_if_le__ls n ls == Just n
len_le__ls n = test . len_if_le__ls n
len_if_le__ls :: Integer -> [a] -> Maybe Integer
--len_if_le__ls n ls = let (init, tail) = splitAt (fromInteger n) ls in
--        justif (n >= 0 && null tail) (length init)
len_if_le__ls i0 ls = f i0 ls where
    f i _ | i < 0 = Nothing
    f i (a:ls) = f (i-1) ls
    f i [] = Just $ i0 - i











