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

module Container_class_base where
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import qualified Data.List as L
import qualified Data.Map as M -- (Map)
import qualified Data.Set as S -- (Set)
import qualified Prelude as P
import qualified Data.Bits as B
import Data.Monoid
import Group
import Prelude hiding (lookup, null, take, length, fromInteger)
-- import Prelude (Eq, Ord, Num, Integral, Integer, Real)
import Data.Bits ((.&.))
import SeedUtils -- hiding (safe_head)
import Ordering
import Boxed

{-
Empty a c : c = {} | {empty} i.e. any c object must be empty container
    ==>> Xxx a c : c |<=| Xxx
CtorEmpty a c : empty in c
    ==>> CtorXxx a c : Xxx |<=| c

class ReadOnly a where
    f :: ... a ... -> OUT
    -- OUT ~ (ReadOnlyRes a), with constraint LikeOut OUT
    -- so that we can refine ReadOnly by refine ReadOnlyRes
class WriteOnly a where
    h :: Input ... -> a
class ReadWrite a where
    g :: ... a ... -> a
class ReadOnly a => C a ==>> C |<=| assume(ReadOnly)
class WriteOnly a => C a ==>> generate(WriteOnly) |<=| C
class ReadWrite a => C a ==>> generate(ReadWrite) |<=| C |<=| assume(ReadWrite)

-- not a -> a -> a
-- divisor cannot be 0; but result could!
Int // NonZero = Int
PInt // Pint = Int








three container class types and two op class type:
    1) Xxx a c: static: readonly: class <==> set of objects
        c |<=| Xxx
        subclass have a subset of objects
        c should not occur as output
    2) XxxOp c/Xxx a c: op over Xxx
    3) (Xxx a c, XxxOp c, CtorYyy) => DynXxx a c: dynamic: constructors
        Yyy |<=| c |<=| DynXxx |<=| Xxx
        output c
        if Xxx a c => Yyy a c
        then DynYyy a c => DynXxx in concept
        i.e. output yyy are subset of output xxx

    1') Opop c/Opop a c:
        c |<=| op
        c should not occur as output
    2') DynOpXxx:
        Xxx |<=| c |<=| DynOpXxx
        output c
-}


class DynOpNumMul a where
    (*) :: a -> a -> a
class DynOpNumDiv a where
    (/) :: a -> a -> a
class DynOpNumAdd a where
    (+) :: a -> a -> a
class (SafeDynOpNumSub a, Singleton a (SafeDynOpNumSubResult a))
    => DynOpNumSub a where
    (-) :: a -> a -> a
class Optional a (SafeDynOpNumSub a) => SafeDynOpNumSub a where
    type SafeDynOpNumSubResult a :: *
    safe_sub :: a -> a -> SafeDynOpNumSubResult a
class SafeDynOpNumAbs a => DynOpNumAbs a where
    abs :: a -> a
class SafeDynOpNumAbs a where
    safe_abs :: a -> Maybe a
class SafeDynOpNumNeg a => DynOpNumNeg a where
    neg :: a -> a
class SafeDynOpNumNeg a where
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



class Explain Bool a => Boolean a where
toBool :: Boolean a => a -> Bool
toBool = explain
test :: Explain Bool a => a -> Bool
test = explain

class (Explain Value_True a, Boolean a) => Boolean_True a
class (Explain Value_False a, Boolean a) => Boolean_False a



data Value_Zero
data Value_One
data Value_ZeroOrOne
data Value_True
data Value_False
data Value_EmptySet
data Value_Just a = Value_Just a



class (Explain Integer a, Ord a) => Integral a where
    -- must be integer
toInteger :: Integral a => a -> Integer
toInteger = explain
newtype UInt = UInt Integer -- nonnegative
newtype PInt = PInt Integer -- positive
class (Integral a, Explain UInt a) => Natural a where
    -- nonnegative integer
class (Natural a, Explain PInt a) => PositiveInt a where
class (Natural a, Explain Zero a) => NaturalEq0 a where
class (Natural a, Explain ZeroOrOne a) => NaturalLe1 a where
class (Natural a, Explain One a) => NaturalEq1 a where



class   ( DynOpAdd a, DynOpMul a, DynOpDivMod a
        , DynOpSucc a)
    => PositiveIntOp a
class PositiveIntOp a => NaturalOp a where
    -- occasional
    -- there are PositiveIntOp that are not NaturalOp
class   (DynOpSub a, DynOpPrev a, NaturalOp a)
        -- , DynOpZero, DynOpOne, DynOpNegOne)
    => IntegralOp a where

class (PositiveInt a, PositiveIntOp a, View PInt a, DynOpOne a)
    => DynPositiveInt a where
class (Natural a, NaturalOp a, View UInt a, DynOpOne a, DynOpZero a)
    => DynNatural a where
class   ( Integral a, IntegralOp a, View Integer a
        , DynOpZero, DynOpOne, DynOpNegOne)
    => DynIntegral a where
    -- can store any integer
fromInteger :: DynIntegral a => Integer -> a
fromInteger = make






class Container a c | c -> a where
class Container a c => Countable a c where
class Countable a c => Finite a c where
class (Finite a c, Natural (SizedResult c)) => Sized a c where
    type SizedResult c :: *
    len :: c -> SizedResult c
    -- why not Integral? because they are not DynIntegral!
-- SetBase without Eq a
class   ( Sized a c, NaturalEq0 (SizedResult c)
        , Boolean_True (NullResult c)
        , Boolean_False (MemberResult c)
        , Set a c, Seq a c, Explain Value_EmptySet c)
    => Empty a c where
    -- always empty
    -- len _ = 0
    -- Empty a c, OpEmpty a c, DynSet a c => DynEmpty a c

class   ( Sized a c, NaturalEq1 (SizedResult c), NonNull a c
        , Set a c, Seq a c, Explain (Value_Just a) c) => Singleton a c
    -- len _ = 1
class   ( Sized a c, NaturalLe1 (SizedResult c)
        , Set a c, Seq a c, Explain (Maybe a) c) => Optional a c
    -- len _ = 0 or 1
class (Container a c, OpMember a c, SetOrd c) => Set c
class (Set k (KeyView c), Optional v (LookupResult c))
    => MapView k v c | c -> k v where
    -- not a container
    type KeyView c :: *
    type LookupResult c :: *
    key_set :: c -> KeyView c
    lookup :: k -> c -> LookupResult c
newtype NaturalAsSet a = NaturalAsSet a -- [0..n-1]
instance Natural a => Set (NaturalAsSet a)
class   ( Countable a c, Explain [a] c, MapView i a c
        , KeyView c ~ NaturalAsSet i, Natural i)
    => Seq a c where



------------- Op
class Boolean c => OpNull a c where
    NullResult c :: *
    null :: c -> NullResult c
class Boolean (MemberResult c) => OpMember a c where
    MemberResult c :: *
    member :: a -> c -> MemberResult c
class (OpNull a c, Boolean_False (NullResult c))
    => NonNull a c


-------------- DynOp
class DynOpEmpty c where
    empty :: c
------------- Dyn
class (Empty a c, DynSet a c, DynSeq a c, View Value_EmptySet c, DynOpEmpty c)
    => DynEmpty a c








class (Container a c, Container b d) => OpOmapEx a c b d where
    omap_ex :: (a->b) -> c -> d
class OpOmapEx a c a c => OpOmap a c where
    omap :: (a->a) -> c -> c
    omap = omap_ex
{-
class Iterable a c => CountableContainer a c where
class (CountableContainer a c, Sized a c) => FiniteContainer a c where
instance Iterable a c => CountableContainer a c
instance (CountableContainer a c, Sized a c) => FiniteContainer a c
-}

class   ( CountableContainer a inner, CountableContainer a outer
        , Container a result)
    => OpMergeCC_Base a inner outer result where
    merge_countable_countable_base :: (a -> inner) -> outer -> result
class OpMergeCC_Base a inner outer outer => OpMergeCC a inner outer where
    -- unorder
    -- countable infinite
    -- see SeedUtils.merge_lsls
    --      Merge a [a] [a]
    --      merge_countable_countable f = merge_lsls . map f
    -- used in Merge pst psts psts => NonDeterministicAutomaton
    --      null_transition :: a -> pst -> psts
    --      null_transitions_step1 :: a -> psts -> psts
    --      null_transitions_step1 a =
    --          merge_countable_countable (null_transition a)
    merge_countable_countable :: (a -> inner) -> outer -> outer
    merge_countable_countable = merge_countable_countable_base
    merge_closureBy
        :: (outer -> outer -> Bool) -> (a -> inner) -> outer -> outer
    merge_closureBy eq f = find_fixed_pointBy eq
        $ merge_countable_countable f

class OpValid c where
    valid :: c -> Bool
class Container a c => OpNull a c where
    --is_empty :: c -> Bool
    --is_empty = null
    null :: c -> Bool
    default null :: NonNull a c => c -> Bool
    null = const False
    not_null :: c -> Bool
    not_null = not . null
class OpNull a c => NonNull a c where
class OpNull a c => OpEmpty a c where
    -- OpNull may not be OpEmpty
    -- e.g. NonNullList
    empty :: c
    -- allow lots empties, may not equal
    -- null empty == True
    -- Eq a => null a =xx=>> a == empty ; there're lots nulls
class Container a c => OpMember a c where
    -- with Eq a!!
    member :: a -> c -> Bool
class OpMember a c => OpChoose a c where
    -- useful if a == b but a is not b
    choose :: a -> c -> Maybe a -- one Eq member
class OpChoose a c => OpChooses a c where
    chooses :: a -> c -> [a]

safe_head_iter :: Iterable a c => c -> Maybe a
safe_head_iter = listToMaybe . iter

class OpNull a c => OpAnyElem a c where
    any_elem :: c -> Maybe a
    default any_elem :: Iterable a c => c -> Maybe a
    any_elem = safe_head_iter
class (OpIterLe a c, CountableContainer a c) => Iterable a c where
    -- exhaust all values, not allow infinite except countable infinite
    iter :: c -> [a]
    toList :: c -> [a]
    toList = iter

fromInteger :: Integral n => Integer -> n
fromInteger i = let n = P.fromInteger i in
    if i /= toInteger n then error "overflow: fromInteger" else n

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

len_le__ls n = test . len_if_le__ls n
len_if_le__ls :: Integer -> [a] -> Maybe Integer
--len_if_le__ls n ls = let (init, tail) = splitAt (fromInteger n) ls in
--        justif (n >= 0 && null tail) (length init)
len_if_le__ls i0 ls = f i0 ls where
    f i _ | i < 0 = Nothing
    f i (a:ls) = f (i-1) ls
    f i [] = Just $ i0 - i


class (OpAnyElem a c, OpLenLe a c) => OpIterLe a c where
    -- allow infinite (not only countable)
    -- but error when fails
    iter_le, iter_eq :: Integer -> c -> [a]
    default iter_le :: Iterable a c => Integer -> c -> [a]
    default iter_eq :: Iterable a c => Integer -> c -> [a]
    iter_le n c | len_le n ls = ls where ls = iter c
    iter_le _ _ = error "iter_le n c while not len_le n c"
    iter_eq n c | len_eq n ls = ls where ls = iter c
    iter_eq _ _ = error "iter_eq n c while not len_eq n c"

    only_elem :: c -> a
    only_elem = only_head . iter_eq 1
    maybe_elem :: c -> Maybe a
    maybe_elem = listToMaybe . iter_le 1

class UnsafeSized a c => OpLenLe a c where
    len_if_le :: Integer -> c -> Maybe Integer
    default len_if_le :: Iterable a c => Integer -> c -> Maybe Integer
    -- when c is not [a]
    len_if_le n = len_if_le n . iter

    len_le, len_eq, len_lt :: Integer -> c -> Bool
    len_le n = test . len_if_le n
    len_lt n = test . len_if_le (n-1)
    len_eq n ls = len_if_le n ls == Just n
    {-
    unsafe_len :: c -> Integer
    unsafe_len c = f n0 where
        e = 10
        n0 = B.shift 1 e
        f n = maybe (f $ B.shift n e) id $ len_if_le n
    -}

class (UnsafeSized a c, OpLenLe a c, FiniteContainer a c)
    => Sized a c where
    -- i.e. finite size; not like [a]
    len :: c -> Integer
class OpNull a c => UnsafeSized a c where
    -- i.e. countable size; like [a]
    -- may not return
    unsafe_len, length :: c -> Integer
    -- default unsafe_len :: OpLenLe a c => c -> Integer
    default unsafe_len :: Sized a c => c -> Integer
    unsafe_len = len
    length = unsafe_len
class Container a c => OpInsert a c where
    extend :: Iterable a it => Integer -> it -> c -> c
    -- extend n it c = foldl' (flip insert) c $ take (fromInteger n) (iter it)
    -- extend n it c = foldr insert c $ take (fromInteger n) (iter it)
    extend n it c = foldr insert c $ take n it
    insert :: a -> c -> c
class Iterable a c => OpPop a c where
    -- sometimes Iterable is not OpPop
    -- e.g. NonNullList, size >= 1 ==>> pop :: c -> (a, Maybe c)
    pop :: c -> Maybe (a, c)
    pop c = listToMaybe $ pops c
    pops :: c -> [(a, c)]
    -- take care: sometimes we cannot implement pops via pop
    -- i.e. (-oo, -2) \/ (2, +oo)
    --  if pop (-oo, i)\/(j,+oo) = Just (i-1, (-oo, i-1)\/(j,+oo))
    --  then pops cannot be implemented via pop
class OpMember a c => OpRemove a c where
    remove :: a -> c -> Maybe ([a], c)
    discard :: a -> c -> Maybe c
    discard a c = remove a c >>= return . snd
    delete :: a -> c -> c
    delete a c = maybe c snd $ remove a c

class (OpUnSingleton a c, Set a c)
    => Optional a c where
    -- No OpEmpty or OpSingleton
    -- DynOptional
class (Optional a c, 
class (NonNull a c, OpSingleton a, OpUnSingleton a c, Set a c
      , Optional a c)
    => Singleton a c where
    unsingleton :: c -> a

class Container a c => OpUnSingleton a c where
    -- c is not necessary OpSingleton
    -- when is singleton, return Just
    safe_unsingleton :: c -> Maybe a
    default safe_unsingleton :: Iterable a c => c -> Maybe a
    safe_unsingleton = safe_unsingleton . iter
    unsafe_unsingleton :: c -> a
    unsafe_unsingleton c = case safe_unsingleton c of
        Just a -> a
        Nothing -> error ""
class Container a c => OpSingleton a c where
    singleton :: a -> c
    default singleton :: (OpEmpty a c, OpInsert a c) => a -> c
    singleton a = insert a empty
class (OpEmpty a c, OpInsert a c, OpSingleton a c)
    => AnyFiniteSize a c where
    -- xxxx error: c can be of any finite size xxxxx
    --      e.g. NonNullList cannot have size 1
    --      input can be of any finite size instead of c!!!
    --      and may be infinite!???
    -- AnyFiniteSize v.s. OpInsert ==>> OpEmpty
    from_iterable :: Iterable a it => Integer -> it -> c
    from_iterable n it = extend n it empty
    fromList :: [a] -> c -- unsafe
    fromList ls = from_iterable (unsafe_len ls) ls
class   ( AnyFiniteSize a q, OpPop a q, SemiGroupWithIdentity q
        , OpOmap a q)
    => Buffer a q


----
class Container a c => OpPartition a c where
    partition :: (a->Bool) -> c -> (c, c) -- (trues, falses)












------------ List
-- but a = a++[] fail null
instance OpOmap a [a] where
    omap = map
instance OpEmpty a [a] where
   empty = []
instance OpNull a [a] where
   null = P.null
instance Container a [a] where
instance Eq a => OpMember a [a] where
    member = P.elem
instance Buffer a [a]
instance AnyFiniteSize a [a]
instance OpSingleton a [a]
instance OpUnSingleton a [a] where
    safe_unsingleton [a] = Just a
    safe_unsingleton _ = Nothing
instance OpPop a [a] where
    pops (h:t) = (h, t) : pops t
    pops [] = []
instance OpInsert a [a] where
    insert = (:)
instance CountableContainer a [a] where
instance Iterable a [a] where
    iter = id
instance OpAnyElem a [a] where
    any_elem = listToMaybe

instance UnsafeSized a [a] where
    unsafe_len = toInteger . P.length
instance OpIterLe a [a]

instance OpLenLe a [a] where
    len_if_le = len_if_le__ls
    {-
    len_le n _ | n < 0 = False
    len_le _ [] = True
    len_le n (a:ls) = len_if_le (n-1) ls
    -}
instance OpPartition a [a] where
    partition = L.partition

instance SemiGroup [a] where
    (><) = (++)
