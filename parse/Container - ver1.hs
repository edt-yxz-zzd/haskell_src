{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}



module Container where
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



--------------   EqFst

class Pair p where
    from_pair :: (k, v) -> p k v
    to_pair :: p k v -> (k, v)
instance Pair (,) where
    from_pair = id
    to_pair = id
{-
instance Pair p => Boxed (a,b) (p a b) where
    box = from_pair
    unbox = to_pair
-}
class (Pair p) => EqFst p where
--instance (EqFst p, Eq k) => Eq (p k v) where
--    a == b = fst (to_pair a) == fst (to_pair b)
newtype EqFstPair p a b = EqFstPair { unEqFstPair :: p a b }
    deriving (Show, Read)
instance Pair p => Pair (EqFstPair p) where
    from_pair = EqFstPair . from_pair
    to_pair (EqFstPair p) = to_pair p
instance (Pair p, Eq a) => Eq (EqFstPair p a b) where
    -- (EqFstPair a _) == (EqFstPair a' _) = a == a'
    (==) = lift2 (fst . to_pair) (==)
instance (Pair p, Ord a) => Ord (EqFstPair p a b) where
    -- (EqFstPair a _) < (EqFstPair a' _) = a < a'
    -- (EqFstPair a _) `compare` (EqFstPair a' _) = a `compare` a'
    (<) = lift2 (fst . to_pair) (<)
    compare = lift2 (fst . to_pair) compare
instance Pair p => EqFst (EqFstPair p)






------------------ Container

class Container a c | c -> a where

class Container a c => OpMap a c where
    omap :: (a->a) -> c -> c
class Container a c => CountableContainer a c where
class CountableContainer a c => FiniteContainer a c where
{-
class Iterable a c => CountableContainer a c where
class (CountableContainer a c, Sized a c) => FiniteContainer a c where
instance Iterable a c => CountableContainer a c
instance (CountableContainer a c, Sized a c) => FiniteContainer a c
-}

class   ( CountableContainer a inner, CountableContainer a outer
        , Container a result)
    => MergeCC_Base a inner outer result where
    merge_countable_countable_base :: (a -> inner) -> outer -> result
class MergeCC_Base a inner outer outer => MergeCC a inner outer where
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

class Valid c where
    valid :: c -> Bool
class Container a c => Null a c where
    --is_empty :: c -> Bool
    --is_empty = null
    null :: c -> Bool
    not_null :: c -> Bool
    not_null = not . null
class Null a c => Empty a c where
    -- Null may not be Empty
    -- e.g. NonNullList
    empty :: c
    -- allow lots empties, may not equal
    -- null empty == True
    -- Eq a => null a =xx=>> a == empty ; there're lots nulls
class (Container a c, Eq a) => Member a c where
    -- contains :: a -> c -> Bool
    -- contains = member
    member :: a -> c -> Bool
class Member a c => Choose a c where
    -- useful if a == b but a is not b
    choose :: a -> c -> Maybe a -- one Eq member
class Choose a c => Chooses a c where
    chooses :: a -> c -> [a]

class Null a c => AnyElem a c where
    any_elem :: c -> Maybe a
    default any_elem :: Iterable a c => c -> Maybe a
    any_elem = safe_head_iter
class (IterLe a c, CountableContainer a c) => Iterable a c where
    -- exhaust all values, not allow infinite except countable infinite
    iter :: c -> [a]
    toList :: c -> [a]
    toList = iter

take, init_le_iter :: Iterable a c => Integer -> c -> [a]
take n = P.take (fromInteger n) . iter -- take op on it instead of seq
init_le_iter = take -- init should op on seq, _iter means call iter first
safe_init_eq_iter :: Iterable a c => Integer -> c -> Maybe [a]
safe_init_eq_iter n = jcheck (len_eq n) . take n

class (AnyElem a c, LenLe a c) => IterLe a c where
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
    maybe_elem = safe_head . iter_le 1

class UnsafeSized a c => LenLe a c where
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

class (UnsafeSized a c, LenLe a c, FiniteContainer a c)
    => Sized a c where
    -- i.e. finite size; not like [a]
    len :: c -> Integer
class Null a c => UnsafeSized a c where
    -- i.e. countable size; like [a]
    -- may not return
    unsafe_len, length :: c -> Integer
    -- default unsafe_len :: LenLe a c => c -> Integer
    default unsafe_len :: Sized a c => c -> Integer
    unsafe_len = len
    length = unsafe_len
class Container a c => Insert a c where
    extend :: Iterable a it => Integer -> it -> c -> c
    -- extend n it c = foldl' (flip insert) c $ take (fromInteger n) (iter it)
    -- extend n it c = foldr insert c $ take (fromInteger n) (iter it)
    extend n it c = foldr insert c $ take n it
    insert :: a -> c -> c
class Iterable a c => Pop a c where
    -- sometimes Iterable is not Pop
    -- e.g. NonNullList, size >= 1 ==>> pop :: c -> (a, Maybe c)
    pop :: c -> Maybe (a, c)
    pop c = safe_head $ pops c
    pops :: c -> [(a, c)]
    -- take care: sometimes we cannot implement pops via pop
    -- i.e. (-oo, -2) \/ (2, +oo)
    --  if pop (-oo, i)\/(j,+oo) = Just (i-1, (-oo, i-1)\/(j,+oo))
    --  then pops cannot be implemented via pop
class Member a c => Remove a c where
    remove :: a -> c -> Maybe ([a], c)
    discard :: a -> c -> Maybe c
    discard a c = remove a c >>= return . snd
    delete :: a -> c -> c
    delete a c = maybe c snd $ remove a c

class Singleton a c => UnSingleton a c where
    -- when is singleton, return Just
    safe_unsingleton :: c -> Maybe a
    unsingleton :: c -> a
    unsingleton c = case safe_unsingleton c of
        Just a -> a
        Nothing -> error ""
class Container a c => Singleton a c where
    singleton :: a -> c
    default singleton :: (Empty a c, Insert a c) => a -> c
    singleton a = insert a empty
class (Empty a c, Insert a c, Singleton a c)
    => AnyFiniteSize a c where
    -- xxxx error: c can be of any finite size xxxxx
    --      e.g. NonNullList cannot have size 1
    --      input can be of any finite size instead of c!!!
    --      and may be infinite!???
    -- AnyFiniteSize v.s. Insert ==>> Empty
    from_iterable :: Iterable a it => Integer -> it -> c
    from_iterable n it = extend n it empty
    fromList :: [a] -> c -- unsafe
    fromList ls = from_iterable (unsafe_len ls) ls

class SemiGroup a where
    (><) :: a -> a -> a -- mul/concat
    default (><) :: SemiGroupWithIdentity a => a -> a -> a
    (><) = mappend
class (SemiGroup a, Monoid a) => SemiGroupWithIdentity a where
    identity :: a
    identity = mempty -- one/empty seq
instance (SemiGroup a, Monoid a) => SemiGroupWithIdentity a
class   ( AnyFiniteSize a q, Pop a q, SemiGroupWithIdentity q
        , OpMap a q)
    => Buffer a q


----
class Container a c => OpPartition a c where
    partition :: (a->Bool) -> c -> (c, c) -- (trues, falses)


























------------------ Seq
safe_head_iter :: Iterable a c => c -> Maybe a
safe_head_iter = listToMaybe . iter
class (Iterable a c, MapView Integer a c) --, Map (,) Integer a c)
    => Seq a c where
    safe_head :: c -> Maybe a
    safe_head = safe_head_iter

class (Seq a c, SemiGroup c) => DynSeq a c where
    -- not Empty/Singleton/Insert/Pop
    -- e.g. seq with size of k*n, k > 0
    -- if basic seq sizes are {3, 7}
    --    then all possible sizes are 3*n+7*m
    --    {3, 6, 7, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,...}
    --    N - {0,1,2,4,5,8,11}
    -- (><) :: c -> c -> c -- ++
    -- (><) = mappend not Monoid, except mempty
    -- semigroup
infixr 5 ><, <|, :<

data ViewL a c = EmptyL | a :< c

class (DynSeq a c, Buffer a c)
    => ListLike a c where
    viewl :: c -> ViewL a c
    (<|) :: a -> c -> c


------------------  Set
-- Set - static set
-- DynSet - dynamic set

class (Member a s, Empty a s, SetOrd s) => Set a s where
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

-- class Container a s => SetDifference a s where
-- why not container??
--  e.g. Bool : /\ == &&
--      False = {}, True = {{}}
class SetDifference s where
    difference :: s -> s -> s
    (\\) :: s -> s -> s
    (\\) = difference
class SetIntersection s where
    intersection :: s -> s -> s
    (/\), (/-\) :: s -> s -> s
    (/\) = intersection
    (/-\) = intersection

    default intersection :: SetDifference s => s -> s -> s
    intersection a b = a \\ (a \\ b)
class SetIntersection s => SetBiasedIntersection s where
    (-/\) :: s -> s -> s -- left biased intersection
    (/\-) :: s -> s -> s -- right biased intersection
    (/\-) = flip (-/\)
    default (-/\) :: SetDifference s => s -> s -> s
    a -/\ b = a \\ (a\\b)
class SetUnion s where
    union :: s -> s -> s
    (\/), (\-/) :: s -> s -> s
    (\/) = union
    (\-/) = union
class SetUnion s => SetBiasedUnion s where
    (-\/) :: s -> s -> s -- left biased union
    (\/-) :: s -> s -> s -- right biased union
    (\/-) = flip (-\/)
    default (-\/) :: SetDifference s => s -> s -> s
    a -\/ b = a \/ (b\\a)

class (SetIntersection s, SetDifference s)
    => SetSep s where
    sep :: s -> s -> (s, s, s) -- (0\\1, 0&1, 1\\0)
    sep a b = (a\\b, a/\b, b\\a)
    (//\\) :: s -> s -> (s, s, s)
    (//\\) = sep

class (SetBiasedIntersection s, SetSep s)
    => SetBiasedSep s where
    (-//\\) :: s -> s -> (s, s, s)
    a -//\\ b = (a\\b, a-/\b, b\\a)
    (//\\-) :: s -> s -> (s, s, s)
    a //\\- b = (a\\b, a/\-b, b\\a)
class (SetSep s, SetUnion s)
    => SetOp s where
    -- any container (not neccesary set) may have these operations
    -- may not be container!!
class Universal s where
    universal :: s
class Universal s => Complement s where
    complement :: s -> s
    -- (~) = complement -- no prefix op and (~) is not valid id
    default complement :: SetDifference s => s -> s
    complement = (universal \\)
class (SetBiasedSep s, SetOp s)
    => SetBiasedOp s where
class (Set a s, Universal s) => UniversalSet a s where
class (Set a s, Buffer a s, SetOp s) => DynSet a s where
    unions :: [s] -> s
    unions = foldr union empty
class (DynSet a s, UniversalSet a s, Complement s)
    => DynUniversalSet a s where


-- No (Eq k); we can customize by p
class (Member (p k v) c, Pair p, EqFst p, MapView k v c)
    => Map p k v c where
    -- p - pair
    -- (k, undefined) in dict
    -- (k1, v) == (k2, u) <==> k1 == k2

    -- Note: member :: p k v -> c -> Bool
    --       insert :: p k v -> c -> c
    --       member = has_key . fst
    --
    -- Member k c???????
    -- is Seq a Map?? Iterable v c???
    -- ==>> MapView
class (Eq k, Set k (MapView2KeySet c)) => MapView k v c | c -> k v where
    -- Note: not a container!!!
    --   e.g. Seq a c is a MapView but not a Map container

    type MapView2KeySet c :: *
    key_set :: c -> MapView2KeySet c
    has_key :: k -> c -> Bool
    has_key k = test . lookup k
    -- has_key k = contains (k, error "should not compare value when comparing pair")
    get_item :: k -> c -> Maybe (k, v) -- (p k v)
    default get_item :: (k~Integer, Seq v c) =>
        Integer -> c -> Maybe (Integer, v)
    get_item i = fmap ((,) i) . lookup i . iter
    get_default :: v -> k -> c -> (k, v)
    get_default v k c = may (k, v) $ get_item k c
    at :: k -> c -> Maybe v
    at = lookup
    lookup :: k -> c -> Maybe v
    lookup k = fmap snd . get_item k
    lookup_default :: v -> k -> c -> v
    lookup_default v k c = snd $ get_default v k c

    unsafe_lookup, (!!) :: k -> c -> v
    unsafe_lookup k = unjust . lookup k
    (!!) = unsafe_lookup


class (Map p k v c, SetOp c, Insert (p k v) c)
    => DynMap p k v c where
    -- Note: insert :: p k v -> c -> c
    set_item :: (k, v) -> c -> c
    set_item kv = insert (from_pair kv)
    set_default :: k -> v -> c -> ((k, v), c)
    set_default k v c = case get_item k c of
        Nothing -> let kv = (k, v) in (kv, set_item kv c)
        Just kv -> (kv, c)
    -- default v before or after (v->v)????
    -- v->v always called
    update_default_before :: v -> k -> (v -> v) -> c -> c
    update_default_before v_ k f c = 
        let (_, v) = get_default v_ k c in set_item (k, f v) c
    update_call_default_before :: v -> k -> (v -> (a, v)) -> c -> (a, c)
    update_call_default_before v_ k f c = 
        let (_, v_') = get_default v_ k c
            (a, v) = f v_'
        in (a, set_item (k, v) c)




class (DynUniversalSet a s) => Partition a s p | p -> s where
    part :: a -> p -> s -- a to equivalent class [=a=]
class Partition a s p => FinitePartition a s p where
    -- all equivalent classes
    to_parts :: p -> [s]
    to_parts = to_parts_over universal
    -- all (not . null) result
    -- disjoint_sets result
    -- unions result = s
    to_parts_over :: s -> p -> [s]
    to_parts_over s = filter (not . null) . map (s /\) . to_parts
    -- nonnull_disjoint_sets, unions [s] = universal
    from_parts :: [s] -> p
    finer :: p -> p -> p -- intersection
    finer q = from_parts . concat . map (flip to_parts_over q) . to_parts


















---------------------------------------------------
--- std

------------ List
-- but a = a++[] fail null
instance OpMap a [a] where
    omap = map
instance Empty a [a] where
   empty = []
instance Null a [a] where
   null = P.null
instance Container a [a] where
instance Eq a => Member a [a] where
    member = P.elem
instance Buffer a [a]
instance AnyFiniteSize a [a]
instance Singleton a [a]
instance UnSingleton a [a] where
    safe_unsingleton [a] = Just a
    safe_unsingleton _ = Nothing
instance Pop a [a] where
    pops (h:t) = (h, t) : pops t
    pops [] = []
instance Insert a [a] where
    insert = (:)
instance CountableContainer a [a] where
instance Iterable a [a] where
    iter = id
instance AnyElem a [a] where
    any_elem = safe_head

instance UnsafeSized a [a] where
    unsafe_len = toInteger . P.length
instance IterLe a [a]
instance LenLe a [a] where
    len_if_le n ls = let (init, tail) = splitAt (fromInteger n) ls in
        justif (n >= 0 && null tail) (length init)
    {-
    len_le n _ | n < 0 = False
    len_le _ [] = True
    len_le n (a:ls) = len_if_le (n-1) ls
    -}
instance OpPartition a [a] where
    partition = L.partition


instance Seq a [a] where
instance SemiGroup [a] where
    (><) = (++)
instance DynSeq a [a] where
instance ListLike a [a] where
    viewl (a:ls) = a :< ls
    viewl _ = EmptyL
    (<|) = (:)




-- maybe infinite long...
instance MapView Integer a [a] where
    -- lookup i ls = safe_init_eq_iter (i+1) ls >>= P.last
    lookup i = L.lookup i . zip [0..i]
    type MapView2KeySet [a] = MapView2KeySetWrapper [a]
    key_set = box



------- [a] as KeySet
newtype MapView2KeySetWrapper a = MapView2KeySetWrapper a
instance Boxed a (MapView2KeySetWrapper a) where
    box = MapView2KeySetWrapper
    unbox (MapView2KeySetWrapper a) = a
instance Set Integer (MapView2KeySetWrapper [a]) where
instance Member Integer (MapView2KeySetWrapper [a]) where
    member i = not . len_le i . unbox
instance Empty Integer (MapView2KeySetWrapper [a]) where
    empty = box []
instance Null Integer (MapView2KeySetWrapper [a]) where
    null = null . unbox
instance Container Integer (MapView2KeySetWrapper [a]) where
instance Eq (MapView2KeySetWrapper [a]) where
    -- one may be infinite long
    -- (==) = liftB2_ $ lift2 length (==)
    (==) = liftB2_ eqLen
instance Ord (MapView2KeySetWrapper [a]) where
    -- (<) = liftB2_ $ lift2 length (<)
    (<) = liftB2_ ltLen
instance OrdIsTotalSetOrd (MapView2KeySetWrapper [a]) where
instance EqIsSetEq (MapView2KeySetWrapper [a]) where















------------- Integral as Set
is_neg :: Integral n => n -> Bool
is_neg = (< 0) . toInteger . signum
newtype IntegralAsSet n = IntegralAsSet { unIntegralAsSet :: n }
    deriving (Show, Read)

__size :: Integral n => IntegralAsSet n -> Integer
__size (IntegralAsSet n) = if is_neg n then 0 else toInteger n
instance Integral n => Eq (IntegralAsSet n) where
    (==) = lift2 __size (==)
instance Integral n => Ord (IntegralAsSet n) where
    (<) = lift2 __size (<)
instance Boxed n (IntegralAsSet n) where
    box = IntegralAsSet
    unbox = unIntegralAsSet
{-
instance Integral a => MapView Integer a (IntegralAsSet a) where
    -- lookup i ls = safe_init_eq_iter (i+1) ls >>= P.last
    lookup i a = justif (member i a) $ fromInteger i
    type MapView2KeySet (IntegralAsSet a) = IntegralAsSet a
    key_set = id
-}
instance Integral n => Set Integer (IntegralAsSet n) where
instance Integral n => Member Integer (IntegralAsSet n) where
    member i a = 0 <= i && i < __size a
instance Integral n => Empty Integer (IntegralAsSet n) where
    empty = box 0
instance Integral n => Null Integer (IntegralAsSet n) where
    null = (== 0) . __size
instance Integral n => Container Integer (IntegralAsSet n) where
instance Integral n => OrdIsTotalSetOrd (IntegralAsSet n) where
instance Integral n => EqIsSetEq (IntegralAsSet n) where




instance Integral n => Pop Integer (IntegralAsSet n) where
    pops bn = zip ls $ map (box . fromInteger) ls where
        ls = iter bn
instance Integral n => CountableContainer Integer (IntegralAsSet n) where
instance Integral n => FiniteContainer Integer (IntegralAsSet n) where
instance Integral n => Iterable Integer (IntegralAsSet n) where
    iter bn = ls where
        n = __size bn
        ls = [n-1, n-2..0] -- overflow??
        iter'   | n <= 0 = []
                | n == 1 = [0]
                | otherwise = ls
instance Integral n => AnyElem Integer (IntegralAsSet n) where
    any_elem n = let s = __size n in justif (s>0) $ s-1

instance Integral n => UnsafeSized Integer (IntegralAsSet n) where
instance Integral n => Sized Integer (IntegralAsSet n) where
    len = __size
instance Integral n => IterLe Integer (IntegralAsSet n) where
instance Integral n => LenLe Integer (IntegralAsSet n) where
-- instance Integral n => Seq Integer (IntegralAsSet n) where















------------ Data.Seq
instance Container a (Seq.Seq a) where
instance OpMap a (Seq.Seq a) where
    omap = fmap

instance Empty a (Seq.Seq a) where
   empty = Seq.empty
instance Null a (Seq.Seq a) where
   null = Seq.null
instance Eq a => Member a (Seq.Seq a) where
    member a = test . Seq.elemIndexL a
instance Buffer a (Seq.Seq a) where
instance AnyFiniteSize a (Seq.Seq a) where
instance Singleton a (Seq.Seq a) where
instance UnSingleton a (Seq.Seq a) where
    safe_unsingleton seq | length seq == 1 = safe_head seq
                         | otherwise = Nothing
instance Pop a (Seq.Seq a) where
    pops ls = case viewl ls of
        h :< t -> (h, t) : pops t
        _ -> []
instance Insert a (Seq.Seq a) where
    insert = (<|)
instance CountableContainer a (Seq.Seq a) where
instance FiniteContainer a (Seq.Seq a) where
instance Iterable a (Seq.Seq a) where
    iter ls = case viewl ls of
        h :< t -> h : iter t
        _ -> []
instance AnyElem a (Seq.Seq a) where
    any_elem = safe_head

instance UnsafeSized a (Seq.Seq a) where
instance Sized a (Seq.Seq a) where
    len = toInteger . Seq.length
instance IterLe a (Seq.Seq a) where
instance LenLe a (Seq.Seq a) where

instance OpPartition a (Seq.Seq a) where
    partition = Seq.partition


instance Seq a (Seq.Seq a) where
instance SemiGroup (Seq.Seq a) where
    (><) = (Seq.><)
instance DynSeq a (Seq.Seq a) where
seq_viewl2viewl v = case v of
    a Seq.:< ls -> a :< ls
    _ -> EmptyL
instance ListLike a (Seq.Seq a) where
    viewl = seq_viewl2viewl . Seq.viewl
    (<|) = (Seq.<|)




-- maybe infinite long...
instance MapView Integer a (Seq.Seq a) where
    -- lookup i ls = safe_init_eq_iter (i+1) ls >>= P.last
    lookup i ls = justif (i < len ls) $ Seq.index ls $ fromInteger i
    type MapView2KeySet (Seq.Seq a) = IntegralAsSet Integer
    key_set = box . len

-- -}










---------  S.Set
instance Container a (S.Set a)
instance Ord a => Valid (S.Set a) where
    valid = S.valid
instance Null a (S.Set a) where
    null = S.null
instance Empty a (S.Set a) where
    empty = S.empty
instance Ord a => Member a (S.Set a) where
    member = S.member
instance Ord a => Remove a (S.Set a) where
    remove a s = if S.member a s
                    then Just ([a], S.delete a s)
                    else Nothing
instance Ord a => Insert a (S.Set a) where
    insert = S.insert

instance Eq a => EqIsSetEq (S.Set a)
--instance Eq a => SetEq (S.Set a) where
--    (|==|) = (==)
instance Ord a => SetOrd (S.Set a) where
    (|<=|) = S.isSubsetOf
    (|<|) = S.isProperSubsetOf
instance Ord a => Set a (S.Set a) where



---------  Bool
instance SetDifference Bool where
    -- False = {}, True = {()}
    -- difference a b = a && not b
    difference a b = not b && a
    -- difference _ True = False
    -- difference x _ = x
instance SetIntersection Bool where
    intersection = (&&)
instance SetUnion Bool where
    union = (||)
instance SetSep Bool
instance SetOp Bool
instance Universal Bool where
    universal = True
instance Complement Bool where
    complement = not


--
instance Num Bool where
    fromInteger i = (i .&. 1) == 1
    a + b = (a && not b) || (not a && b) -- xor
    (-) = (+)
    (*) = (&&)
    negate = id
    abs = id
    signum = id
instance Real Bool where
    toRational = toRational . toInteger
instance Integral Bool where
    toInteger b = if b then 1 else 0
    quotRem a b = map2 fromInteger $ lift2 toInteger quotRem a b
instance Container () Bool
instance OpMap () Bool where
    omap = const id
instance Null () Bool where
    null = not
    not_null = id
instance Empty () Bool where
    empty = False
instance Member () Bool where
    member _ = id
instance AnyElem () Bool where
    any_elem c = justif c () -- if c then Just () else Nothing
instance CountableContainer () Bool where
instance FiniteContainer () Bool where
instance Iterable () Bool where
    iter c = if c then [()] else []
instance IterLe () Bool where
instance LenLe () Bool where
instance UnsafeSized () Bool where
instance Sized () Bool where
    len = toInteger
instance Insert () Bool where
    insert _ _ = True
    extend n it c = if null $ take n it -- take (fromInteger n) (iter it)
                    then c else True
instance Pop () Bool where
    pops True = [((), False)]
    pops _ = []
instance Remove () Bool where
    remove _ c = justif c ([()], False)
    discard _ c = justif c False
    delete _ _ = False
instance Singleton () Bool where
    singleton = const True
instance AnyFiniteSize () Bool where
instance SemiGroup Bool where
instance Monoid Bool where
    mempty = False
    mappend = (||)
instance Buffer () Bool where
instance Set () Bool where
instance UniversalSet () Bool where
instance DynSet () Bool where
instance DynUniversalSet () Bool where

