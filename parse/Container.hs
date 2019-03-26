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

module Container where
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import qualified Data.List as L
import qualified Data.Map as M -- (Map)
import qualified Data.Set as S -- (Set)
import qualified Prelude as P
import qualified Data.Bits as B
import Data.Monoid
import Prelude hiding (lookup, null, take, length, fromInteger, fst, snd)
-- import Prelude (Eq, Ord, Num, Integral, Integer, Real)
import Data.Bits ((.&.))
import SeedUtils -- hiding (safe_head)
import Ordering
import Boxed
import qualified Data.Sequence as Seq

import Container_class_base
import Container_class_Set
import Group


--------------   EqFst

class Pair p where
    from_pair :: (k, v) -> p k v
    to_pair :: p k v -> (k, v)
    fst :: p k v -> k
    fst = P.fst . to_pair
    snd :: p k v -> v
    snd = P.snd . to_pair
instance Pair (,) where
    from_pair = id
    to_pair = id
{-
instance Pair p => Boxed (a,b) (p a b) where
    box = from_pair
    unbox = to_pair
-}
class (Pair p) => EqFst p where
    fst_eq :: p k v1 -> p k v2 -> Bool
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


























------------------ Seq
class (Iterable a c, MapView Integer a c) --, Map (,) Integer a c)
    => Seq a c where
    safe_head :: c -> Maybe a
    safe_head = safe_head_iter

class (Seq a c, SemiGroup c) => DynSeq a c where
    -- not OpEmpty/OpSingleton/OpInsert/OpPop
    -- e.g. seq with size of k*n, k > 0
    -- if basic seq sizes are {3, 7}
    --    then all possible sizes are 3*n+7*m
    --    {3, 6, 7, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,...}
    --    N - {0,1,2,4,5,8,11}
    -- (><) :: c -> c -> c -- ++
    -- (><) = mappend not Monoid, except mempty
    -- semigroup

infixr 5 <|, :<

data ViewL a c = EmptyL | a :< c

class (DynSeq a c, Buffer a c)
    => ListLike a c where
    viewl :: c -> ViewL a c
    (<|) :: a -> c -> c




import Container_class_Map
module Container_class_Map where

-- No (Eq k); we can customize by p
class (OpMember (p k v) c, Pair p, EqFst p, MapView k v c)
    => Map p k v c where
    -- p - pair
    -- (k, undefined) in dict
    -- (k1, v) == (k2, u) <==> k1 == k2

    -- Note: member :: p k v -> c -> Bool
    --       insert :: p k v -> c -> c
    --       member = has_key . fst
    --
    -- OpMember k c???????
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


class (Map p k v c, SetOp c, OpInsert (p k v) c)
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

instance Seq a [a] where

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
instance OpMember Integer (MapView2KeySetWrapper [a]) where
    member i = not . len_le i . unbox
instance OpEmpty Integer (MapView2KeySetWrapper [a]) where
    empty = box []
instance OpNull Integer (MapView2KeySetWrapper [a]) where
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
instance Integral n => OpMember Integer (IntegralAsSet n) where
    member i a = 0 <= i && i < __size a
instance Integral n => OpEmpty Integer (IntegralAsSet n) where
    empty = box 0
instance Integral n => OpNull Integer (IntegralAsSet n) where
    null = (== 0) . __size
instance Integral n => Container Integer (IntegralAsSet n) where
instance Integral n => OrdIsTotalSetOrd (IntegralAsSet n) where
instance Integral n => EqIsSetEq (IntegralAsSet n) where




instance Integral n => OpPop Integer (IntegralAsSet n) where
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
instance Integral n => OpAnyElem Integer (IntegralAsSet n) where
    any_elem n = let s = __size n in justif (s>0) $ s-1

instance Integral n => UnsafeSized Integer (IntegralAsSet n) where
instance Integral n => Sized Integer (IntegralAsSet n) where
    len = __size
instance Integral n => OpIterLe Integer (IntegralAsSet n) where
instance Integral n => OpLenLe Integer (IntegralAsSet n) where
-- instance Integral n => Seq Integer (IntegralAsSet n) where















------------ Data.Seq
instance Container a (Seq.Seq a) where
instance OpOmap a (Seq.Seq a) where
    omap = fmap

instance OpEmpty a (Seq.Seq a) where
   empty = Seq.empty
instance OpNull a (Seq.Seq a) where
   null = Seq.null
instance Eq a => OpMember a (Seq.Seq a) where
    member a = test . Seq.elemIndexL a
instance Buffer a (Seq.Seq a) where
instance AnyFiniteSize a (Seq.Seq a) where
instance OpSingleton a (Seq.Seq a) where
instance OpUnSingleton a (Seq.Seq a) where
    safe_unsingleton seq | length seq == 1 = safe_head seq
                         | otherwise = Nothing
instance OpPop a (Seq.Seq a) where
    pops ls = case viewl ls of
        h :< t -> (h, t) : pops t
        _ -> []
instance OpInsert a (Seq.Seq a) where
    insert = (<|)
instance CountableContainer a (Seq.Seq a) where
instance FiniteContainer a (Seq.Seq a) where
instance Iterable a (Seq.Seq a) where
    iter ls = case viewl ls of
        h :< t -> h : iter t
        _ -> []
instance OpAnyElem a (Seq.Seq a) where
    any_elem = safe_head

instance UnsafeSized a (Seq.Seq a) where
instance Sized a (Seq.Seq a) where
    len = toInteger . Seq.length
instance OpIterLe a (Seq.Seq a) where
instance OpLenLe a (Seq.Seq a) where

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
instance Ord a => OpValid (S.Set a) where
    valid = S.valid
instance OpNull a (S.Set a) where
    null = S.null
instance OpEmpty a (S.Set a) where
    empty = S.empty
instance Ord a => OpMember a (S.Set a) where
    member = S.member
instance Ord a => OpRemove a (S.Set a) where
    remove a s = if S.member a s
                    then Just ([a], S.delete a s)
                    else Nothing
instance Ord a => OpInsert a (S.Set a) where
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
instance OpOmap () Bool where
    omap = const id
instance OpNull () Bool where
    null = not
    not_null = id
instance OpEmpty () Bool where
    empty = False
instance OpMember () Bool where
    member _ = id
instance OpAnyElem () Bool where
    any_elem c = justif c () -- if c then Just () else Nothing
instance CountableContainer () Bool where
instance FiniteContainer () Bool where
instance Iterable () Bool where
    iter c = if c then [()] else []
instance OpIterLe () Bool where
instance OpLenLe () Bool where
instance UnsafeSized () Bool where
instance Sized () Bool where
    len = toInteger
instance OpInsert () Bool where
    insert _ _ = True
    extend n it c = if null $ take n it -- take (fromInteger n) (iter it)
                    then c else True
instance OpPop () Bool where
    pops True = [((), False)]
    pops _ = []
instance OpRemove () Bool where
    remove _ c = justif c ([()], False)
    discard _ c = justif c False
    delete _ _ = False
instance OpSingleton () Bool where
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

