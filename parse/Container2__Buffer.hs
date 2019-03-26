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
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}



module Container2__Buffer
where

import Container2__base as CB
import Container2__Set
import Container2__Map
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
import SeedUtils__TH (def__instances)




class (Container a, Boolean (FullExResult a)) => OpFull a where
    -- like null
    -- insert will take no effect or discard other elements
    full :: a -> Bool
    type FullExResult a :: *
    full_ex :: a -> FullExResult a


    default full_ex :: OpDynTheOnlyValue (FullExResult a)
        => a -> FullExResult a
    full_ex _ = the_only_value
    full = explain . full_ex


--class Container a => OpDyn a where
class Container a => OpDynEmpty a where
    empty :: a
class Container a => OpDynInsert a where
    insert :: Element a -> a -> a
    extend :: (Iterable b, Element a ~ Element b) => b -> a -> a
    extend b a = foldr insert a $ iter b

data PopData r a c -- pops :: c -> PopData a r -- r may not be c
    = PopData a c (PopData r a c)
    | EmptyPopData
    | RemainPopData r
data PopList a c
    = PopList a c (PopList a c)
    | EmptyPopList

type Pairs a b = [(a, b)]
type FPairs a b = FiniteList (a, b)
type Triple = (,,)
pairs2popdata :: Pairs a c -> PopData r a c -- r may not be c
pairs2popdata ((a,c):ls) = PopData a c $ pairs2popdata ls
pairs2popdata _ = EmptyPopData
poplist2popdata :: PopList a c -> PopData r a c -- r may not be c
poplist2popdata (PopList a c ls) = PopData a c $ poplist2popdata ls
poplist2popdata _ = EmptyPopData
pairs2poplist :: Pairs a c -> PopList a c
pairs2poplist ((a,c):ls) = PopList a c $ pairs2poplist ls
pairs2poplist _ = EmptyPopList
poplist2pairs :: PopList a c -> Pairs a c
poplist2pairs (PopList a c ls) = (a,c) : poplist2pairs ls
poplist2pairs _ = []

let a = f "a"
    c = f "c"
    r = f "r"
    f = varT . mkName
    _PopData_r_a_c = [t| PopData $r $a $c |]
    _PopList_a_c = [t| PopList $a $c |]
    _Pairs_a_c = [t| Pairs $a $c |]
    _Maybe_a_c = [t| Maybe ($a, $c) |]
    _FPairs_a_c = [t| FPairs $a $c |]
    _Value_EmptySet_a_c = [t| Value_EmptySet ($a, $c) |]
    (~$) = flip ($)
    infixl 0 ~$
 in [ instExplain _PopData_r_a_c _PopList_a_c
        [e| poplist2popdata |]
    , instExplain _PopData_r_a_c _Pairs_a_c
        [e| pairs2popdata |]
    , instExplain _PopList_a_c _Pairs_a_c
        [e| pairs2poplist |]
    , instExplain _Pairs_a_c _PopList_a_c
        [e| poplist2pairs |]
    , instExplain _PopData_r_a_c _Maybe_a_c
        [e| maybe EmptyPopData (\(a,c)->PopData a c EmptyPopData) |]
    , instExplain _PopList_a_c _Maybe_a_c
        [e| maybe EmptyPopList (\(a,c)->PopList a c EmptyPopList) |]
    , instExplain _PopData_r_a_c _FPairs_a_c
        [e| explain . unFiniteList |]
    , instExplain _PopList_a_c _FPairs_a_c
        [e| explain . unFiniteList |]
    , instExplain _PopData_r_a_c _Value_EmptySet_a_c
        [e| const EmptyPopData |]
    , instExplain _PopList_a_c _Value_EmptySet_a_c
        [e| const EmptyPopList |]
    ] ~$ fmap concat . mapM id

instance OpSafeFrom (PopData r a c) (PopList a c) where
    safe_from (PopData a c ls) = safe_from ls >>= return . PopList a c
    safe_from EmptyPopData = return EmptyPopList
    safe_from _ = Nothing
    unsafe_from (PopData a c ls) = PopList a c $ unsafe_from ls
    unsafe_from EmptyPopData = EmptyPopList
    unsafe_from _ = error "unsafe_from (RemainPopData _) -> PopList"
instance OpSafeFrom (PopData (Value_EmptySet a) a c) (PopList a c) where
instance OpFrom (PopData (Value_EmptySet a) a c) (PopList a c) where
    from = popData_as_popList

ignoreRemain :: PopData r a c -> PopList a c
ignoreRemain (PopData a c ls) = PopList a c $ ignoreRemain ls
ignoreRemain _ = EmptyPopList
popData_as_popList :: Empty r => PopData r a c -> PopList a c
popData_as_popList = ignoreRemain



class   ( Container a
        --, Iterable (PopsExResult a)
        --, (Element a, a) ~ Element (PopsExResult a)
        , Explain (PopData (PopsEx_RemainType a) (Element a) a)
                  (PopsExResult a)
        , ContainerEx (Element a) (PopsEx_RemainType a)
        ) => OpDynPopBase a where
    -- not Iterable/Countable!
    -- pop Nothing ==xx==>> null
    type PopsExResult a :: *
    type PopsEx_RemainType a :: *
    pops_ex :: a -> PopsExResult a
    std_pops_ex :: a -> (PopData (PopsEx_RemainType a) (Element a) a)
    std_pops_ex = explain . pops_ex

class   ( OpDynPopBase a, Iterable a, OpDynEmpty a
        , Empty (PopsEx_RemainType a)
        , Explain (PopList (Element a) a) (PopsExResult a)
        , Explain (Pairs (Element a) a) (PopsExResult a)
        )
    => OpDynPop a where
    -- pop Nothing <==> null
    std_popls_ex :: a -> PopList (Element a) a
    popls :: a -> [(Element a, a)]
    pop :: a -> Maybe (Element a, a)

    std_popls_ex = explain . pops_ex
    popls = explain . pops_ex
    pop = listToMaybe . popls



class Container a => OpDynAppend a where
    append, (><) :: a -> a -> a
    (><) = append
    default append :: (Iterable a, OpDynInsert a) => a -> a -> a
    append = extend
infixr 5 ><
class Container a => OpDynSingleton a where
    singleton :: Element a -> a
def__instances [d|
    class (OpFull a, Iterable a) => BufferR a
    class   (OpDynEmpty a, OpDynSingleton a, OpDynInsert a, OpDynAppend a
            --, OpFrom (FiniteList (Element a)) a
            )
        => BufferW a
        -- Monoid
    class (BufferW a, BufferR a, OpDynPop a) => Buffer a
    |]







--------------------------------------
instance OpFull (Maybe a) where
    type FullExResult (Maybe a) = Bool
    full_ex = P.not . CB.null
    full = full_ex

instance OpDynEmpty (Maybe a) where
    empty = Nothing
instance OpDynInsert (Maybe a) where
    -- as (First a), leftmost, to support lazy-eval
    insert a _ = Just a
instance OpDynPopBase (Maybe a) where
    type PopsExResult (Maybe a) = Maybe (a, Maybe a)
    type PopsEx_RemainType (Maybe a) = Value_EmptySet a
    pops_ex (Just a) = Just (a, Nothing)
    pops_ex Nothing = Nothing
instance OpDynPop (Maybe a) where
    popls (Just a) = [(a, Nothing)]
    popls Nothing = []
instance OpDynSingleton (Maybe a) where
    singleton = Just
instance OpDynAppend (Maybe a) where
    -- First a
    append x@(Just _) _ = x
    append _ x = x








instance OpFull ([] a) where
    type FullExResult ([] a) = Value_False
    full _ = False

instance OpDynEmpty ([] a) where
    empty = []
instance OpDynInsert ([] a) where
    insert = (:)
instance OpDynPopBase ([] a) where
    type PopsExResult ([] a) = [(a, [] a)]
    type PopsEx_RemainType ([] a) = Value_EmptySet a
    pops_ex (a:ls) = (a, ls) : pops_ex ls
    pops_ex [] = []
instance OpDynPop ([] a) where
    popls = pops_ex
instance OpDynSingleton [a] where
    singleton e = [e]
instance OpDynAppend [a] where
    append = (++)




instance OpFull (Value_EmptySet a) where
    type FullExResult (Value_EmptySet a) = Value_True
    full _ = True

instance OpDynEmpty (Value_EmptySet a) where
    empty = the_only_value
instance OpDynInsert (Value_EmptySet a) where
    insert _ _ = the_only_value
instance OpDynPopBase (Value_EmptySet a) where
    type PopsExResult (Value_EmptySet a) = Value_EmptySet (a, Value_EmptySet a)
    type PopsEx_RemainType (Value_EmptySet a) = Value_EmptySet a
    pops_ex _ = the_only_value
instance OpDynPop (Value_EmptySet a) where
    popls _ = []
    pop _ = Nothing
instance OpDynAppend (Value_EmptySet a) where
    append _ = id








instance OpFull (Value_Just a) where
    type FullExResult (Value_Just a) = Value_True
    full _ = True

instance OpDynInsert (Value_Just a) where
    insert e _ = Value_Just e
instance OpDynSingleton (Value_Just a) where
    singleton = Value_Just
instance OpDynAppend (Value_Just a) where
    append a _ = a



instance OpFull (ListGt0 a) where
    type FullExResult (ListGt0 a) = Value_False
    full _ = False

instance OpDynInsert (ListGt0 a) where
    insert e ls = ListGt0 (e, iter ls)
instance OpDynPopBase (ListGt0 a) where
    type PopsExResult (ListGt0 a) = PopData (Value_Just a) a (ListGt0 a)
    type PopsEx_RemainType (ListGt0 a) = Value_Just a
    pops_ex (ListGt0 (h, a:ls)) =
        let ls' = ListGt0 (a,ls) in PopData h ls' $ pops_ex ls'
    pops_ex (ListGt0 (h, [])) = RemainPopData (Value_Just h)
instance OpDynSingleton (ListGt0 a) where
    singleton e = ListGt0 (e, [])
instance OpDynAppend (ListGt0 a) where
    append (ListGt0 (h, t)) b = ListGt0 (h, t >< iter b)







instance OpFull (FiniteList a) where
    type FullExResult (FiniteList a) = Value_False
    full _ = False

instance OpDynEmpty (FiniteList a) where
    empty = FiniteList []
instance OpDynInsert (FiniteList a) where
    insert a (FiniteList ls) = FiniteList (a:ls)
instance OpDynPopBase (FiniteList a) where
    type PopsExResult (FiniteList a) = FiniteList (a, FiniteList a)
    type PopsEx_RemainType (FiniteList a) = Value_EmptySet a
    pops_ex (FiniteList ls) = FiniteList $ map f $ pops_ex ls where
        f (a, ls) = (a, FiniteList ls)
instance OpDynPop (FiniteList a) where
    popls = unFiniteList . pops_ex
instance OpDynSingleton (FiniteList a) where
    singleton e = FiniteList [e]
instance OpDynAppend (FiniteList a) where
    FiniteList a `append` FiniteList b = FiniteList (a++b)







instance OpFull (D_Seq a) where
    type FullExResult (D_Seq a) = Value_False
    full _ = False

instance OpDynEmpty (D_Seq a) where
    empty = Seq.empty
instance OpDynInsert (D_Seq a) where
    insert = (Seq.<|)
instance OpDynPopBase (D_Seq a) where
    type PopsExResult (D_Seq a) = FiniteList (a, D_Seq a)
    type PopsEx_RemainType (D_Seq a) = Value_EmptySet a
    pops_ex ls = FiniteList $ f ls where
        f ls = case Seq.viewl ls of
            h Seq.:< t -> (h, t) : f t
            _ -> []
instance OpDynPop (D_Seq a) where
    popls = unFiniteList . pops_ex
instance OpDynSingleton (D_Seq a) where
    singleton = Seq.singleton
instance OpDynAppend (D_Seq a) where
    append = (Seq.><)









instance OpFull (D_Set a) where
    type FullExResult (D_Set a) = Value_False
    full _ = False

instance OpDynEmpty (D_Set a) where
    empty = S.empty
instance Ord a => OpDynInsert (D_Set a) where
    insert = (S.insert)
instance OpDynPopBase (D_Set a) where
    type PopsExResult (D_Set a) = FiniteList (a, D_Set a)
    type PopsEx_RemainType (D_Set a) = Value_EmptySet a
    pops_ex s = FiniteList $ f s where
        f s = case S.minView s of
            Just x@(h, t) -> x : f t
            _ -> []
instance OpDynPop (D_Set a) where
    popls = unFiniteList . pops_ex
instance OpDynSingleton (D_Set a) where
    singleton = S.singleton
instance Ord a => OpDynAppend (D_Set a) where
    append = (S.union)





instance OpFull D_IntSet where
    type FullExResult D_IntSet = Value_False
    full _ = False

instance OpDynEmpty D_IntSet where
    empty = IS.empty
instance OpDynInsert D_IntSet where
    insert = (IS.insert)
instance OpDynPopBase D_IntSet where
    type PopsExResult D_IntSet = FiniteList (Int, D_IntSet)
    type PopsEx_RemainType D_IntSet = Value_EmptySet Int
    pops_ex s = FiniteList $ f s where
        f s = case IS.minView s of
            Just x@(h, t) -> x : f t
            _ -> []
instance OpDynPop D_IntSet where
    popls = unFiniteList . pops_ex
instance OpDynSingleton D_IntSet where
    singleton = IS.singleton
instance OpDynAppend D_IntSet where
    append = (IS.union)








instance OpFull (AMap k v) where
    type FullExResult (AMap k v) = Value_False
    full _ = False

instance OpDynEmpty (AMap k v) where
    empty = AMap (FiniteList [])
instance OpDynInsert (AMap k v) where
    insert a (AMap ls) = AMap (insert a ls)
instance OpDynPopBase (AMap k v) where
    type PopsExResult (AMap k v) = FiniteList ((k,v), AMap k v)
    type PopsEx_RemainType (AMap k v) = Value_EmptySet (k,v)
    pops_ex (AMap (FiniteList ls)) = FiniteList $ map f $ pops_ex ls where
        f (a, ls) = (a, AMap (FiniteList ls))
instance OpDynPop (AMap k v) where
    popls = unFiniteList . pops_ex
instance OpDynSingleton (AMap k v) where
    singleton = AMap . singleton
instance OpDynAppend (AMap k v) where
    AMap a `append` AMap b = AMap (a >< b)









instance Container (D_Map k v) where
    type Element (D_Map k v) = (k,v)
instance CountableContainer (D_Map k v) where
instance FiniteContainer (D_Map k v) where

instance OpNull (D_Map k v) where
    type NullExResult (D_Map k v) = Bool
    null_ex = M.null
    null = null_ex

instance OpUnsafeLen (D_Map k v)
instance OpLenIfLe (D_Map k v)
instance Sized (D_Map k v) where
    type LenExResult (D_Map k v) = UInt
    len_ex = unsafe_from . P.toInteger . M.size
    len = len_ex
instance OpAnyElem (D_Map k v) where
    type AnyElemExResult (D_Map k v) = Maybe (k,v)
instance OpIterLe (D_Map k v) where
instance Iterable (D_Map k v) where
    iter = M.toList
instance OpIterAsSeq (D_Map k v) where
instance OpFull (D_Map k v) where
    type FullExResult (D_Map k v) = Value_False
    full _ = False

instance OpDynEmpty (D_Map k v) where
    empty = M.empty
instance Ord k => OpDynInsert (D_Map k v) where
    insert = uncurry M.insert
instance OpDynPopBase (D_Map k v) where
    type PopsExResult (D_Map k v) = FiniteList ((k,v), D_Map k v)
    type PopsEx_RemainType (D_Map k v) = Value_EmptySet (k,v)
    pops_ex m = FiniteList $ f m where
        f m = case M.minViewWithKey m of
            Just x@(kv, m') -> x : f m'
            _ -> []
instance OpDynPop (D_Map k v) where
    popls = unFiniteList . pops_ex
instance OpDynSingleton (D_Map k v) where
    singleton = uncurry M.singleton
instance Ord k => OpDynAppend (D_Map k v) where
    append = M.union








instance Container (D_IntMap v) where
    type Element (D_IntMap v) = (Int,v)
instance CountableContainer (D_IntMap v) where
instance FiniteContainer (D_IntMap v) where

instance OpNull (D_IntMap v) where
    type NullExResult (D_IntMap v) = Bool
    null_ex = IM.null
    null = null_ex

instance OpUnsafeLen (D_IntMap v)
instance OpLenIfLe (D_IntMap v)
instance Sized (D_IntMap v) where
    type LenExResult (D_IntMap v) = UInt
    len_ex = unsafe_from . P.toInteger . IM.size
    len = len_ex
instance OpAnyElem (D_IntMap v) where
    type AnyElemExResult (D_IntMap v) = Maybe (Int,v)
instance OpIterLe (D_IntMap v) where
instance Iterable (D_IntMap v) where
    iter = IM.toList
instance OpIterAsSeq (D_IntMap v) where
instance OpFull (D_IntMap v) where
    type FullExResult (D_IntMap v) = Value_False
    full _ = False

instance OpDynEmpty (D_IntMap v) where
    empty = IM.empty
instance OpDynInsert (D_IntMap v) where
    insert = uncurry IM.insert
instance OpDynPopBase (D_IntMap v) where
    type PopsExResult (D_IntMap v) = FiniteList ((Int,v), D_IntMap v)
    type PopsEx_RemainType (D_IntMap v) = Value_EmptySet (Int,v)
    pops_ex m = FiniteList $ f m where
        f m = case IM.minViewWithKey m of
            Just x@(kv, m') -> x : f m'
            _ -> []
instance OpDynPop (D_IntMap v) where
    popls = unFiniteList . pops_ex
instance OpDynSingleton (D_IntMap v) where
    singleton = uncurry IM.singleton
instance OpDynAppend (D_IntMap v) where
    append = IM.union


















































--}
--}
--}
--}
