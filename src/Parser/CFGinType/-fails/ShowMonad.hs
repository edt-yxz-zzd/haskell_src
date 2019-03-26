{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Parser.ShowMonad
    ( S(), SA(), A()
    , putS, runS
    , putA, runA
    )
where

import Seed.AList -- hiding (AList, alist2list, list2alist)
import Seed.MonadOps
import Seed.ListOps -- shortest_firstL
import Seed.ListOps.MergeLists
import IntDefs.IntDefs (UInt)

import Seed.By
import Seed.Boxed
import Data.Semigroup
import Explain.ExplainBase
import Data.Semigroup
import ADT.ModuloBy
import Seed.ProxyOps
import ADT.MonoidBy
import Control.Arrow


{-
type AList = []
alist2list = id
list2alist = id
-}

data S by ls len als a = S [(len, [(a, als)])] -- sorted by len

type C2 len_by len als = (IModuloBy len_by len, Monoid als)
{-
add :: (e ~ (len, [(a, als)]), C2 len_by len als)
    => proxy (len_by, ) -> e -> e -> e
add (len1, ls1) (len2, ls2) = (len1 <> len2, sums) where
    sums = do
-}

type SA by t = S by [t] (UInt) (AList t)

type C by ls len als
    = (OpToModulo by ls, ModuloType by ls ~ len, View ls als
        , Monoid als -- , IMonoidBy (ModuloBy by ls) len
        , IMonoidBy (OpToModulo2SemigroupBy by ls) ls
        )
putS :: (C by ls len als)
    => ls -> S by ls len als ()
putS ls = r where
    r = S [(len, [((), als)])]
    len = to_moduloBy p ls
    als = make ls
    gp :: proxyS by ls len als a -> Proxy by
    gp _ = Proxy
    p = gp r
    -- p = Proxy :: Proxy by

type A by ls len als = Kleisli (S by ls len als)
putA :: (C by ls len als, A by ls len als ~ arr)
    => ls -> arr i ()
putA ls = Kleisli $ \_ -> putS ls

runS :: C by ls len als => S by ls len als a -> [ls]
runA :: C by ls len als => A by ls len als i o -> i -> [ls]
runA arr = runS . runKleisli arr
runS (S pairs) = concat $ map f pairs where
    f (len, pairs) = map explain $ map snd pairs

s :: SA ByLen Char ()
s = putS " "

instance C by ls len als => Functor (S by ls len als) where
    fmap = fmapM2F
instance C by ls len als => Applicative (S by ls len als) where
    pure = return
    (<*>) = (<***>)
instance C by ls len als => Monad (S by ls len als) where
    return a = S [(zero_len, [(a, mempty)])] where
        empty_ls :: ls
        empty_ls = memptyBy (Proxy :: Proxy (OpToModulo2SemigroupBy by ls))
        zero_len :: ModuloType by ls
        zero_len = to_moduloBy (Proxy :: Proxy by) empty_ls
    S ls >>= a2S = S r where
        mul = mulBy (Proxy :: Proxy (ModuloBy by ls))
        pairss = do
            (len1, a_als_pairs) <- ls
            (a, als) <- a_als_pairs
            let S ls' = a2S a
            return $ do
                (len2, b_als_pairs) <- ls'
                let len = mul len1 len2
                (b, als') <- b_als_pairs
                let ls'' = als `mappend` als'
                return (len, (b, ls''))
        to_ord = fst
        pairs = merge_sorted_sorted_listss to_ord pairss
        r = group_fst pairs
    {-
    S ls >>= a2S = S pairs where
        pairs = do
            (a, als) <- ls
            let S ls' = a2S a
            (b, als') <- ls'
            return (b, als <> als')
    -}

instance C by ls len als => MonadPlus (S by ls len als) where
    mzero = S []
    S lhs `mplus` S rhs = r where
        cmp = fst_op (<=)
        ls = merge_two_sorted_lists cmp lhs rhs
        -- to concat ls with eq ord
        ls' = map (fmap concat) $ group_fst ls
        r = S ls'
    {-
    S lhs' `mplus` S rhs' = r where
        to_ls (a, als) = (a, alist2list als)
        to_als (a, ls) = (a, list2alist ls)
        len_eq n (a, ls) = list_len_eq n ls
        lhs = map to_ls lhs'
        rhs = map to_ls rhs'
        ls = shortest_first_ex len_eq [lhs, rhs]
        r = S $ map to_als ls
    -}
instance C by ls len als => Alternative (S by ls len als) where
    empty = mzero
    (<|>) = mplus

--}
--}
--}
--}
