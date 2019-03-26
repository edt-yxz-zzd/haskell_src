
module Parser.ShowMonad
    ( S()
    , puts
    , run
    )
where

import Seed.AList hiding (AList, alist2list, list2alist)
import Seed.MonadOps
import Seed.ListOps -- shortest_firstL
import IntDefs.IntDefs (UInt)

type AList = []
alist2list = id
list2alist = id
data S t a = S [(a, AList t, UInt)]


puts :: [t] -> S t ()
puts ts = S [((), ts, genericLength ts)]
run :: S t a -> [[t]]
run (S pairs) = map f pairs where
    f (_, ls, _) = ls
instance Functor (S t) where
    fmap = fmapM2F
instance Applicative (S t) where
    pure = return
    (<*>) = (<***>)
instance Monad (S t) where
    return a = S [(a, list2alist [], 0)]
    S ls >>= a2S = S pairs where
        pairss = do
            (a, als, na) <- ls
            let S ls' = a2S a
            return $ do
                (b, als', nb) <- ls'
                let ls'' = als <> als'
                    --n = length ls''
                    n = na + nb
                return (b, ls'', n)
        to_ord (_,_,n) = n
        pairs = merge_sorted_sorted_listss to_ord pairss
    {-
    S ls >>= a2S = S pairs where
        pairs = do
            (a, als) <- ls
            let S ls' = a2S a
            (b, als') <- ls'
            return (b, als <> als')
    -}

instance MonadPlus (S t) where
    mzero = S []
    S lhs `mplus` S rhs = r where
        cmp (_,_,n) (_,_,m) = n <= m
        ls = merge_two_sorted_lists cmp lhs rhs
        r = S ls
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
instance Alternative (S t) where
    empty = mzero
    (<|>) = mplus

