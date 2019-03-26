
module Seed.PairOps
    ( module Seed.PairOps
    -- , module Data.Bifunctor
    , bimap
    , module Seed.OpSwap
    )
where
import Data.Bifunctor (bimap)
import Seed.OpSwap


{-
use swap instead
pair_flip :: (a, b) -> (b, a)
pair_flip (a, b) = (b, a)
-}
fork :: i -> (i, i)
fork i = (i,i)


mapFst :: (a->c) -> (a, b) -> (c, b)
mapSnd :: (b->c) -> (a, b) -> (a, c)
mapFst = flip bimap id
mapSnd = fmap

pair2triple :: a -> (b,c) -> (a, b, c)
pair2triple a (b,c) = (a, b, c)
pairs2triples :: a -> [(b, c)] -> [(a, b, c)]
pairs2triples a = map (pair2triple a)
a_pairs_ls2triples :: [(a, [(b,c)])] -> [(a, b, c)]
a_pairs_ls2triples = concat . map f where
    f (a, pairs) = pairs2triples a pairs


-- for Arrow
ab_c2a_bc :: ((a,b), c) -> (a, (b,c))
a_bc2ab_c :: (a, (b,c)) -> ((a,b), c)
ab_c2a_bc ((a,b),c) = (a,(b,c))
a_bc2ab_c (a,(b,c)) = ((a,b),c)


abc2ab_c :: (a,b,c) -> ((a,b),c)
abc2a_bc :: (a,b,c) -> (a,(b,c))
abc2ab_c (a,b,c) = ((a,b),c)
abc2a_bc (a,b,c) = (a,(b,c))
ab_c2abc :: ((a,b),c) -> (a,b,c)
a_bc2abc :: (a,(b,c)) -> (a,b,c)
ab_c2abc ((a,b),c) = (a,b,c)
a_bc2abc (a,(b,c)) = (a,b,c)
ab2ba :: (a,b) -> (b,a)
ab2ba (a,b) = (b,a)


ab_c2ac_b :: ((a,b),c) -> ((a,c),b)
ab_c2ac_b ((a,b),c) = ((a,c),b)
ab_c2ac_c :: ((a,b),c) -> ((a,c),c)
ab_c2ac_c ((a,b),c) = ((a,c),c)


ab2aa :: (a,b) -> (a,a)
ab2_a :: (a,b) -> ((),a)
ab2bb :: (a,b) -> (b,b)
ab2aa (a,b) = (a,a)
ab2_a (a,b) = ((),a)
ab2bb (a,b) = (b,b)

