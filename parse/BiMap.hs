{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , TupleSections
            , UndecidableInstances #-}

import SeedUtils
import Container hiding (Set, Map)
import qualified Container as C
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)
import Data.List (foldl')













-- class BiMap k v m | m -> k v where





data BiMap k v = BM { k2vs :: Map k (Set v)
                       , v2ks :: Map v (Set k)
                       , size :: Int
                       }

type BM = BiMap

type Maps k v = Map k (Set v)

instance Container (k, v) (BM k v)
instance (Container (k, v) (BM k v), Ord k, Ord v)
    => Member (k,v) (BM k v) where
    member (k, v) m = contain k v m
--instance Member (k, v) (BM k v) => Choose (k, v) (BM k v) where
    --choose (k, v) m = get_vs k m
instance Null (k, v) (BM k v) => AnyElem (k, v) (BM k v) where
    any_elem = safe_head . iter


-- for = flip map
instance AnyElem (k, v) (BM k v) => Iterable (k, v) (BM k v) where
    -- iter :: BM k v -> [(k, v)]
    iter m = concat $ for (M.toList $ k2vs m) $ \(k, vs) -> 
                map (k,) (S.toList vs)

instance Container (k, v) (BM k v) => Null (k, v) (BM k v) where
    null m = size m == 0
instance Null (k, v) (BM k v) => Sized (k, v) (BM k v) where
    len = toInteger . size
instance Null (k, v) (BM k v) => Empty (k, v) (BM k v) where
    empty = BM M.empty M.empty 0

instance (Container (k, v) (BM k v), Ord k, Ord v) 
    => Insert (k, v) (BM k v) where
    -- extend :: Iterable a it => Integer -> it -> c -> c
    insert kv@(k,v) m@(BM k2vs v2ks size) = if contains kv m then m
        else BM (put_kv kv k2vs) (put_kv (v,k) v2ks) (size+1)
instance (Iterable (k, v) (BM k v), Ord k, Ord v)
    => Pop (k, v) (BM k v) where
    -- pops :: c -> [(a, c)]
    -- pops m = if is_emtpy m then [] else case m of
    pops (BM k2vs v2ks size) = if size == 0 then [] else
        --updateMin f k2vs -- deleteFindMin
        let (k, vs) = M.findMin k2vs
            Just (v, vs') = S.minView vs
            del k ks = let ks' = S.delete k ks in 
                       if S.null ks' then Nothing else Just ks'
            v2ks' = M.update (del k) v v2ks
            k2vs' = M.update (del v) k k2vs
            m' = BM k2vs' v2ks' (size-1)
            a = (k, v)
            in (a, m') : pops m'
instance (Empty (k, v) (BM k v), Insert (k, v) (BM k v))
    => AnyFiniteSize (k, v) (BM k v) where
    singleton (k, v) = BM (M.singleton k (S.singleton v))
                          (M.singleton v (S.singleton k))
                          1

instance (AnyFiniteSize a q, Pop a q) => Buffer a q


instance (Member (k, v) (BM k v), Ord k, Ord v) => Remove (k, v) (BM k v) where
    remove kv@(k, v) m@(BM k2vs v2ks size) = if contains kv m then
        let m' = BM (remove_kv kv k2vs) (remove_kv (v, k) v2ks) (size-1)
            -- why return input kv??
            -- I donot know how to choose the element
            -- maybe s-(s-{e})??
        in Just ([kv], m')
        else Nothing
--instance (Empty (k, v) (BM k v), Insert (k, v) (BM k v))
--    => AnyFiniteSize (k, v) (BM k v) where



-- get_vs :: Ord k => k -> m -> Maybe (Set v)
get_vs k m = M.lookup k $ k2vs m
-- get_ks :: Ord v => v -> m -> Maybe (Set k)
get_ks v m = M.lookup v $ v2ks m

-- contain :: (Ord k, Ord v) => k -> v -> m -> Bool
contain k v m = maybe False (S.member v) $ get_vs k m

put_kv (k, v) k2vs = M.alter (Just . f) k k2vs where
    f = maybe (S.singleton v) (S.insert v)
remove_kv (k, v) k2vs = M.alter f k k2vs where
    -- (mvs :: Maybe (Set v))
    --
    f mvs = mvs >>= \vs -> just $ S.delete v vs



sizeK = M.size . k2vs
listK = M.keys . k2vs

sizeV = M.size . v2ks
listV = M.keys . v2ks




{-
instance Insert (k, v) (BiMap k v) where
add :: (Ord k, Ord v) => k -> v -> m -> m
add k v (BiMap k2vs v2ks) = 
-}






--    sizeK :: 


