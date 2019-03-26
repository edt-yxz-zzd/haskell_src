{-# LANGUAGE TypeFamilies #-}



module Seed.ListOps.MergeLists
    ( merge_sorted_sorted_listss, merge_two_sorted_lists
    --, shortest_first, shortest_first_ex
    --, pr
    )
where
import Data.Bifunctor
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map (Map)
import Seed.MapOps (group_by_Map)


merge_two_sorted_lists :: (a->a->Bool) -> [a] -> [a] -> [a]
merge_two_sorted_lists good_order = f where
    f [] rhs = rhs
    f lhs [] = lhs
    f lhs@(v:vs) rhs@(u:us) =
        if good_order v u then v : f vs rhs else u : f lhs us



merge_sorted_sorted_listss :: Ord ord => (a->ord) -> [[a]] -> [a]
merge_sorted_sorted_listss to_ord lss = r where
    frontier = M.empty
    r = merge_frontier to_ord frontier lss



-- Map k lss
--  - lss not null, ls in lss not null, key is from head of each ls
--  - ls is sorted
-- lss - lss sorted, each ls sorted
-- Note: fail when some "a" has infinite ord...
merge_frontier :: (lss~[[a]], Ord k) => (a->k) -> Map k lss -> lss -> [a]
merge_frontier to_ord frontier lss = r where
    -- frontier :: Map ord [[a]] -- all begin with ord
    -- mk_frontierX frontier = (M.minViewWithKey frontier, frontier)
    r = merge frontier lss
    good_order ka b = ka <= to_ord b
    put [] frontier = frontier
    put vs frontier = insertWays [vs] frontier
    insertWays vss frontier
        = group_by_Map (to_ord . head) id vss' frontier where
        vss' = filter (not . null) vss

    merge frontier x@((v:vs):lss) =
        case M.minViewWithKey frontier of
            Nothing -> v : merge (put vs frontier) lss
            Just ((ord, uss), frontier') -> if good_order ord v
                then let (us, frontier'') = handle_minUSS uss frontier'
                     in  us ++ merge frontier'' x
                else v : merge (put vs frontier) lss
    merge frontier ([]:lss) = merge frontier lss
    merge frontier [] = flush frontier
    flush frontier = case M.minView frontier of
        Just (uss, frontier') ->
            let (us, frontier'') = handle_minUSS uss frontier'
            in  us ++ flush frontier''
        Nothing -> []
    handle_minUSS uss frontier' = (us, frontier'') where
        us = map head uss
        uss' = map tail uss
        frontier'' = insertWays uss' frontier'


pr = do
    let lss = [[i..] | i <- [0..]] :: [[Int]]
        ord = id
    print $ merge_sorted_sorted_listss ord lss




{-
-- lsss - finite
-- lss  - shortest first; may infinite, but finite per length
-- used in CFG generate process
-- list_ABS_eq - e.g. list_len_eq
shortest_first :: [[[a]]] -> [[a]]
shortest_first = shortest_first_ex list_len_eq
shortest_first_ex :: (Natural -> ls -> Bool) -> [[ls]] -> [ls]
shortest_first_ex list_ABS_eq finite_list_of_shortest_first_lss = lss' where
    _lsss = finite_list_of_shortest_first_lss
    lss' = _shortest_first 0 _lsss
    --  return (len_eq n, len_gt n)
    --pick_len :: Natural -> [[a]] -> ([[a]], [[a]])
    --pick_len n lsss = partition (list_len_eq n) lsss
    --since sorted
    pick_len n lss = span (list_ABS_eq n) lss
    collect_eq _ [] = ([], []) -- (lss, lsss)
    collect_eq n (lss : lsss) = (eq ++ lss', lsss'') where
        (eq, gt) = pick_len n lss
        (lss', lsss') = collect_eq n lsss
        lsss'' = if null gt then lsss' else gt : lsss'
    -- lsss - all len(lss) >= n
    _shortest_first n [] = []
    _shortest_first n lsss = eq ++ _shortest_first (succ n) lsss' where
        (eq, lsss') = collect_eq n lsss



-- another shortest_first
-- assert lsls grouped by len's
shortest_firstL
    :: (lss ~ [ls], (ord, lss) ~ group, Ord ord, way ~ [group])
    => [way] -> way
shortest_firstL finite_ways_with_sorted_groups = r where
    ways = finite_ways_with_sorted_groups
    unsafe_group1s2ord way@(group : _) = fst group
    -- d :: Map ord [way]
    d = insertWays ways M.empty
    insertWays ways d = group_by_Map unsafe_group1s2ord id ways' d where
        ways' = filter (not . null) ways
    -- get_min_group :: Map ord way -> Maybe (group, Map ...)
    get_min_group d = fmap f $ M.minView d where
        f (ways, d) = (group, d') where
            groups = map head ways
            min_ord = unsafe_group1s2ord groups
            group = (min_ord, concat $ map snd groups)
            ways' = map tail ways
            d' = insertWays ways' d

    r = _shortest_first d
    _shortest_first = unfoldr get_min_group
    {-
    _shortest_first d = case get_min_group d of
        Nothing -> []
        Just (group, d') -> group : _shortest_first d'
    -}

-- shortest_firstL is for alternatives
-- shortest_first_chain is for right part
{-
shortest_first_chain
    => (a -> a -> a) -> (a -> a -> Bool)
    -> ([a] -> [a] -> [a])
shortest_first_chain add good_order sorted_way1 sorted_way2 = r where
-}

product_two_sorted_lists
    :: (a->a->a) -> [a] -> [a] -> [[a]]
product_two_sorted_lists add lhs rhs = fmap f lhs where
    f a = fmap (add a) rhs
product_and_merge_two_sorted_lists
    :: Ord ord => (a->ord) -> (a->a->a) -> [a] -> [a] -> [a]
product_and_merge_two_sorted_lists to_ord add lhs rhs = r where
    lss = product_two_sorted_lists add lhs rhs
    r = merge_sorted_sorted_listss to_ord lss
product_and_merge_finite_sorted_list1s
    :: Ord ord => (a->ord) -> (a->a->a) -> [[a]] -> [a]
product_and_merge_finite_sorted_list1s to_ord add lss = r where
    r = foldr1 f lss
    f = product_and_merge_two_sorted_lists to_ord add

{- error : fail for infinite list??
merge_sorted_listss :: (a->a->Bool) -> [[a]] -> [a]
merge_sorted_listss good_order lss = foldr f [] lss where
    f lhs rhs = merge_two_sorted_lists good_order lhs rhs
--
merge_sorted_listss good_order [] = []
merge_sorted_listss good_order [ls] = ls
merge_sorted_listss good_order ([]:lss) = merge_sorted_listss good_order lss
merge_sorted_listss good_order ((h:ls):lss)
    = merge_sorted_listss good_order lss
-}

--}
--}
--}
--}
--}


