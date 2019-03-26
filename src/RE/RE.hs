{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module RE.RE where

import Numeric.Natural
import Prelude hiding (Bounded)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.List (genericLength, genericIndex, sort, groupBy)
--import Seed.Boxed

import RE.IDFA
import RE.Bounded
import RE.RangePoint
import RE.RangeMap



-------------------------------------------
-------------------------------------------
data RE p
    = RE_Union_Tokens [(RawValue p, RawValue p)] -- from to
    | RE_Not_Union_Tokens [(RawValue p, RawValue p)] -- from to
    | RE_List [RE p]
    | RE_Union [RE p]
    | RE_Star (RE p)
    --  | RE_Not (RE p)
data RE_State
    = RE_Union_Tokens_State (Set Bool)
        -- False ~ initial; True ~ final; {} ~ error
    | RE_Not_Union_Tokens_State (Set Bool)
        -- False ~ initial; True ~ final; {} ~ error
    | RE_List_State Natural Bool (Map Natural RE_State)
        -- max_length; is_final; middle states
        -- (False, {}) ~ error; (True, _) ~ final; ...
        -- not $ is_error middle_states[i]
        --      i.e. any empty set will be removed
    | RE_Union_State Natural Bool (Map Natural RE_State)
        -- max_length; is_final; alter states
        -- is_final == any is_final alter_states
        -- {} ~ error
        -- not $ is_error alter_states[i]
        --      i.e. any empty set will be removed
    | RE_Star_State RE_State
    deriving (Eq, Ord, Show, Read)

initial_RE_Union_Tokens_State = RE_Union_Tokens_State $ S.singleton False
final_RE_Union_Tokens_State = RE_Union_Tokens_State $ S.singleton True
error_RE_Union_Tokens_State = RE_Union_Tokens_State $ S.empty
initial_RE_Not_Union_Tokens_State = RE_Not_Union_Tokens_State $ S.singleton False
final_RE_Not_Union_Tokens_State = RE_Not_Union_Tokens_State $ S.singleton True
error_RE_Not_Union_Tokens_State = RE_Not_Union_Tokens_State $ S.empty
all_states_RE_Union_Tokens_State = S.fromList
    $ map (RE_Union_Tokens_State . S.fromList) [[a,b] | a<-ls, b<-ls] where
    ls = [False, True]


re2disjoint_ranges, re2ranges :: RangePoint p => RE p -> [Range p]
re2disjoint_ranges =
    ranges2full_touched_ranges_MergeNoRawValueRange . re2ranges
re2ranges = f where
    fromto2rngs = map (uncurry mkRange)
    res2rngs = concat . map re2ranges
    f re = case re of
        RE_Union_Tokens from_to_pairs -> fromto2rngs from_to_pairs
        RE_Not_Union_Tokens from_to_pairs -> fromto2rngs from_to_pairs
        RE_List re_ls -> res2rngs re_ls
        RE_Union re_ls -> res2rngs re_ls
        RE_Star re -> re2ranges re




merge_RE_State :: RE_State -> RE_State -> RE_State
merge_RE_State (RE_Union_Tokens_State sts) (RE_Union_Tokens_State sts')
    = RE_Union_Tokens_State $ S.union sts sts'
merge_RE_State (RE_Not_Union_Tokens_State sts) (RE_Not_Union_Tokens_State sts')
    = RE_Not_Union_Tokens_State $ S.union sts sts'
merge_RE_State  (RE_Union_State len isFinal idx2st)
                (RE_Union_State len' isFinal' idx2st')
    = if len /= len' then error "not same shape: RE_State" else
        RE_Union_State len (isFinal || isFinal')
        $ M.unionWith merge_RE_State idx2st idx2st'
merge_RE_State  (RE_List_State len isFinal idx2st)
                (RE_List_State len' isFinal' idx2st')
    = if len /= len' then error "not same shape: RE_State" else
        RE_List_State len (isFinal || isFinal')
        $ M.unionWith merge_RE_State idx2st idx2st'
merge_RE_State (RE_Star_State st) (RE_Star_State st')
    = RE_Star_State $ merge_RE_State st st'

instance (RangePoint p) => IDFA (RE p) where
    type State (RE p) = RE_State
    type Symbol (RE p) = RawValue p
    transition x@(RE_Star re) st sym = r where
        r   | is_error x st = st
            | otherwise = case st of
                RE_Star_State st -> ff st
        ff st = r where
            st' = transition re st sym
            st'' = if is_final re st' then merge_RE_State init_st st else st
            r = RE_Star_State st''
        init_st = initial_state re
    transition x@(RE_List re_ls) st sym = r where
        r   | is_error x st = st
            | otherwise = case st of
                RE_List_State len isFinal idx2st -> g $ ff idx2st 0 re_ls
        len = genericLength re_ls
        g (isFinal, idx2st) = RE_List_State len isFinal idx2st
        ff idx2st = f False where
            f with_initial_st idx [] = let isFinal = with_initial_st in
                (isFinal, M.empty)
            f with_initial_st idx (re : ls) =
                h $ f with_initial_st' (succ idx) ls
              where
                init_st = initial_state re
                (h, with_initial_st') = case M.lookup idx idx2st of
                        Nothing -> h0
                        Just st -> h1 $ transition re st sym
                h0 = if with_initial_st
                        then (fmap $ M.insert idx init_st, False)
                        else (id, False)
                h1 st = if is_error re st then h0 else
                    if not with_initial_st
                    then (fmap $ M.insert idx st, is_final re st)
                    else (fmap $ M.insert idx $ merge_RE_State init_st st
                        , is_final re st)

    transition x@(RE_Union re_ls) st sym = r where
        r   | is_error x st = st
            | otherwise = case st of
                RE_Union_State len isFinal idx2st -> f idx2st
        len = genericLength re_ls
        f idx2st = RE_Union_State len isFinal' idx2st' where
            idx2st' = M.fromList $ map fst ls
            isFinal' = any snd ls
            ls =
                [ ((idx, st'), is_final re st')
                | (idx, st) <- M.toList idx2st
                , idx < len
                , let re = genericIndex re_ls idx
                , let st' = transition re st sym
                , not $ is_error re st'
                ]


    -- transition :: forall. RE p -> RE_State -> RawValue p -> RE_State
    transition (RE_Not_Union_Tokens from_to_pairs) st sym = r where
        p = raw_value2point sym :: p
        rngs = map (uncurry mkRange) from_to_pairs
        inside = any . is_in_range
        r = if st /= initial_RE_Not_Union_Tokens_State
            then error_RE_Not_Union_Tokens_State else
            if inside p rngs
            then error_RE_Not_Union_Tokens_State else
            final_RE_Not_Union_Tokens_State

    transition (RE_Union_Tokens from_to_pairs) st sym = r where
        p = raw_value2point sym :: p
        rngs = map (uncurry mkRange) from_to_pairs
        inside = any . is_in_range
        r = if st /= initial_RE_Union_Tokens_State
            then error_RE_Union_Tokens_State else
            if inside p rngs
            then final_RE_Union_Tokens_State else
            error_RE_Union_Tokens_State




    is_final re st = (not $ is_error re st) && f re st where
        f (RE_Union_Tokens _) st = final_RE_Union_Tokens_State == st
        f (RE_Not_Union_Tokens _) st = final_RE_Not_Union_Tokens_State == st
        f (RE_Union _) (RE_Union_State len isFinal _) = isFinal
        f (RE_List _) (RE_List_State len isFinal _) = isFinal
        f (RE_Star re) (RE_Star_State st) = is_final re st

    is_error (RE_Union_Tokens _) st = error_RE_Union_Tokens_State == st
    is_error (RE_Not_Union_Tokens _) st = error_RE_Not_Union_Tokens_State == st
    is_error (RE_Union ls) (RE_Union_State len isFinal d) =
        genericLength ls /= len || (not isFinal && M.null d)
    is_error (RE_List ls) (RE_List_State len isFinal d) =
        genericLength ls /= len || (not isFinal && M.null d)
    is_error (RE_Star re) (RE_Star_State st) = is_error re st

    initial_state (RE_Union_Tokens _) = initial_RE_Union_Tokens_State
    initial_state (RE_Not_Union_Tokens _) = initial_RE_Not_Union_Tokens_State
    initial_state (RE_Union re_ls) = RE_Union_State len isFinal idx2st where
        idx2st = M.fromList $ map fst ls
        isFinal = any snd ls
        len = genericLength re_ls
        ls =
            [ ((idx, st), is_final re st)
            | (idx, re) <- zip [0..] re_ls
            , let st = initial_state re
            , not $ is_error re st
            ]
    initial_state (RE_List re_ls) = g $ f 0 re_ls where
        len = genericLength re_ls
        g (isFinal, idx2st) = RE_List_State len isFinal idx2st
        f idx [] = (True, M.empty)
        f idx (re : ls) = r where
            st = initial_state re
            r = if is_final re st
                then fmap (M.insert idx st) $ f (succ idx) ls
                else if is_error re st
                then (False, M.empty)
                else (False, M.singleton idx st)
    initial_state (RE_Star re) = RE_Star_State $ initial_state re

--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
--}
