{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module RE.DFA where

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
import RE.RE
import RE.UtilsDFA (RawDFA(RawDFA))
import qualified RE.UtilsDFA as U



-- DFA { dfa_table, dfa_finals, dfa_initial, dfa_all_states, dfa_full_touched_ranges, dfa_std_symbol2range}
map_stateDFA :: Ord st' => (st -> st') -> DFA p st -> DFA p st'
map_stateDFA f  DFA { dfa_table = dfa_table
                    , dfa_finals = dfa_finals
                    , dfa_initial = dfa_initial
                    , dfa_all_states = dfa_all_states
                    , dfa_full_touched_ranges = dfa_full_touched_ranges
                    , dfa_std_symbol2range = dfa_std_symbol2range
                    }
    = DFA   { dfa_table = M.mapKeys f $ fmap (fmap f) dfa_table
            , dfa_finals = S.map f dfa_finals
            , dfa_initial = f dfa_initial
            , dfa_all_states = S.map f dfa_all_states
            , dfa_full_touched_ranges = dfa_full_touched_ranges
            , dfa_std_symbol2range = dfa_std_symbol2range
            }
type TableDFA p st = Map st (AutoMergeRangeMap p st)
data DFA p st = DFA { dfa_table :: TableDFA p st
                    , dfa_finals :: Set st
                    , dfa_initial :: st
                    --
                    , dfa_all_states :: Set st
                        -- include error/final states and initial_st
                        -- dfa_table may not contains error/final states
                    , dfa_full_touched_ranges :: [Range p]
                    -- disjoint_range <-> sym
                    , dfa_std_symbol2range :: Map (RawValue p) (Range p)
                    }
    -- st not in dfa_table ==>> is_error st
    -- any sym ==>> sym in dfa_table[st]
instance (Ord st, RangePoint p) => IDFA (DFA p st) where
    type State (DFA p st) = st
    type Symbol (DFA p st) = RawValue p
    transition dfa st sym = r where
        r = maybe st f $ M.lookup st $ dfa_table dfa
        f = fromJust . may_getAutoMergeRangeMap sym
    is_final dfa st = st `S.member` dfa_finals dfa
    is_error dfa st = st `M.notMember` dfa_table dfa
    initial_state dfa = dfa_initial dfa



re2DFA :: (RangePoint p) => RE p -> DFA p RE_State
re2DFA re = mkDFA (re2disjoint_ranges re) re
mkDFA :: (IDFA a, RangePoint p, RawValue p ~ Symbol a, Ord (State a))
        => [Range p] -> a -> DFA p (State a)
mkDFA full_touched_ranges idfa =
    DFA { dfa_table = dfa_table
        , dfa_finals = dfa_finals
        , dfa_initial = dfa_initial
        , dfa_all_states = dfa_all_states
        , dfa_full_touched_ranges = dfa_full_touched_ranges
        -- , dfa_all_symbols = dfa_all_symbols
        , dfa_std_symbol2range = dfa_std_symbol2range
        } where
    range2may_raw_valueEx rng = fmap (\sym->(sym,rng)) $ range2may_raw_value rng
    symbol2range = M.fromList . catMaybes
        $ map range2may_raw_valueEx full_touched_ranges
    dfa_all_symbols = M.keysSet symbol2range
    dfa_std_symbol2range = symbol2range
    sym_rng_pairs = M.toList symbol2range
    swap (a, b) = (b, a)
    rng_sym_pairs = map swap sym_rng_pairs

    dfa_full_touched_ranges = full_touched_ranges
    dfa_initial = initial_state idfa
    --
    -- error: dfa_table = f (S.singleton dfa_initial) M.empty
    dfa_table = f (put' dfa_initial S.empty) M.empty
    -- puts :: table -> [st] -> Set st -> Set st
    -- put :: table -> st -> Set st -> Set st
    -- put' :: st -> Set st -> Set st
    put' st = if is_error idfa st then id else S.insert st
    put table st = if st `M.member` table then id else put' st
    puts table st_ls sts = foldr (put table) sts st_ls
    -- f :: states -> table -> table
    -- states /-\ keysSet table == {}
    f states table = case S.minView states of
        Nothing -> table
        Just (st, sts) ->
            let rng_st_pairs = map (fmap $ transition idfa st) rng_sym_pairs
                auto_merge = fromListAutoMergeRangeMap rng_st_pairs
                table' = M.insert st auto_merge table
                st_ls = S.toList . S.fromList $ map snd rng_st_pairs
                states' = puts table' st_ls sts
            in  f states' table'



    dfa_all_key_states = M.keys dfa_table
    dfa_all_value_states = concat
        . map (map snd . iterAutoMergeRangeMap) $ M.elems dfa_table
    dfa_finals = S.fromList $ filter (is_final idfa) dfa_all_key_states
    dfa_all_states = S.fromList
        $ dfa_initial : dfa_all_key_states ++ dfa_all_value_states
    -- dfa_finals = S.filter (is_final idfa) dfa_all_states









-----------------------
-- minimal DFA

-- minimalDFA :: (Ord st, Ord sym) => RawDFA sym st -> RawDFA sym Natural
minimalDFA :: (Ord st, RangePoint p) => DFA p st -> DFA p Natural
minimalDFA dfa = raw_dfa2DFA rngs sym2rng . U.minimalDFA $ dfa2RawDFA dfa where
    rngs = dfa_full_touched_ranges dfa
    sym2rng = dfa_std_symbol2range dfa

-- dfa2RawDFA :: DFA p st -> RawDFA (RawValue p) st
dfa_table2raw_dfa_table :: RangePoint p
    => Set (RawValue p) -> TableDFA p st -> U.TableDFA (RawValue p) st
-- dfa_table2raw_dfa_table dfa_all_symbols dfa_table =
dfa_table2raw_dfa_table dfa_all_symbols dfa_table = fmap f dfa_table where
    f auto_dict = M.mapMaybe id $ M.fromSet (g auto_dict) dfa_all_symbols
    g auto_dict sym = may_getAutoMergeRangeMap sym auto_dict
raw_dfa_table2dfa_table :: (RangePoint p, Eq st)
    => (RawValue p -> Range p) -> U.TableDFA (RawValue p) st -> TableDFA p st
-- raw_dfa_table2dfa_table sym2range raw_dfa_table =
raw_dfa_table2dfa_table sym2range raw_dfa_table = fmap f raw_dfa_table where
    f sym2st = fromListAutoMergeRangeMap . map g $ M.toList sym2st
    g (sym, st) = (sym2range sym, st)

dfa2RawDFA :: (Ord st, RangePoint p) => DFA p st -> RawDFA (RawValue p) st
raw_dfa2DFA
    :: (Ord st, RangePoint p)
    => [Range p] -> Map (RawValue p) (Range p)
    -> RawDFA (RawValue p) st -> DFA p st


dfa2RawDFA DFA
    { dfa_table, dfa_finals, dfa_initial, dfa_all_states
    , dfa_full_touched_ranges, dfa_std_symbol2range
    } = r where

    dfa_all_symbols = M.keysSet dfa_std_symbol2range
    r = RawDFA
        { U.dfa_table=dfa_table2raw_dfa_table dfa_all_symbols dfa_table
        , U.dfa_finals=dfa_finals
        , U.dfa_initial=dfa_initial
        , U.dfa_all_states=dfa_all_states
        , U.dfa_all_symbols=dfa_all_symbols
        }
raw_dfa2DFA dfa_full_touched_ranges dfa_std_symbol2range
    RawDFA  { U.dfa_table=raw_dfa_table
            , U.dfa_finals=dfa_finals
            , U.dfa_initial=dfa_initial
            , U.dfa_all_states=dfa_all_states
            , U.dfa_all_symbols=dfa_all_symbols
            }
    = r where

    f sym = M.findWithDefault undefined sym dfa_std_symbol2range
    dfa_table' = raw_dfa_table2dfa_table f raw_dfa_table
    r = DFA { dfa_table=dfa_table'
            , dfa_finals=dfa_finals
            , dfa_initial=dfa_initial
            , dfa_all_states=dfa_all_states
            , dfa_full_touched_ranges=dfa_full_touched_ranges
            , dfa_std_symbol2range=dfa_std_symbol2range
            }


{-

may_getAutoMergeRangeMap
-- RawDFA {dfa_table, dfa_finals, dfa_initial, dfa_all_states, dfa_all_symbols}










{-
re2dfa :: RangePoint p => RE p -> DFA RE_State p
re2dfa (RE_Union_Tokens from_to_pairs) = DFA
    { dfa_table = dfa_table
    , dfa_finals = dfa_finals
    , dfa_initial = dfa_initial
    -- , dfa_all_states = dfa_all_states
    , dfa_full_touched_ranges = dfa_full_touched_ranges
    } where
    dfa_finals = S.singleton final_RE_Union_Tokens_State
    dfa_initial = initial_RE_Union_Tokens_State
    -- dfa_full_touched_ranges = -ranges2disjoint_ranges rngs
    dfa_all_states = all_states_RE_Union_Tokens_State
    rngs = map (uncurry mkRange) from_to_pairs
    insert = flip insertAutoMergeRangeMap final_RE_Union_Tokens_State
    initial_dict = foldr insert emptyAutoMergeRangeMap rngs
    final_dict = mkFullAutoMergeRangeMap error_RE_Union_Tokens_State
    -- no error_dict
    dfa_table = M.fromList  [ (initial_RE_Union_Tokens_State, initial_dict)
                            , (final_RE_Union_Tokens_State, final_dict)
                            ]
re2dfa (RE_List re_ls) = dfa where
    len = genericLength re_ls
    map re2dfa re_ls
re2dfa _ =




{-
class Mapping a where
    type Key a :: *
    type Value a :: *
    getM :: Monad m => Key a -> a -> m (Value a)
    may_get :: Key a -> a -> Maybe (Value a)
    default_get :: Value a -> Key a -> a -> Value a
    result_get :: r -> (Value a -> r) -> Key a -> a -> r
class Mapping a => OpInsert a where
    type UpdateKey a :: *
    insert :: (UpdateKey a, Value a) -> a -> a
    inserts :: [(UpdateKey a, Value a)] -> a -> a
instance OpIsEmpty
instance OpEmpty

instance RangePoint p => Mapping (RangeMap p v) where
    type Key (RangeMap p v) = RawValue p
    type Value (RangeMap p v) = v
    may_get k (RangeMap d) =
        p = raw_value2point k
        -- search ["p]", "p]"]
        -- search "p)"
        __smalls, __may_midV, larges = split (RightExcluded p) d
        do
            (right_bound, (left_bound, v)) <- min larges
            let rng = (left_bound, right_bound)
            if p `is_in_range` rng then Just v
            else Nothing
instance RangePoint p => OpInsert (RangeMap p v) where
    type UpdateKey (RangeMap p v) = Range p
    insert (k, v) (RangeMap d) =
        (left, right) = raw_value2range k
        -- ([\[(] left, right [)\]])
        -- search ["left)", "right]"]
        smalls, may_leftV, _larges = split (RightExcluded $ unLeftBound left) d
        _middles, may_rightV, larges = split (RightIncluded $ unRightBound right) _larges
        middles = if isJust may_leftV then insert ... _middles else _middles
        -- snd_half of head middles; fst_half of head larges
        -- or:  middle of head larges
        d' = for k <- middles except fst: delete k d
        ...



--}
--}
--}
--}
--}
--}
--}
--}

