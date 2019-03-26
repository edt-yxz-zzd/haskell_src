{-# LANGUAGE NamedFieldPuns #-}

module RE.UtilsDFA where


import Numeric.Natural
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import RE.Refine (refine__Functor, mkOld2NewDefault)


set2pred :: Ord k => Set k -> (k -> Bool)
set2pred = flip S.member
set2not_pred :: Ord k => Set k -> (k -> Bool)
set2not_pred = flip S.notMember

intersection_Set_Map :: Ord k => Set k -> Map k v -> Map k v
intersection_Set_Map set dict =
    M.intersection dict $ M.fromSet (const ()) set
partitionByKeySet :: Ord k => Set k -> Map k v -> (Map k v, Map k v)
partitionByKeySet = partitionByKey . set2pred
partitionByValueSet :: Ord v => Set v -> Map k v -> (Map k v, Map k v)
partitionByValueSet = partitionByValue . set2pred

partitionByValue :: (v -> Bool) -> Map k v -> (Map k v, Map k v)
partitionByValue = M.partition
partitionByKey :: (k -> Bool) -> Map k v -> (Map k v, Map k v)
    -- (Trues, Falses)
partitionByKey f = M.partitionWithKey (\k v -> f k)
--filterBySet :: Set v -> Map k v -> Map k v
--filterBySet = M.filter . set2pred



type TableDFA sym st = Map st (Map sym st)
type BackTableDFA st = Map st (Set st)
    -- {st : [st', ..], ..} ==>> exists sym, table[st'][sym] == st
mapState :: Ord k => (st -> k) -> TableDFA sym st -> TableDFA sym k
mapState f = fmap (fmap f) . M.mapKeys f
-- mapStateToNatural :: Ord st => TableDFA sym st -> TableDFA sym Natural



mkBackTableDFA :: Ord st => TableDFA sym st -> BackTableDFA st
mkBackTableDFA = M.foldrWithKey f M.empty where
    f st sym2st st2sts =
        foldr (\k -> M.alter (g st) k) st2sts $ M.elems sym2st
    g st = Just . maybe (S.singleton st) (S.insert st)
mkValueStates :: Ord st => TableDFA sym st -> Set st
mkValueStates table = S.fromList values where
    values = concat . map M.elems $ M.elems table
collectStates :: Ord st => TableDFA sym st -> Set st
collectStates table = S.union keys values where
    keys = M.keysSet table
    values = mkValueStates table
mkAllStates :: Ord st => TableDFA sym st -> Set st -> Set st -> Set st
mkAllStates table initials finals = r where
    sts = collectStates table
    r = S.unions [sts, finals, initials]

findReachableStates :: Ord st => TableDFA sym st -> Set st -> Set st
findReachableStates table initials = news1 initials initials where
    -- step1 :: sts -> sts'
    step1 sts = mkValueStates
        $ M.filterWithKey (\k v -> S.member k sts) table
    -- news <= reachables
    news1 reachables news | S.null news = reachables
    news1 reachables news = news1 reachables' news' where
        news' = step1 news S.\\ reachables
        reachables' = S.union reachables news'



findNonErrorStates, findErrorStates
    :: Ord st => TableDFA sym st -> Set st -> Set st
findErrorStates table finals = errors where
    nonerrors = findNonErrorStates table finals
    errors = collectStates table S.\\ nonerrors
findNonErrorStates table finals = f back_table finals finals where
    -- may not null (finals \\ table); i.e. some finals not in keys table
    back_table = mkBackTableDFA table
    -- new_nonerrors <= all_nonerrors
    f back_table all_nonerrors new_nonerrors
        | S.null new_nonerrors = all_nonerrors
    f back_table all_nonerrors new_nonerrors = r where
        (to_collect, back_table') = partitionByKeySet new_nonerrors back_table
        new_nonerrors' = S.unions (M.elems to_collect) S.\\ all_nonerrors
        all_nonerrors' = S.union all_nonerrors new_nonerrors'
        r = f back_table' all_nonerrors' new_nonerrors'

{-
removeDeadStates
    :: (Ord st, Ord sym) => TableDFA sym st -> TableDFA sym st
    -- {st : {}, ..} or st not in {..}
removeDeadStates table = table'' where
    (to_removesMap, table') = partitionByValue M.null table
    to_removes = M.keysSet to_removesMap
    table'' = fmap (snd . partitionByValueSet to_removes) table'
-}
removeStates
    :: (Ord st) => Set st -> TableDFA sym st -> TableDFA sym st
removeStates to_removes table = table'' where
    (_, table') = partitionByKeySet to_removes table
    table'' = fmap (snd . partitionByValueSet to_removes) table'
    -- r = removeDeadStates table''


find_removeErrorStates
    :: Ord st => Set st -> TableDFA sym st -> TableDFA sym st
find_removeErrorStates finals table = removeStates errors table where
    errors = findErrorStates table finals




-------------
-- minimal DFA
groupStates
    :: (Ord st, Ord sym)
    => Set st -> Set st -> Set sym -> TableDFA sym st -> Map st Natural
groupStates finals all_states all_symbols table = partition' where
    partition = M.fromSet f all_states
    error_nat = fromIntegral $ S.size all_states
        -- should be the same as error_nat that present in refine__Functor!
    f st = if S.member st finals then 0 else error_nat
    -- partition' = refine (recolor error_nat) M.empty table partition
    partition' = refine__Functor complete_color M.empty table partition
    default_color = M.fromSet (const error_nat) all_symbols
    complete_color color = M.union color default_color





-- RawDFA {dfa_table, dfa_finals, dfa_initial, dfa_all_states, dfa_all_symbols}
data RawDFA sym st
    = RawDFA{ dfa_table :: TableDFA sym st
                -- info :: Map sym st
                -- M.size info == S.size dfa_all_symbols
                -- so (color :: Map sym Natural) can compare
                -- but this requires an error state!
            , dfa_finals :: Set st
            , dfa_initial :: st
            --
            , dfa_all_states :: Set st
                -- include error/final states and initial_st
                -- dfa_table may not contains error/final states
            , dfa_all_symbols :: Set sym
            }
    deriving (Read, Show, Eq, Ord)

mapStateDFA :: Ord k => (st -> k) -> RawDFA sym st -> RawDFA sym k
mapStateDFA f RawDFA
    {dfa_table, dfa_finals, dfa_initial, dfa_all_states, dfa_all_symbols}
    = RawDFA
    { dfa_table = mapState f dfa_table
    , dfa_finals = S.map f dfa_finals
    , dfa_initial = f dfa_initial
    , dfa_all_states = S.map f dfa_all_states
    , dfa_all_symbols = dfa_all_symbols
    }
mapStateToNaturalDFA :: Ord st => RawDFA sym st -> RawDFA sym Natural
mapStateToNaturalDFA dfa = mapStateDFA (st2nat M.!) dfa where
    all_states = S.toList $ dfa_all_states dfa
    st2nat = M.fromList $ zip all_states [0..]


minimalDFA :: (Ord st, Ord sym) => RawDFA sym st -> RawDFA sym Natural
minimalDFA = minimalDFA_ . mapStateToNaturalDFA
minimalDFA_ :: (Ord st, Ord sym) => RawDFA sym st -> RawDFA sym Natural
minimalDFA_ RawDFA
    {dfa_table, dfa_finals, dfa_initial, dfa_all_states, dfa_all_symbols}
    = r where
    reachables = findReachableStates dfa_table $ S.singleton dfa_initial
    nonerrors = findNonErrorStates dfa_table dfa_finals
    good_states = S.intersection reachables nonerrors
    bad_states = dfa_all_states S.\\ good_states
    dfa_table' = removeStates bad_states dfa_table
    dfa_all_states' = S.insert dfa_initial good_states
    dfa_finals' = S.intersection dfa_finals good_states
    partition = groupStates
        dfa_finals' dfa_all_states' dfa_all_symbols dfa_table'

    error_nat = fromIntegral $ M.size partition
    f = mkOld2NewDefault partition error_nat
        -- dfa_initial may be error
    r = RawDFA
        { dfa_table = mapState f dfa_table'
        , dfa_finals = S.map f dfa_finals'
        , dfa_initial = f dfa_initial
        , dfa_all_states = S.map f dfa_all_states'
        , dfa_all_symbols = dfa_all_symbols
        }



--------------------------------
-- test

all_bools = S.fromList [False, True]
all_bools2const :: v -> Map Bool v
all_bools2const a = M.fromSet (const a) all_bools
data St = Initial | Middle | Final | Error
    deriving (Read, Show, Eq, Ord)
dead_dfa = minimalDFA RawDFA
        { dfa_table = M.empty
        , dfa_finals = S.empty
        , dfa_initial = Initial
        , dfa_all_states = S.singleton Initial
        , dfa_all_symbols = all_bools
        }

main = do
    print dead_dfa
    print success_dfa
    print char_dfa

success_dfa = minimalDFA RawDFA
        { dfa_table = M.singleton Initial $ all_bools2const Initial
        , dfa_finals = S.singleton Initial
        , dfa_initial = Initial
        , dfa_all_states = S.singleton Initial
        , dfa_all_symbols = all_bools
        }
char_dfa = minimalDFA RawDFA
        { dfa_table = M.fromList
                    [ (Initial, M.fromList [(False, Error), (True, Final)])
                    , (Final, all_bools2const Error)
                    ]
        , dfa_finals = S.singleton Final
        , dfa_initial = Initial
        , dfa_all_states = S.fromList [Initial, Final, Error]
        , dfa_all_symbols = all_bools
        }
{-
dead_dfa = RawDFA
        { dfa_table = 
        , dfa_finals = 
        , dfa_initial = 
        , dfa_all_states = 
        , dfa_all_symbols = 
        }
dead_dfa = RawDFA
        { dfa_table = 
        , dfa_finals = 
        , dfa_initial = 
        , dfa_all_states = 
        , dfa_all_symbols = 
        }
dead_dfa = RawDFA
        { dfa_table = 
        , dfa_finals = 
        , dfa_initial = 
        , dfa_all_states = 
        , dfa_all_symbols = 
        }


{-
        { dfa_table = 
        , dfa_finals = 
        , dfa_initial = 
        , dfa_all_states = 
        , dfa_all_symbols = 
        }

--}
--}
--}
--}
--}
--}
--}
--}
