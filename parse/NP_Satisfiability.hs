{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

-- Satisfiability
-- Davis-Putnam procedure


{-
data Literal bv = PositiveLiteral bv | NegativeLiteral bv
type Clause bv = Set (Literal bv) -- NonemptySet (Literal bv)
type BooleanFormula_CNF bv = Set (Clause bv)
type TrueAssignment bv = bv -> Bool
satisfies :: TrueAssignment bv -> BooleanFormula_CNF bv -> Bool
satisfies ta bf = eval bf on ta
satisfiable :: BooleanFormula_CNF bv -> Bool
satisfiable bf = exist ta: ta `satisfies` bf
-}


import NP_BacktrackingAlgorithm
import Problem
import NP_LocalImprovement
import Count
import Container_base_static hiding (Set, null, take)
import qualified Container_base_static as C
-- import qualified ExplainEx as E
import ExplainEx (toInteger)
import Explain (From(..))
import SeedUtils (lift2, justif, blocksLenN)
import Data.List (maximumBy)
import System.Random




import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Prelude hiding ((+), (-), toInteger)
import qualified Prelude as P
import Container_static_instance__List

-- bv -- boolean variable
data Literal bv = NegativeLiteral bv | PositiveLiteral bv 
    deriving (Eq, Ord, Show, Read)
lbv2pair (PositiveLiteral bv) = (True, bv)
lbv2pair (NegativeLiteral bv) = (False, bv)
pair2lbv (False, bv) = NegativeLiteral bv
pair2lbv (True, bv) = PositiveLiteral bv
flip_literal (PositiveLiteral bv) = NegativeLiteral bv
flip_literal (NegativeLiteral bv) = PositiveLiteral bv

newtype Clause bv = Clause { unClause :: Set (Literal bv)}
    -- {} <==> False
    -- \/~

{- since:
instance Ord a => Ord (Set a) where
    compare s1 s2 = compare (toAscList s1) (toAscList s2)
we redefine compare by newtype clause
-}
instance Eq bv => Eq (Clause bv) where
    Clause a == Clause b = S.size a == S.size b && a == b
instance Ord bv => Ord (Clause bv) where
    Clause a `compare` Clause b =
        let s1 = S.size a
            s2 = S.size b
        in  if s1 /= s2 then compare s1 s2 else compare a b

to_lsls :: BooleanFormula_CNF bv -> [[Literal bv]]
to_lsls bf = map (S.toAscList . unClause) $ S.toAscList bf


type BooleanFormula_CNF bv = Set (Clause bv)
    -- {} <==> True
    -- /\~
type TrueAssignment bv = Map bv Bool -- bv -> Bool
satisfies__clause :: Ord bv => TrueAssignment bv -> [Literal bv]
            -> Maybe [Literal bv]
            -- Nothing <==> True
            -- Just [] <==> False
            -- Just [...] <==> unknown bv
satisfies__clause ta [] = Just []
satisfies__clause ta (a:ls) =
    let (b, v) = lbv2pair a
        maybe_b = M.lookup v ta
    in  case maybe_b of
    Just b' -> if b' == b then Nothing else f ls
    _ -> case f ls of
        Just ls' -> Just $ a:ls'
        _ -> Nothing
  where
    f = satisfies__clause ta


satisfies__formula
            :: Ord bv => TrueAssignment bv
            -> [[Literal bv]] -- BooleanFormula_CNF bv
            -> Maybe [[Literal bv]]
            -- Nothing <==> False
            -- Just [] <==> True
            -- Just [nonnull [..]] <==> unknown literals
satisfies__formula ta lsls =
    let lsls' = catMaybes $ map (satisfies__clause ta) lsls
    in  if any null lsls' then Nothing else Just lsls'
satisfies_ex :: Ord bv => TrueAssignment bv
    -> BooleanFormula_CNF bv -> Either [[Literal bv]] Bool
satisfies_ex ta bf = case satisfies__formula ta $ to_lsls bf of
    Nothing -> return False
    Just [] -> return True
    Just lsls -> Left lsls
satisfies :: Ord bv => TrueAssignment bv -> BooleanFormula_CNF bv -> Bool
satisfies ta bf = case satisfies_ex ta bf of
    Right b -> b
    Left lsls -> error "unknown literals"
num_satisfies :: Ord bv => TrueAssignment bv -> BooleanFormula_CNF bv
    -> Integer
num_satisfies ta bf = sum . map f $ to_lsls bf where
    f c = case g c of
        Nothing -> 1 -- True
        Just [] -> 0 -- False
        _ -> error "unknown literals"
    g = satisfies__clause ta
num_clauses :: BooleanFormula_CNF bv -> Integer
num_clauses bf = P.toInteger $ S.size bf
all_literals :: Ord bv => BooleanFormula_CNF bv -> [bv]
all_literals bf = S.toAscList . S.fromList
    . map (snd . lbv2pair) . concat $ to_lsls bf



assign_one :: Ord bv =>
    Bool -> bv -> BooleanFormula_CNF bv -> BooleanFormula_CNF bv
assign_one b v bf = S.map g $ S.filter f bf where
    f (Clause c) = not $ S.member lbv c -- ignore True Cause
    g (Clause c) = Clause $ S.delete n_lbv c -- may yield [] i.e. False Clause
    lbv = pair2lbv (b, v)
    n_lbv = pair2lbv (not b, v)


satisfiable :: Ord bv => BooleanFormula_CNF bv -> Bool
satisfiable bf = -- undefined -- exist ta: ta `satisfies` bf
    Nothing /= solve_by_backtracking_algorithm bf

{-
    org_problem/subproblem - BooleanFormula_CNF bv
    result/partial_result - TrueAssignment bv
    problem - (partial_result, subproblem)


instance    ( result~TrueAssignment bv
            , org_problem~BooleanFormula_CNF bv
            , subproblem~org_problem
            , problem~(result, subproblem)
            , problems~[problem]
            , Ord bv
            )

-}


type Result_Sat bv = TrueAssignment bv
type PResult_Sat bv = Result_Sat bv
type OrgP_Sat bv = BooleanFormula_CNF bv
type SubP_Sat bv = OrgP_Sat bv
type P_Sat bv = (PResult_Sat bv, SubP_Sat bv)
type Ps_Sat bv = [P_Sat bv]


instance Problem (Result_Sat bv) (P_Sat bv)

instance OpQuickHeuristicTest (Result_Sat bv) (P_Sat bv) where
    quick_heuristic_test (ta, bf) = if S.null bf then Solution ta else
        -- if any (S.null . unClause) $ S.toList bf
        if any (S.null . unClause) . take 1 $ S.toAscList bf
        then Backtracking else
        NotObviously
instance Ord bv
    => BacktrackingAlgorithm (Result_Sat bv) (P_Sat bv) (Ps_Sat bv) where
    branching (r, p) = case S.minView p of
        Nothing -> [(r, p)] -- solution!
        Just (Clause c, p') -> case S.maxView c of
            Nothing -> [] -- fail
            Just (lbv, _) -> let (b, v) = lbv2pair lbv in
                [f r p b v, f r p (not b) v]
                -- ^^ remove clause   ^^ remove lbv
                -- [f r p (not b) v, f r p b v]
                -- ^^ remove lbv   ^^ remove clause
      where
        f r p b v = (M.insert v b r, assign_one b v p)

    choose_to_branch (a:ls) = Just (a, ls)
    choose_to_branch _ = Nothing
    careless_union = flip (++)

instance Ord bv => SolveByBacktrackingAlgorithm
    (Result_Sat bv) (P_Sat bv) (Ps_Sat bv) (OrgP_Sat bv) where
    prepare_for_backtracking_algorithm org = [(M.empty, org)]


_p = PositiveLiteral
_n = NegativeLiteral
_f = flip_literal
_mk name = (_p name, _n name)
(x, _x) = _mk "x"
(y, _y) = _mk "y"
(z, _z) = _mk "z"

{-
(+) :: String -> Literal String
(+) = _p
(-) :: String -> Literal String
(-) = _n

w = "w"
-- ww = [w+, w-] -- fail
-- A section must be enclosed in parentheses thus: (w +)
-}

plain2encoded :: Ord bv => [[Literal bv]] -> BooleanFormula_CNF bv
plain2encoded lsls = S.fromList $ map (Clause . S.fromList) lsls

_solve plain_instance = solve_by_backtracking_algorithm $
    plain2encoded plain_instance
_check plain_instance ans = ans == _solve plain_instance
_mk_dict falses trues = M.fromList $
    map (, False) falses ++ map (, True) trues
_mk_just_ans falses trues = Just $ _mk_dict falses trues
_strs2just_ans fs ts = _mk_just_ans (words fs) (words ts)
_sat = satisfiable . plain2encoded


plain_instance1 = [[x, y, z], [_x, y], [_y, z], [_z, x], [_x, _y, _z]]
ans1 = Nothing
b1 = _check plain_instance1 ans1

plain_instance2 = [[x, y, z], [_x, y], [_y, z], [_z, x], [x, _y, _z]]
ans2 = _strs2just_ans "" "x y z"
    -- Just $ M.fromList [("x", True), ("y", True), ("z", True)]
b2 = _check plain_instance2 ans2

b = b1 && b2 && not (_sat plain_instance1) && _sat plain_instance2
main = do
    print b
    r1 <- _solve' 3 plain_instance1
    r2 <- _solve' 3 plain_instance2
    print r1
    print r2

_solve' n plain_instance = solve_by_local_improve n $
    plain2encoded plain_instance







































------------ MAX SAT

class   ( Ord bv, DynCount bf, KeyType bf ~ Clause bv
        -- , Monoid (Sum (ValueType bf))
        , OpAdd (ValueType bf)
        , OpMinView (KeyType bf, ValueType bf) bf
        , OpNull (KeyType bf, ValueType bf) bf
        )
    => BooleanFormula_CNF_Ex bv bf
assign_one_ex :: BooleanFormula_CNF_Ex bv bf  =>
    Bool -> bv -> bf -> bf
assign_one_ex b v bf = map_key (+) g $ filter_key f bf where
    f (Clause c) = not $ S.member lbv c
    g (Clause c) = Clause $ S.delete n_lbv c
    lbv = pair2lbv (b, v)
    n_lbv = pair2lbv (not b, v)


newtype MaxSatInstance bv =
    MaxSatInstance { unMaxSatInstance :: BooleanFormula_CNF bv }

data SubP_MaxSat bv

{-
instance    ( result~(goat, TrueAssignment bv)
            , total ~ Integer -- num of clauses
            , goat ~ Integer -- num of clauses satisfied
            --, cost ~ Integer -- num of removed clauses
            , lower ~ Integer -- num of removed clauses - total <= 0
            , partial_result ~ (lower, TrueAssignment bv)
            , org_problem ~ MaxSatInstance bv
            , subproblem ~ SubP_MaxSat bv -- BooleanFormula_CNF bv
            , BooleanFormula_CNF_Ex bv subproblem
            , problem ~ (partial_result, subproblem)
            )
    => Problem result problem
-}

instance BooleanFormula_CNF_Ex bv (SubP_MaxSat bv)
    => Problem (Result_MaxSat bv) (P_MaxSat bv)

type Goat_MaxSat = Integer
type Total_MaxSat = Integer
type Lower_MaxSat = Integer
type Cost_MaxSat = Integer
type Result_MaxSat bv = (Goat_MaxSat, TrueAssignment bv)
type PResult_MaxSat bv = (Lower_MaxSat, TrueAssignment bv)
type OrgP_MaxSat bv = MaxSatInstance bv
--type SubP_MaxSat bv = BooleanFormula_CNF bv
type P_MaxSat bv = (PResult_MaxSat bv, SubP_MaxSat bv)
type Ps_MaxSat bv = [P_MaxSat bv]

max_sat_partial2result :: PResult_MaxSat bv -> Result_MaxSat bv
max_sat_partial2result (lower, ta) = (goat, ta) where
    goat = -lower

instance --Problem (Result_MaxSat bv) (P_MaxSat bv)
    BooleanFormula_CNF_Ex bv (SubP_MaxSat bv)
    => OpQuickHeuristicTest (Result_MaxSat bv) (P_MaxSat bv) where
    quick_heuristic_test ((lower, ta), bf) =
        if C.null bf then Solution (-lower, ta) else
        --if any (S.null . unClause) . take 1 $ S.toAscList bf
        --then Backtracking else
        NotObviously
instance (Ord bv, BooleanFormula_CNF_Ex bv (SubP_MaxSat bv))
    => BacktrackingAlgorithm
        (Result_MaxSat bv) (P_MaxSat bv) (Ps_MaxSat bv) where
    branching (r, p) = case minView p of
        Nothing -> [(r, p)] -- solution!
        Just ((Clause c, count), p') -> case S.maxView c of
            Nothing -> -- [] -- fail
                let (lower, ta) = r in [((lower P.+ toInteger count, ta), p')]
            Just (lbv, _) -> let (b, v) = lbv2pair lbv in
                [f r p b v, f r p (not b) v]
                -- ^^ remove clause   ^^ remove lbv
                -- [f r p (not b) v, f r p b v]
                -- ^^ remove lbv   ^^ remove clause
      where
        f (lower, ta) p b v = ((lower, M.insert v b ta), assign_one_ex b v p)

    choose_to_branch (a:ls) = Just (a, ls)
    choose_to_branch _ = Nothing
    careless_union = flip (++)

instance (Ord bv, BooleanFormula_CNF_Ex bv (SubP_MaxSat bv))
    => SolveByBacktrackingAlgorithm
        (Result_MaxSat bv) (P_MaxSat bv) (Ps_MaxSat bv) (OrgP_MaxSat bv)
        where
    prepare_for_backtracking_algorithm (MaxSatInstance bf) =
        [((-total, M.empty), bf_ex)]
        where
            total = P.toInteger $ S.size bf
            bf_ex = fromList . map (, unsafe_from (1::Integer)) $ S.toList bf

























instance OpCostFunction Cost_MaxSat (Result_MaxSat bv) where
    cost_function (goat, _) = -goat
instance BooleanFormula_CNF_Ex bv (SubP_MaxSat bv)
    => OptimizationProblem
        (Cost_MaxSat) (Result_MaxSat bv) (P_MaxSat bv) where
    solution2value _ = cost_function
instance BooleanFormula_CNF_Ex bv (SubP_MaxSat bv)
    => MinimizationProblem
        (Cost_MaxSat) (Result_MaxSat bv) (P_MaxSat bv) where
instance BooleanFormula_CNF_Ex bv (SubP_MaxSat bv)
    => OpQuickHeuristicMinCost
        (Cost_MaxSat) (Result_MaxSat bv) (P_MaxSat bv) where
    quick_heuristic_min_cost p = case quick_heuristic_test p of
        Solution cr@(c, r) -> Solution (c, cr)
        Backtracking -> Backtracking
        NotObviously -> NotObviously
instance OpCostAsLowerBound (Lower_MaxSat) (Cost_MaxSat) where
    cost_as_lower = id
instance OpLowerBound (Lower_MaxSat) (Cost_MaxSat) (P_MaxSat bv) where
    lower_bound ((lower, _ta), _subp) = lower
instance BooleanFormula_CNF_Ex bv (SubP_MaxSat bv)
    => BranchAndBound Lower_MaxSat Cost_MaxSat
        (Result_MaxSat bv) (P_MaxSat bv) (Ps_MaxSat bv) where
instance BooleanFormula_CNF_Ex bv (SubP_MaxSat bv)
    => SolveByBranchAndBound Lower_MaxSat Cost_MaxSat
        (Result_MaxSat bv) (P_MaxSat bv) (Ps_MaxSat bv) (OrgP_MaxSat bv)
        where

































---------------- by NP_LocalImprovement

{-
    org_problem : BooleanFormula_CNF bv
    org_result : TrueAssignment bv
    result : (goat, org_result)
    problem : (literals, total_clauses, org_problem)

-}

type OrgP_MaxSat_li bv = BooleanFormula_CNF bv
type P_MaxSat_li bv = ([bv], Integer, OrgP_MaxSat_li bv)
type Result_MaxSat_li bv = (Goat_MaxSat, TrueAssignment bv)
    -- == Result_MaxSat bv

type Pair a = (a, a)
type Pairs a = [Pair a]
instance Eq bv
    => RelationAbout (Result_MaxSat_li bv) (P_MaxSat_li bv) where
    type Relation (Result_MaxSat_li bv) (P_MaxSat_li bv) =
        Pairs (Result_MaxSat_li bv)
    relation = undefined

ta2result__max_sat_li bf ta = (num_satisfies ta bf, ta)
ta_ls2results__max_sat_li bf = map (ta2result__max_sat_li bf)

instance Ord bv => OpNeighborhoodOf (Result_MaxSat_li bv) (P_MaxSat_li bv) where
    type Neighborhood (Result_MaxSat_li bv) (P_MaxSat_li bv) =
        [] (Result_MaxSat_li bv)
    neighborhood_of p@(literals, total, bf) r@(goat, ta) = rs where
        ta_ls = [change bv | bv <- literals]
        change bv = M.adjust not bv ta
        rs = ta_ls2results__max_sat_li bf ta_ls
        -- rs = map (\ta->(eval ta, ta)) ta_ls
        -- eval ta = num_satisfies ta bf
instance Problem (Result_MaxSat_li bv) (P_MaxSat_li bv)
instance OptimizationProblem
        Cost_MaxSat (Result_MaxSat_li bv) (P_MaxSat_li bv) where
    solution2value = undefined
--instance OpCostFunction Cost_MaxSat (Result_MaxSat_li bv) where
--    cost_function = fst
instance MinimizationProblem
    Cost_MaxSat (Result_MaxSat_li bv) (P_MaxSat_li bv) where
instance Ord bv => LocalImprovement
    Cost_MaxSat (Result_MaxSat_li bv) (P_MaxSat_li bv) where
    any_better_neighbor p@(literals, total, bf) r@(goat, ta) = r'
        -- best
      where
        ns = neighborhood_of p r
        cmp = (lift2 fst compare)
        best = maximumBy cmp ns
        r' = justif (not (null ns) && cmp best r == GT) best

instance Ord bv
    => OpRandomizedSolutions (Result_MaxSat_li bv) (P_MaxSat_li bv)
    where
    type RandomizedSolutions (P_MaxSat_li bv) = [Result_MaxSat_li bv]
    randomized_solutions p@(literals, total, bf) g = rs where
        n = C.length literals
        bools = randoms g
        boolss = blocksLenN n bools
        ta_ls = [M.fromList $ zip literals bools | bools <- boolss]
        rs = ta_ls2results__max_sat_li bf ta_ls

instance Ord bv => SolveByLocalImprovement
    Cost_MaxSat (Result_MaxSat_li bv) (P_MaxSat_li bv) (OrgP_MaxSat_li bv)
    where
    prepare_for_local_improvement bf =
        (all_literals bf, num_clauses bf, bf)



{-
class   ( OpNeighborhoodOf result problem
        , MinimizationProblem cost result problem
        --, Neighborhood 
        )
    => LocalImprovement cost result problem where
    any_better_neighbor :: problem -> result -> Maybe result
        -- if Just r = this_f p r0 then
        --    assert cost_func r < cost_func r0
        --    assert is_neighbor p (r0, r)
    local_improve :: problem -> result -> result
        -- return local optimum
    local_improve = local_improveBy any_better_neighbor
class LocalImprovement c r p => OpAnyBestNeighbor c r p where
    any_best_neighbor :: p -> r -> Maybe r
    local_improve__best :: p -> r -> r
    local_improve__best = local_improveBy any_best_neighbor

class   ( LocalImprovement c r p
        , OpRandomizedSolutions r p
        , Iterable r (RandomizedSolutions p)
        )
    => SolveByLocalImprovement c r p org_p | org_p -> p where
    prepare_for_local_improvement :: org_p -> p
    solve_by_local_improve :: Integer -> org_p -> Maybe r
    solve_by_local_improve n org = r where
        p = prepare_for_local_improvement org
        rs = take n $ randomized_solutions p
        rs' = map (local_improve p) rs
        r = minByKey cost_function rs'


class (OpMember (a, a) (Relation a env)) => RelationAbout a env where
    type Relation a env :: *
    relation :: env -> Relation a env
    is_in_relation :: env -> (a, a) -> Bool
    is_in_relation env pair = member pair $ relation env
class (RelationAbout a g, OpMember a (Neighborhood a g))
    => OpNeighborhoodOf a g where
    type Neighborhood a g :: *
    is_neighbor :: g -> (a, a) -> Bool
    is_neighbor = is_in_relation
    neighborhood_of :: g -> a -> Neighborhood a g
        -- neighborhood a = {b | is_neighbor (a, b)}


--}
--}
--}
--}
--}

