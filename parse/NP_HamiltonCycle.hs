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

-- HamiltonCycle



import NP_BacktrackingAlgorithm
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Prelude hiding ((+), (-))
import Container_static_instance__List
import Graph






{-
    org_problem - graph
    subproblem - (Set vtx, graph)
    result/partial_result - path
    problem - (partial_result, subproblem)
-}


{-

class OpQuickHeuristicTest result problem | problem -> result where
    quick_heuristic_test :: problem -> QuickResult result
class   ( Container problem problems
        , Iterable problem problems
        , OpPartitionList problem problems
        , OpQuickHeuristicTest result problem
        )
    => BacktrackingAlgorithm result problem problems where
    -- org_problem -> Maybe result
    -- org_problem -> (trivial partial_result, subproblem)
    -- problem = (partial_result, subproblem)
    -- halt at:
    --      any (full partial_result, trivial subproblem)
    --          full partial_result -> Just result
    --      all (partial_result, empty subproblem)
    --          () -> Nothing
    branching :: problem -> problems
    choose_to_branch :: problems -> Maybe (problem, problems)
    careless_union :: problems -> problems -> problems
        -- c = careless_union a b
        -- <==> member x a \/ member x b <--> member x c
        -- careless_union old_problems new_branches
    backtracking_algorithm :: problems -> Maybe result
    backtracking_algorithm problems =
        choose_to_branch problems >>= \(problem, old_problems) ->
          case partition_list part $ branching problem of
            (maybe_results, new_branches) ->
              let problems_ = careless_union old_problems new_branches
                  r = self_f problems_
                  maybe_result = listToMaybe $ catMaybes maybe_results
              in  maybe r Just maybe_result
      where
        self_f = backtracking_algorithm
        -- part :: problem -> Maybe (Maybe result)
        part problem = case quick_heuristic_test problem of
            Solution result -> Just $ Just result
            Backtracking -> Just Nothing
            NotObviously -> Nothing

class BacktrackingAlgorithm result problem problems
    => SolveByBacktrackingAlgorithm result problem problems org_problem
    | org_problem -> problems where
    prepare_for_backtracking_algorithm :: org_problem -> problems
    solve_by_backtracking_algorithm :: org_problem -> Maybe result
    solve_by_backtracking_algorithm =
        backtracking_algorithm . prepare_for_backtracking_algorithm


-}


instance    ( result~Path_Vtc g
            , org_problem~g
            , subproblem~(Set (Vtx g), org_problem)
            , problem~(result, subproblem)
            , Ord (Vtx g)
            , OpNumVertices g
            )
    => OpQuickHeuristicTest result problem where
    quick_heuristic_test (ta, bf) = if S.null bf then Solution ta else
        if any (S.null . unClause) $ S.toList bf then Backtracking else
        NotObviously
instance    ( result~TrueAssignment bv
            , org_problem~BooleanFormula_CNF bv
            , subproblem~org_problem
            , problem~(result, subproblem)
            , problems~[problem]
            , Ord bv
            )
    => BacktrackingAlgorithm result problem problems where
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

instance    ( result~TrueAssignment bv
            , org_problem~BooleanFormula_CNF bv
            , subproblem~org_problem
            , problem~(result, subproblem)
            , problems~[problem]
            , Ord bv
            )
    => SolveByBacktrackingAlgorithm result problem problems org_problem
    where
    prepare_for_backtracking_algorithm org = [(M.empty, org)]


_p = PositiveLiteral
_n = NegativeLiteral
_f = flip_literal
_mk name = (_p name, _n name)
(x, _x) = _mk "x"
(y, _y) = _mk "y"
(z, _z) = _mk "z"

(+) :: String -> Literal String
(+) = _p
(-) :: String -> Literal String
(-) = _n

w = "w"
-- ww = [w+, w-] -- fail
-- A section must be enclosed in parentheses thus: (w +)

plain2encoded :: Ord bv => [[Literal bv]] -> BooleanFormula_CNF bv
plain2encoded lsls = S.fromList $ map (Clause . S.fromList) lsls

_solve plain_instance = solve_by_backtracking_algorithm $
    plain2encoded plain_instance
_check plain_instance ans = ans == _solve plain_instance
_mk_dict falses trues = M.fromList $
    map (, False) falses ++ map (, True) trues
_mk_just_ans falses trues = Just $ _mk_dict falses trues
_strs2just_ans fs ts = _mk_just_ans (words fs) (words ts)

plain_instance1 = [[x, y, z], [_x, y], [_y, z], [_z, x], [_x, _y, _z]]
ans1 = Nothing
b1 = _check plain_instance1 ans1

plain_instance2 = [[x, y, z], [_x, y], [_y, z], [_z, x], [x, _y, _z]]
ans2 = _strs2just_ans "" "x y z"
    -- Just $ M.fromList [("x", True), ("y", True), ("z", True)]
b2 = _check plain_instance2 ans2

b = b1 && b2
main = do
    print b


--}
--}




