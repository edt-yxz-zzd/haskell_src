{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}


{-
    Elemnts Of The Theory Of Computation (2ed)(1998)(Harry Lewis)
    [page 341] 7.4: Coping with NP-completeness
    backtracking algorithm
        -- for "yes or no" problem, i.e. decision problem
    branch-and-bound
        -- for optimization problem
-}


module NP_BacktrackingAlgorithm where

import qualified Data.Set as S
import Data.Set (Set)
import Container_base_static
import Container_base_op (OpFilter(..), OpPartitionList(..))
import Data.Maybe (catMaybes, listToMaybe)
import Data.List (minimumBy, minimum)
import Prelude hiding (filter)
import Problem
import Explain





data QuickResult result
    = Solution result
    | Backtracking -- NoResult
    | NotObviously -- NotClearly

{-
class BacktrackingAlgorithm result problem | problem -> result where
    quick_heuristic_test :: problem -> QuickResult result
    branching :: problem -> Set problem
    backtracking_algorithm :: Ord problem => Set problem -> Maybe result
    backtracking_algorithm problems =
      case S.minView problems of
        Nothing -> Nothing
        Just (problem, problems') ->
          case quick_heuristic_test problem of
            Solution result -> Just result
            Backtracking -> f problems'
            NotObviously -> f $ S.union problems' $ branching problem
      where f = backtracking_algorithm
-}

{-
class Container a c => OpFilter a c where
    filter :: (a -> Bool) -> c -> c
        -- c from True
    filter_not :: (a -> Bool) -> c -> c
    filter_not f = filter (not . f)
class Container a c => OpPartitionList a c where
    partition_list :: (a -> Maybe r) -> c -> ([r], c)
        -- ([r] from Just r, c from Nothing)
-}













class Problem result problem => OpQuickHeuristicTest result problem where
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












class   ( OpQuickHeuristicTest result problem
        , MinimizationProblem cost result problem
        )
    => OpQuickHeuristicMinCost cost result problem where
    -- quick_heuristic_test return any result
    -- but quick_heuristic_min_cost return any result with min cost
    quick_heuristic_min_cost :: problem -> QuickResult (cost, result)

class (Ord cost, Ord lower, Explain lower cost)
    => OpCostAsLowerBound lower cost where
    cost_as_lower :: cost -> lower
    cost_as_lower = explain
class   ( Ord cost, Ord lower, Explain lower cost
        , OpCostAsLowerBound lower cost
        )
    => OpLowerBound lower cost problem | problem -> cost lower where
    lower_bound :: problem -> lower

result2bestsofar :: OpCostFunction cost result
    => result -> BestSoFar cost result
result2bestsofar result = BestSoFar (cost_function result) result

data BestSoFar cost result = BestSoFar cost result | Init_OO_Undefined
instance Eq cost => Eq (BestSoFar cost result) where
    BestSoFar cost1 _ == BestSoFar cost2 _ = cost1 == cost2
    Init_OO_Undefined == Init_OO_Undefined = True
    _ == _ = False
instance Ord cost => Ord (BestSoFar cost result) where
    BestSoFar cost1 _ `compare` BestSoFar cost2 _ = compare cost1 cost2
    compare Init_OO_Undefined Init_OO_Undefined = EQ
    compare Init_OO_Undefined _ = GT
    compare _ Init_OO_Undefined = LT
class   ( BacktrackingAlgorithm result problem problems
        , OpLowerBound lower cost problem
        , MinimizationProblem cost result problem
        , OpFilter problem problems
        , OpQuickHeuristicMinCost cost result problem
        )
    => BranchAndBound lower cost result problem problems where
    -- org_problem -> Maybe result
    -- org_problem -> (trivial partial_result, lower_bound, subproblem)
    -- problem = (partial_result, lower_bound, subproblem)
    -- problem -> lower_bound
    -- halt at:
    --      min cost $ all (full partial_result, cost, trivial subproblem)
    --          -> Just result
    --      all (partial_result, lower_bound, empty subproblem)
    --          -> Nothing

    branch_and_bound :: BestSoFar cost result -> problems -> Maybe result
    branch_and_bound best_so_far problems =
        case choose_to_branch problems of
        Nothing -> case best_so_far of
          BestSoFar cost result -> Just result
          _ -> Nothing
        Just (problem, old_problems) ->
          case partition_list part $ branching problem of
            (maybe_bestsofars, new_branches_) ->
              let best_so_far' = minimum . (best_so_far :)
                    $ catMaybes maybe_bestsofars
                  new_branches = flip ($) new_branches_ $
                    case best_so_far' of
                      Init_OO_Undefined -> id
                      BestSoFar cost _ -> filter $
                        \p -> lower_bound p < cost_as_lower cost
                  problems' = careless_union old_problems new_branches
              in  self_f best_so_far' problems'
      where
        self_f = branch_and_bound
        -- part :: problem -> Maybe (Maybe (BestSoFar cost result))
        part problem = case quick_heuristic_min_cost problem of
            Solution (cost, result) -> Just $ Just $ BestSoFar cost result
            Backtracking -> Just Nothing
            NotObviously -> Nothing

class Solve result init org_problem
    | org_problem -> result init, init -> org_problem where
    prepare :: org_problem -> init
    solve_impl :: init -> Maybe result
    solve :: org_problem -> Maybe result
    solve = solve_impl . prepare

class   ( BranchAndBound lower cost result problem problems
        , SolveByBacktrackingAlgorithm result problem problems org_problem
        )
    => SolveByBranchAndBound lower cost result problem problems org_problem
    where
    prepare_for_branch_and_bound :: org_problem -> problems
    prepare_for_branch_and_bound = prepare_for_backtracking_algorithm
    solve_by_branch_and_bound :: org_problem -> Maybe result
    solve_by_branch_and_bound =
        branch_and_bound Init_OO_Undefined . prepare_for_branch_and_bound








