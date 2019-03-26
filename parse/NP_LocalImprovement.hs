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
    local improvement
        -- for optimization problem with a initial solution
-}


module NP_LocalImprovement where
-- import NP_BacktrackingAlgorithm (OpCostFunction, MinimizationProblem)
import Problem
import Container_base_static
import SeedUtils (fromInteger, minByKey)
import Prelude hiding (fromInteger, take)
import System.Random


{-
import qualified Data.Set as S
import Data.Set (Set)
import Container_base_op (OpFilter(..), OpPartitionList(..))
import Data.Maybe (catMaybes, listToMaybe)
import Data.List (minimumBy, minimum)
import Prelude hiding (filter)
-}


local_improveBy :: (p -> r -> Maybe r) -> (p -> r -> r)
local_improveBy better_f p initial_solution = f initial_solution where
    f r = maybe r f $ better_f p r

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
    solve_by_local_improve
        :: Integer -> org_p -> IO (Maybe r)
    solve_by_local_improve n p =
        getStdGen >>= return . solve_by_local_improve_ex n p
    solve_by_local_improve_ex
        :: RandomGen g => Integer -> org_p -> g -> Maybe r
    solve_by_local_improve_ex n org g = r where
        p = prepare_for_local_improvement org
        rs = take n $ randomized_solutions p g
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

