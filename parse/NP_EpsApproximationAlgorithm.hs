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
    Special Cases
    eps-Approximation Algorithm -- epsilon
        -- for optimization problem
        p - an instance of the problem
        e A - the worst-case relative error of algorithm A
            nonnegative real value
            unless P=NP, e A =/= 0
        opt - NP-complete - e.g. branch-and-bound
        opt p - cost/goal value of an optimum solution
            positive integer?
            positive value? nonzero value?
            integer? rational? real?
        A - a polynomial algorithm - eps-approximation algorithm
            polynomial about 1/eps and input size n
            1/eps may be 2^n when eps -> 0
        A p - cost/goal value of some solution
        for all p: |(opt p - A p) / opt p| <= e A
    approximate special cases
-}


module NP_EpsApproximationAlgorithm where

import Problem --(OpCostFunction)
import qualified Data.Set as S
import Data.Set (Set)
import Container_base_static
import Data.Maybe (catMaybes, listToMaybe)
import Data.List (minimumBy, minimum)
import Prelude hiding (filter)
import ExplainEx





class   ( Container e (ErrorsStaticType problem)
        , Container e (ErrorsDynamicType problem)
        --, PositiveRational e
        , NonNegativeRational e
        --, OpCostFunction cost result
        , MinimizationProblem cost result problem
        , RationalBut0 cost -- since RealBut0 without Ord!
        , Optional result (ApproxResult problem)
        , Container (SpecialCase problem) (SpecialCases problem)
        )
    => EpsApproximationAlgorithm e cost result problem
    | problem -> result e where
    type ErrorsStaticType problem :: *
    type ErrorsDynamicType problem :: * -- why? special cases ...
    type SpecialCases problem :: *
    type SpecialCase problem :: *
    available_errors__static :: ErrorsStaticType problem
    available_errors__dynamic :: problem -> ErrorsDynamicType problem
    special_cases :: problem -> SpecialCases problem

    type ApproxResult problem :: *
    -- @e : exist eps: e >= eps |<-| errors_dynamic |>=| errors_static
    approximation :: e -> problem -> ApproxResult problem

class   ( EpsApproximationAlgorithm e cost result problem
        , Explain (Either Value_TheURationalSet Value_ThePRationalSet)
            (ErrorsStaticType problem) -- except 0
        , Singleton result (ApproxResult problem)
        )
    => FullyApproximable e cost result problem where
class   ( EpsApproximationAlgorithm e cost result problem
        , NonNull e (ErrorsStaticType problem)
        )
    => PartlyApproximable e cost result problem where
class   ( EpsApproximationAlgorithm e cost result problem
        , Empty e (ErrorsStaticType problem)
        --, Empty result (ApproxResult problem)
        )
    => InApproximable e cost result problem where














