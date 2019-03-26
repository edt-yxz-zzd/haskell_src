{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}




module Problem where
import Container_base_static
import System.Random

class Problem result problem | problem -> result where
class (Problem r p, Container r (RandomizedSolutions p))
    => OpRandomizedSolutions r p where
    type RandomizedSolutions p :: *
    randomized_solutions :: RandomGen g => p -> g -> RandomizedSolutions p

class (Problem r p, Ord v) => OptimizationProblem v r p where
    -- | p -> v where
    solution2value :: p -> r -> v
    -- default solution2value :: MinimizationProblem 
class (OptimizationProblem cost r p, OpCostFunction cost r)
    => MinimizationProblem cost r p
class Ord cost => OpCostFunction cost result | result -> cost where
    cost_function :: result -> cost




