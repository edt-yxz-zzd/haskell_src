{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- LANGUAGE ExistentialQuantification #-}
{- LANGUAGE GADTs #-}
{- LANGUAGE DefaultSignatures #-}




module Bijection.SubsetBijection

where

import Seed.Boxed
import Lambda.Subset
{-
import Bijection.Bijection

class Bijection a => BijectionWith a where
    type BijectionWithConfig a :: *
    get_bijection_config :: a -> BijectionFrom a -> BijectionWithConfig a
    verify_config :: a -> BijectionWithConfig a -> Bool
-}
class (Subset (SubsetBijectionFrom a), Subset (SubsetBijectionTo a))
    => SubsetBijection a where
    type SubsetBijectionFrom a :: *
    type SubsetBijectionTo a :: *
    -- forward_constraint a . backward_constraint a === id
    -- forward_subset a . backward_subset a === id
    -- get_subset_constraint a . forward_subset a
    --  === forward_constraint a . get_subset_constraint a
    forward_constraint
        :: a
        -> SubsetConstraint (SubsetBijectionFrom a)
        -> SubsetConstraint (SubsetBijectionTo a)
    backward_constraint
        :: a
        -> SubsetConstraint (SubsetBijectionTo a)
        -> SubsetConstraint (SubsetBijectionFrom a)
    forward_subset :: a -> SubsetBijectionFrom a -> SubsetBijectionTo a
    backward_subset :: a -> SubsetBijectionTo a -> SubsetBijectionFrom a


data SwapPair a b
swap_pair = undefined
instance (Subset a, Subset b) => SubsetBijection (SwapPair a b) where
    type SubsetBijectionFrom (SwapPair a b) = (a, b)
    type SubsetBijectionTo (SwapPair a b) = (b, a)
    forward_constraint _ (ca, cb) = (cb, ca)
    backward_constraint _ (cb, ca) = (ca, cb)
    forward_subset _ (a, b) = (b, a)
    backward_subset _ (b, a) = (a, b)

data SwapEither a b






data LiftedFirst a b

instance (Subset a, Subset b) => SubsetBijection (LiftedValue a b) where
    type SubsetBijectionFrom (LiftedValue a b) = (a, b)
    type SubsetBijectionTo (LiftedValue a b) = LiftedValue a b
    forward_constraint = mkLiftedConstraint
    backward_constraint = unLiftedConstraint
    forward_subset = 






--constraint


