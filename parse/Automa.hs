{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Automa where

import Container
class DeterministicAutomaton tst sym a | a -> tst sym where
    -- tst - total_state
    -- sym - input symbol;
    --       one way automaton (i.e. read from left to right)
    -- method_ - like method, but using implict parameter self
    -- type TotalState a :: *
    initial_tst :: a -> tst
    is_final_tst :: a -> tst -> Bool
    is_accepted_tst :: a -> tst -> Bool -- accepted -->> final
    transition :: a -> sym -> tst -> tst
        -- why not a -> tst -> sym -> tst?
        --      foldl (flip transition_) tst syms
        --      v.s. transition_ sym2 . transition_ sym1 $ tst
        --           foldr transition_ tst (reverse syms)

    transition_, (-<<) :: (?self :: a) => sym -> tst -> tst
    transition_ = transition ?self

infixr 1 -<< -- like =<<


class   ( DeterministicAutomaton tst sym a
        , DynSet pst (PartialStates a)
        , MergeCC pst (PartialStates a) (PartialStates a))
    => NonDeterministicAutomaton pst tst sym a | a -> pst where
    -- pst - partial_state
    -- psts - partial_state_set - incomplete state
    -- mini defines:
    --      partial_transition
    --      null_transition
    --      complete_psts
    --      unpack_tst
    type PartialStates a :: *



    initial_psts :: a -> PartialStates a
    is_final_pst :: a -> pst -> Bool
    partial_transition :: a -> sym -> pst -> PartialStates a
    partial_transition_psts
        :: a -> sym -> PartialStates a -> PartialStates a
    partial_transition_psts_closure
        :: a -> sym -> PartialStates a -> PartialStates a
        -- unpack_tst . transition_ sym . complete_psts_
        -- === partial_transition_psts_closure_ sym
    complete_psts :: a -> PartialStates a -> tst
        -- == box . null_transition_psts_closure_
    unpack_tst :: a -> tst -> PartialStates a
        -- == unbox
        -- id === complete_psts_ . unpack_tst_
        -- psts |<=| unpack_tst_ (complete_psts_ psts)
        -- {} |==| unpack_tst_ (complete_psts_ {})
    null_transition :: a -> pst -> PartialStates a
    null_transition_psts_closure
        :: a -> PartialStates a -> PartialStates a
        -- unpack_tst_ . complete_psts_
        -- === null_transition_psts_closure_






    partial_transition_psts a sym =
        merge_countable_countable (partial_transition a sym)
    partial_transition_psts_closure a sym =
        null_transition_psts_closure a . partial_transition_psts a sym
        . null_transition_psts_closure a
    null_transition_psts_closure a =
        merge_closureBy set_eq $ null_transition a





    partial_transition_ :: (?self :: a) => sym -> pst -> PartialStates a
    complete_psts_ :: (?self :: a) => PartialStates a -> tst
    unpack_tst_ :: (?self :: a) => tst -> PartialStates a
    null_transition_ :: (?self :: a) => pst -> PartialStates a
    null_transition_psts_closure_
        :: (?self :: a) => PartialStates a -> PartialStates a

    partial_transition_ = partial_transition ?self
    complete_psts_ = complete_psts ?self
    unpack_tst_ = unpack_tst ?self
    null_transition_ = null_transition ?self
    null_transition_psts_closure_ = null_transition_psts_closure ?self



