{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


module RE.INDFA where

import Data.Set (Set)
import qualified Data.Set as S

class Ord (PartialState a) => INDFA a where
    type PartialState a :: *
    type SymbolRange a :: *
    -- partial state
    direct_transition :: a -> PartialState a -> SymbolRange a -> Set (PartialState a)
    null_transition :: a -> PartialState a -> Set (PartialState a)
        -- Map st (AutoMergeRangeMap sym_rng st)
    is_final_partial_state :: a -> PartialState a -> Bool
    initial_partial_states :: a -> Set (PartialState a)
null_transition_step1
    :: INDFA a => a -> PartialState a -> Set (PartialState a)
null_transition_step1_set, null_transition_complete
    :: INDFA a => a -> Set (PartialState a) -> Set (PartialState a)
null_transition_step1 a st = S.insert st $ null_transition a st
null_transition_step1_set a sts = S.unions
    . map (null_transition_step1 a) $ S.toList sts
null_transition_complete a sts = r where
    sts' = null_transition_step1_set a sts'
    r = if S.size sts == S.size sts' then sts else
            null_transition_complete a sts'
initial_states_complete :: INDFA a => a -> Set (PartialState a)
initial_states_complete a =
    null_transition_complete a $ initial_partial_states a
direct_transition_set
    :: INDFA a => a -> Set (PartialState a) -> SymbolRange a
    -> Set (PartialState a)
direct_transition_set a sts rng = S.unions
    . map (\st -> direct_transition a st rng) $ S.toList sts
transition_complete2complete
    :: INDFA a => a -> Set (PartialState a) -> SymbolRange a
    -> Set (PartialState a)
transition_complete2complete a sts rng = null_transition_complete a
    $ direct_transition_set a sts rng


