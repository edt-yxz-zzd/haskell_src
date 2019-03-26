{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


module RE.IDFA where

import Data.Set (Set)
import qualified Data.Set as S

-- st : state
-- sym : symbol, terminal
class IDFA a where
    type State a :: *
    type Symbol a :: *
    -- total state
    transition :: a -> State a -> Symbol a -> State a
        -- Map st (AutoMergeRangeMap sym_rng st)
    is_final :: a -> State a -> Bool
    is_error :: a -> State a -> Bool
    initial_state :: a -> State a
