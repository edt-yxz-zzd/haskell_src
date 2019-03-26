{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module ADT.IArrowCount
where

import Control.Arrow
import Seed.ArrowOps (execA)

class (Arrow arr, Enum (ArrowCount arr)) => IArrowCount arr where
    type ArrowCount arr
    -- fetch current count and succ underlying counter
    tickA :: arr x (ArrowCount arr)
    default tickA
        :: (OpGetCountA arr, OpSetCountA arr)
        => arr x (ArrowCount arr)
    tickA = get_countA >>> execA (succ ^>> set_countA)
class (IArrowCount arr) => OpGetCountA arr where
    -- fetch current count
    get_countA :: arr x (ArrowCount arr)
class (IArrowCount arr) => OpSetCountA arr where
    -- store as current count
    set_countA :: arr (ArrowCount arr) ()


