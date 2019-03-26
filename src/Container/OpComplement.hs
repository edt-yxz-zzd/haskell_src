{-# LANGUAGE DefaultSignatures #-}
module Container.OpComplement
where

import Container.SetOps
import Container.OpEmpty
import Container.OpUniversal


class (OpEmpty s, OpUniversal s) => OpComplement s where
    complement :: s -> s
    -- (~) = complement -- no prefix op and (~) is not valid id
    default complement :: OpSetDifference s => s -> s
    complement = (universal \-\)


