{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}




module IntDefs.IntDefs
    ( module IntDefs.IntGe
    , module IntDefs.IntDefs
    , module IntDefs.IntGe0
    , module IntDefs.IntGe1
    , module IntDefs.IntGe2
    , module IntDefs.IntGe3
    )
where
import IntDefs.IntGe
import IntDefs.IntGe0
import IntDefs.IntGe1
import IntDefs.IntGe2
import IntDefs.IntGe3


class (Integral i, IntGe i) => IntGeEx i where
instance (Integral i, IntGe i) => IntGeEx i where
class (Integral i, UIntGe i, IntGeEx i) => UIntGeEx i where
instance (Integral i, UIntGe i) => UIntGeEx i where



type UInt = IntGe0
type PInt = IntGe1
type QInt = IntGe2

type Int0x = IntGe0
type Int1x = IntGe1
type Int2x = IntGe2
type Int3x = IntGe3


