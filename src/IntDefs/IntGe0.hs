
module IntDefs.IntGe0 (IntGe0) where


import Numeric.Natural
import IntDefs.IntGe
import Seed.Boxed
-- import Seed.UnsafeUnpack (unsafe_right)

--type UInt = Natural
type IntGe0 = Natural
xxx :: Num a => a
xxx = 0
instance IntGe Natural where
instance UIntGe Natural where
    labelled_uint_lower_bound = box xxx
    __private_uintge_constructor = makeUnsafeFromNatural id
    __private_uintge_destructor = id


