{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Container.OpLen
where
import Container.IContainer
import Container.OpEmpty
import IntDefs.IntDefs (UInt)
import Explain.ExplainBase
import IntDefs.Explain__Integer


class (IContainer a) => OpUnsafeLen a where
    -- not OpIsEmpty
    -- error or dead loop
    unsafe_len :: a -> UInt
    default unsafe_len :: OpLen a => a -> UInt
    unsafe_len = len
class (OpUnsafeLen a, OpIsEmpty a) => OpLenIfLe a where
    len_if_le :: Integer -> a -> Maybe UInt
    len_le :: Integer -> a -> Bool
    len_lt :: Integer -> a -> Bool
    -- {-# MINIMAL len_if_le #-}
    len_le i = maybe False (const True) . len_if_le i
    len_lt i = len_le (i-1)
    default len_if_le :: OpLen a => Integer -> a -> Maybe UInt
    len_if_le i a = safe_from i >>= \u ->
        let n = len a in if n <= u then Just n else Nothing
class (FiniteContainer a, OpLenIfLe a)
    => OpLen a where
    -- or call me Sized
    len :: a -> UInt

{-
class (OpLenEx a, OpDynTheOnlyValue (LenExResult a))
    => StaticSized a where
    static_len :: Value_Ignore_Just a UInt
    static_len = Value_Ignore_Just $
        explain (the_only_value :: LenExResult a)
-}



