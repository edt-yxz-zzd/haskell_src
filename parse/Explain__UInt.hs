{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Explain__UInt
where
import Explain
import UInt
import SeedUtils (justif)
import qualified Prelude as P -- hiding (or, and, not)
import Prelude hiding (or, and, not)



------------------ to/from UInt -- alias
class SafeTo UInt a => OpSafeToUInt a where
    unsafe_toUInt :: a -> UInt
    safe_toUInt :: a -> Maybe UInt
class To UInt a => OpToUInt a where
    toUInt :: a -> UInt
instance SafeTo UInt a => OpSafeToUInt a where
    unsafe_toUInt = unsafe_from
    safe_toUInt = safe_from
instance To UInt a => OpToUInt a where
    toUInt = from

class SafeFrom UInt a => OpSafeFromUInt a where
    unsafe_fromUInt :: UInt -> a
    safe_fromUInt :: UInt -> Maybe a
class From UInt a => OpFromUInt a where
    fromUInt :: UInt -> a
instance SafeFrom UInt a => OpSafeFromUInt a where
    unsafe_fromUInt = unsafe_from
    safe_fromUInt = safe_from
instance From UInt a => OpFromUInt a where
    fromUInt = from





