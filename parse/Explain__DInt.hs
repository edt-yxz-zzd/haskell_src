{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Explain__DInt
where
import Explain
import DInt
import SeedUtils (justif)
import qualified Prelude as P -- hiding (or, and, not)
import Prelude hiding (or, and, not)



------------------ to/from DInt -- alias
class SafeTo DInt a => OpSafeToDInt a where
    unsafe_toDInt :: a -> DInt
    safe_toDInt :: a -> Maybe DInt
class To DInt a => OpToDInt a where
    toDInt :: a -> DInt
instance SafeTo DInt a => OpSafeToDInt a where
    unsafe_toDInt = unsafe_from
    safe_toDInt = safe_from
instance To DInt a => OpToDInt a where
    toDInt = from

class SafeFrom DInt a => OpSafeFromDInt a where
    unsafe_fromDInt :: DInt -> a
    safe_fromDInt :: DInt -> Maybe a
class From DInt a => OpFromDInt a where
    fromDInt :: DInt -> a
instance SafeFrom DInt a => OpSafeFromDInt a where
    unsafe_fromDInt = unsafe_from
    safe_fromDInt = safe_from
instance From DInt a => OpFromDInt a where
    fromDInt = from





