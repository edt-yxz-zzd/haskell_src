{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Explain__PInt
where
import Explain
import PInt
import SeedUtils (justif)
import qualified Prelude as P -- hiding (or, and, not)
import Prelude hiding (or, and, not)



------------------ to/from PInt -- alias
class SafeTo PInt a => OpSafeToPInt a where
    unsafe_toPInt :: a -> PInt
    safe_toPInt :: a -> Maybe PInt
class To PInt a => OpToPInt a where
    toPInt :: a -> PInt
instance SafeTo PInt a => OpSafeToPInt a where
    unsafe_toPInt = unsafe_from
    safe_toPInt = safe_from
instance To PInt a => OpToPInt a where
    toPInt = from

class SafeFrom PInt a => OpSafeFromPInt a where
    unsafe_fromPInt :: PInt -> a
    safe_fromPInt :: PInt -> Maybe a
class From PInt a => OpFromPInt a where
    fromPInt :: PInt -> a
instance SafeFrom PInt a => OpSafeFromPInt a where
    unsafe_fromPInt = unsafe_from
    safe_fromPInt = safe_from
instance From PInt a => OpFromPInt a where
    fromPInt = from





