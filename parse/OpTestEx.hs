{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module OpTestEx
    ( module OpTestEx
    , module OpTest
    )
where

import Explain_abc_Boolean
import Explain__Bool
import Explain
import OpDynTheOnlyValue
import OpTest
import Data.Set as S
import Data.Map as M
import Prelude as P
import UInt
import PInt
import DInt
import Data.Ratio
import Data.Sequence as Seq

-- Double? Complex?
-- Word? Char?
-- Ratio
instance Integral a => OpTest (Ratio a) where
    type TestExResult (Ratio a) = Bool
instance Integral a => OpSafeFrom (Ratio a) Bool where
instance Integral a => OpFrom (Ratio a) Bool where
    from = (0 /=)






-- UInt/...
--------------- UInt
instance OpTest UInt where
    type TestExResult UInt = Bool
instance OpSafeFrom UInt Bool where
instance OpFrom UInt Bool where
    from = (0 /=)
--------------- PInt
instance OpTest PInt where
    type TestExResult PInt = Value_True
instance OpSafeFrom PInt Bool where
instance OpFrom PInt Bool where
    from _ = True
instance OpSafeFrom PInt Value_True where
instance OpFrom PInt Value_True where
    from _ = the_only_value
--------------- DInt
instance OpTest DInt where
    type TestExResult DInt = Value_True
instance OpSafeFrom DInt Bool where
instance OpFrom DInt Bool where
    from _ = True
instance OpSafeFrom DInt Value_True where
instance OpFrom DInt Value_True where
    from _ = the_only_value





-- Set Map Seq
--------------- Set
instance OpTest (Set a) where
    type TestExResult (Set a) = Bool
instance OpSafeFrom (Set a) Bool where
instance OpFrom (Set a) Bool where
    from = P.not . S.null
--------------- Map
instance OpTest (Map a b) where
    type TestExResult (Map a b) = Bool
instance OpSafeFrom (Map a b) Bool where
instance OpFrom (Map a b) Bool where
    from = P.not . M.null

-- Seq
instance OpTest (Seq a) where
    type TestExResult (Seq a) = Bool
instance OpSafeFrom (Seq a) Bool where
instance OpFrom (Seq a) Bool where
    from = P.not . Seq.null



