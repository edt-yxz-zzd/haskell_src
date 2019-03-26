{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DefaultSignatures #-}


module ADT.OpDefaultValue
where

import ADT.IMonoidBy
import Seed.ProxyOps
import Seed.MaybeOps (isNothing)



class OpDefaultValueBy by a where
    default_valueBy :: proxy by -> a
    default_valueBy_ :: (?by :: proxy by) => a
    default_valueBy_ = withBy_ default_valueBy
    --default_valueBy = withBy default_valueBy_
    default default_valueBy :: IMonoidBy by a => proxy by -> a
    default_valueBy = memptyBy
    --{-# MINIMAL (default_valueBy | default_valueBy_) #-}
    {-# MINIMAL (default_valueBy) #-}
class OpDefaultValueBy by a => OpIsDefaultValueBy by a where
    is_default_valueBy :: proxy by -> a -> Bool
    is_default_valueBy_ :: (?by :: proxy by) => a -> Bool
    {-# MINIMAL (is_default_valueBy | is_default_valueBy_) #-}
    is_default_valueBy_ = withBy_ is_default_valueBy
    is_default_valueBy = withBy is_default_valueBy_



------------- class --  default without by
class OpDefaultValueBy (DefaultValue2By a) a
    => OpDefaultValueDefaultBy a where
    type DefaultValue2By a
    type DefaultValue2By a = ()
class OpDefaultValueDefaultBy a => OpDefaultValue a where
    default_value :: a
class (OpDefaultValue a, OpIsDefaultValueBy (DefaultValue2By a) a)
    => OpIsDefaultValue a where
    is_default_value :: a -> Bool


------------- instance --  default without by
instance (OpDefaultValueDefaultBy a, by ~ DefaultValue2By a)
    => OpDefaultValue a where
    default_value = default_valueBy (Proxy :: Proxy by)
instance (OpDefaultValue a, OpIsDefaultValueBy by a, by ~ (DefaultValue2By a))
    => OpIsDefaultValue a where
    is_default_value = is_default_valueBy (Proxy :: Proxy by)


--------------- ()
instance OpDefaultValueBy () () where
    default_valueBy _ = ()
instance OpDefaultValueDefaultBy () where
instance OpIsDefaultValueBy () () where
    is_default_valueBy_ = const True


--------------- Maybe
instance OpDefaultValueBy () (Maybe a) where
    default_valueBy _ = Nothing
instance OpDefaultValueDefaultBy (Maybe a) where
instance OpIsDefaultValueBy () (Maybe a) where
    is_default_valueBy_ = isNothing

--------------- []
instance OpDefaultValueBy () ([] a) where
    default_valueBy _ = []
instance OpDefaultValueDefaultBy ([] a) where
instance OpIsDefaultValueBy () ([] a) where
    is_default_valueBy_ = null

-------------- Num a ==>> Sum -> 0; Product -> 1


maybe2either_ex :: OpDefaultValue d => Maybe a -> Either d a
maybe2either_ex = maybe (Left default_value) Right
maybe2default :: OpDefaultValue d => Maybe d -> d
maybe2default = maybe default_value id



