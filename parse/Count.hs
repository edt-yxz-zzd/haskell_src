{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Count where
import Container_base_static
import ExplainEx
import Prelude hiding ((+))

data Value_TheAllInstanceSet
newtype Dict2Count m = Dict2Count m


class   ( Natural (ValueType c), Mapping c
        , Explain (Value_TheAllInstanceSet) (KeySetType c)
        , OptionalMapView (PresentMapView c)
        , KeyType c ~ KeyType (PresentMapView c)
        , PositiveInt (ValueType (PresentMapView c))
        , Explain (PresentMapView c) c
        )
    => Count c where
    -- default 0 if absent
    -- delete if 0

    type PresentMapView c :: *
    toPresentMapView :: c -> PresentMapView c
    toPresentMapView = explain


class OpFromList e a | a->e where
    -- not Container
    fromList :: [e] -> a
class OpToList e a | a->e where
    -- not Container
    toList :: a -> [e]
class OpAdd a where
    (+) :: a -> a -> a
infixl 6 +



class (Container a c, Ord a) => OpMinView a c where
    minView :: c -> Maybe (a, c)
class (Container a c, Ord a) => OpMaxView a c where
    maxView :: c -> Maybe (a, c)
class Container a c => OpFoldr a c where
    foldr :: (a->b->b) -> b -> c -> b
class Count c => OpFromPresentMapView c where
    fromPresentMapView :: PresentMapView c -> c
class   ( Count c
        , OpFromList (KeyType c, UInt) c
        , OpMapKey c
        , OpFromPresentMapView c
        , OpDiscardNKey c
        , OpFilterKey c
        , OpAdd (ValueType c)
        )
    => DynCount c where
class MapView c => OpFilterKey c where
    filter_key :: (KeyType c -> Bool) -> c -> c
class MapView c => OpDiscardNKey c where
    discard_n_key :: Integer -> KeyType c -> c -> c
class (MapView c, MapView c') => OpMapKeyEx c c' where
    map_key_ex :: a~ValueType c => (a->a->a) -> (KeyType c -> KeyType c') -> c -> c'
class (OpMapKeyEx c c) => OpMapKey c where
    map_key :: a~ValueType c => (a->a->a) -> (KeyType c -> KeyType c) -> c -> c
    map_key = map_key_ex



{-
instance (Integral (ValueType m), OptionalMapView m)
    => MapView (Dict2Count m) where
instance (Integral (ValueType m), OptionalMapView m)
    => OptionalMapView (Dict2Count m) where
instance (Integral (ValueType m), OptionalMapView m)
    => Mapping (Dict2Count m) where


--}
--}
--}
--}


