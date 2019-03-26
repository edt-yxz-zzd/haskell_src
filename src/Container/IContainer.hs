{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Container.IContainer
where
--import Numeric.Natural


class IContainer a where
    type Element a :: *

class (e~Element a, IContainer a) => ContainerEx e a | a -> e
instance (e~Element a, IContainer a) => ContainerEx e a

class IContainer a => CountableContainer a
class CountableContainer a => FiniteContainer a
class IContainer a => SetConcept a
    -- no OpMember a
    -- no Eq (Element a)
class CountableContainer a => SequenceConcept a

class WithKeyType a where
    type KeyType a :: *
class WithKeyRangeType a where
    type KeyRangeType a :: *
    -- for update mapping
class WithValueType a where
    type ValueType a :: *
class WithMemberType a where
    type MemberType a :: *
class   ( MappingConcept a
        , WithKeyRangeType a
        , IMappingItem (ItemRangeType a)
        , KeyType (ItemRangeType a) ~ KeyRangeType a
        , ValueType (ItemRangeType a) ~ ValueType a
        )
    => DynMappingConcept a where
    type ItemRangeType a :: *
class   ( WithKeyType a, WithValueType a
        , IMappingItem (ItemType a)
        , KeyType (ItemType a) ~ KeyType a
        , ValueType (ItemType a) ~ ValueType a
        )
    => MappingConcept a where
    -- not a IContainer
    -- Seq v -> Map Int v -> IContainer v ??
    -- Set k -> Map k () -> IContainer k ??
    --
    -- is_element :: Element a -> a -> Bool
    -- is_key :: KeyType a -> a -> Bool
    -- is_value :: ValueType a -> a -> Bool
    -- is_member :: MemberType a -> a -> Bool
    -- Map k v => IContainer, MappingConcept
    --      Element = ItemType = (k, v)
    --      is_member = is_key
    -- Seq v => IContainer, MappingConcept
    --      Element = v -- Element != ItemType !!!!!!!!!!
    --      KeyType = UInt
    --      ValueType = v
    --      ItemType = (UInt, v)
    --      is_member = is_value
    -- Set k => IContainer, MappingConcept
    --      Element = k
    --      KeyType = k
    --      ValueType = k
    --      ItemType = k
    --      is_member = is_value = is_key
    type ItemType a :: *
    type ItemType a = (KeyType a, ValueType a)

class (WithKeyType a, WithValueType a) => IMappingItem a where
    item2key :: a -> KeyType a
    item2value :: a -> ValueType a

instance WithKeyType (k, v) where
    type KeyType (k, v) = k
instance WithValueType (k, v) where
    type ValueType (k, v) = v
instance IMappingItem (k, v) where
    item2key = fst
    item2value = snd



