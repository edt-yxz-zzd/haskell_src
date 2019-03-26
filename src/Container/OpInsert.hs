{-# LANGUAGE TypeFamilies #-}
module Container.OpInsert
where
import Container.IContainer
import Container.OpIter

class DynMappingConcept a => OpInsert a where -- v.s. OpPut
    --insert :: KeyType a -> ValueType a -> a -> a
    insert :: ItemRangeType a -> a -> a
    inserts :: (OpIter b, ItemRangeType a ~ Element b) => b -> a -> a
    inserts b a = foldr insert a $ iter b


