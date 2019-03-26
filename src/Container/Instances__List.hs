{-# LANGUAGE TypeFamilies #-}

module Container.Instances__List
where

import Container.IContainer
import Container.IStream
import Container.OpIter
--import Container.OpAnyElem
import Container.OpEmpty
import Container.OpLen
import Container.OpPop
import Data.List

instance IContainer [a] where
    type Element [a] = a
instance OpIsEmpty [a] where
    is_empty = null
instance OpAnyElem [a] where
instance OpUnsafeLen [a] where
    unsafe_len = genericLength
instance OpLenIfLe [a] where
    len_if_le n ls = if toInteger n' <= n then Just n' else Nothing where
        n' = genericLength $ genericTake (n+1) ls
instance OpIterLe [a] where
instance OpIter [a] where
    iter = id
instance CountableContainer [a] where
instance SequenceConcept [a] where

instance OpPop [a] where
    pop (h:ts) = Just (h, ts)
    pop _ = Nothing



