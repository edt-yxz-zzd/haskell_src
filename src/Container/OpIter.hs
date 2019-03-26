{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Container.OpIter
where
import Container.IContainer
import Container.OpLen
import Data.List
import Data.Maybe
import qualified Data.Foldable as F
import Container.OpEmpty
--import Container.OpAnyElem

class (OpIsEmpty a, IContainer a) => OpAnyElem a where
    any_elem :: a -> Maybe (Element a)
    default any_elem :: OpIterLe a => a -> Maybe (Element a)
    any_elem = listToMaybe . iter_le 1

class (OpAnyElem a, OpLenIfLe a) => OpIterLe a where
    -- allow infinite not only countable infinite
    iter_le :: Integer -> a -> [Element a]
        -- iter_le n a = ls ==>> assert len ls <= n && |a| >= len ls
        -- if len ls < n ==>> |a| == len ls
    default iter_le :: OpIter a => Integer -> a -> [Element a]
    iter_le n = genericTake n . iter
class (CountableContainer a, OpIterLe a) => OpIter a where
    -- OpFrom a [Element a]??
    iter :: a -> [Element a]
    default iter :: (F.Foldable f, f (Element a) ~ a) => a -> [Element a]
    iter = F.toList



