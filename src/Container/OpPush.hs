{-# LANGUAGE TypeFamilies #-}
module Container.OpPush
where
import Container.IContainer
import Container.OpIter
import Container.OpPop

class IContainer a => OpPut a where -- v.s. OpInsert
    put :: Element a -> a -> a
    puts :: (OpIter b, Element a ~ Element b) => b -> a -> a
    puts b a = foldr put a $ iter b
class (OpPut a, OpPop a) => OpPush a where
    -- pop (push v a) == Just (v, a)
    -- should not be Mapping!!!
    push :: Element a -> a -> a
    pushs :: (OpIter b, Element a ~ Element b) => b -> a -> a
    push = put
    pushs = puts


