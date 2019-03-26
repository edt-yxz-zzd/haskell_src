



module Container.OpEmpty
where

class OpEmpty s where
    empty :: s
class OpIsEmpty s where
    is_empty :: s -> Bool
class OpIsFull s where
    is_full :: s -> Bool

