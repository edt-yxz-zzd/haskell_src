
module Container.OpUniversal
where

class OpUniversal s where -- v.s. OpEmpty
    universal :: s
class OpIsUniversal s where -- v.s. OpNull/OpIsEmpty
    is_universal :: s -> Bool


