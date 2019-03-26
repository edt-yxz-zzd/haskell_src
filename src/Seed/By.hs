
module Seed.By
where

data ByLen
data BySum
data ByProduct
type ByLast = ByDual ByFirst
data ByFirst
data ByEndo
data ByMin
type ByMax = ByDual ByMin

data ByDual a -- swap lhs rhs
data ByNot a
data ByAnd a b
type ByOr a b = ByNot (ByAnd (ByNot a) (ByNot b))
type ByXor a b = ByOr (ByAnd a (ByNot b)) (ByAnd (ByNot a) b)
data ByNXor a b = ByNot (ByXor a b)
data ByAny a
data ByAll a




