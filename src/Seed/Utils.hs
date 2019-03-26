

module Seed.Utils
where

caseBool :: (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
caseBool pred onTrue onFalse a = if pred a then onTrue a else onFalse a


-- (.)
(...) :: (x->o) -> (a->b->x) -> (a->b->o)
(x2o ... op) a b = x2o (op a b)
infixr 1 ...


