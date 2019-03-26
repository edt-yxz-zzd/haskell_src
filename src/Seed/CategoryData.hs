{-# LANGUAGE GADTs #-}

module Seed.CategoryData
where
import Control.Arrow
import Seed.ArrowOps (forkA, constA, i2mo_to_i2iEo, eitherA, mk_app)
import Seed.MaybeOps (isJust)
import Seed.EitherOps (mayRight)
import Control.Category
import Prelude hiding ((.), id)


data CategoryPair arr i o where
    Chain2 :: arr i x -> arr x o -> CategoryPair arr i o
data CategoryTriple arr i o where
    Chain3 :: arr i x -> arr x y -> arr y o -> CategoryTriple arr i o
data CategoryData arr i o where
    -- ID :: Category arr => arr i o
    Arr :: arr i o -> CategoryData arr i o
    Chain
        :: ar ~ CategoryData arr
        => ar i x -> ar x o -> CategoryData arr i o

idCategoryPair :: Category arr => CategoryPair arr i i
idCategoryPair = Chain2 id id
mkCategoryPair :: Category arr => arr i o -> CategoryPair arr i o
mkCategoryPair i2o = Chain2 i2o id
unCategoryPair :: Category arr => CategoryPair arr i o -> arr i o
unCategoryPair (Chain2 i2x x2o) = x2o . i2x
instance Category arr => Category (CategoryPair arr) where
    id = Chain2 id id
    lhs . rhs = Chain2 (unCategoryPair rhs) (unCategoryPair lhs)
instance Arrow arr => Arrow (CategoryPair arr) where
    arr = mkCategoryPair . arr
    first (Chain2 a b) = Chain2 (first a) (first b)
    -- error: Chain2 a2x x2b *** Chain2 c2y y2d = Chain2 (a2x *** c2y) (x2b *** y2d)
instance ArrowChoice arr => ArrowChoice (CategoryPair arr) where
    --left (Chain2 a b) = Chain2 (left a) (left b)
    Chain2 a2x x2b +++ Chain2 c2y y2d = Chain2 (a2x +++ c2y) (x2b +++ y2d)
instance ArrowApply arr => ArrowApply (CategoryPair arr) where
    app = mk_app mkCategoryPair unCategoryPair
instance ArrowZero arr => ArrowZero (CategoryPair arr) where
    zeroArrow = Chain2 zeroArrow id
instance (ArrowPlus arr, ArrowChoice arr)
    => ArrowPlus (CategoryPair arr) where
    Chain2 i2x x2o <+> Chain2 i2y y2o =
        Chain2 (eitherA (+++) i2x i2y) (x2o ||| y2o)







--}
--}
--}
--}
--}

