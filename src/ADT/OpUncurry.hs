{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module ADT.OpUncurry
where

--import Seed.ArrowOps (withInput, ab_c2ac_bA)
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id, uncurry, curry)
--import Seed.Boxed



curry :: Arrow arr => arr (a, b) c -> arr a (arr b c)
curry ab2c = a2b2c where
    a2b2c = arr (\a -> (,) a ^>> ab2c)
class ArrowApply arr => OpUncurry arr where
    uncurry :: arr a (arr b c) -> arr (a, b) c
    uncurry = app2uncurry app
instance ArrowApply arr => OpUncurry arr where
    uncurry a2b2c = ab2c where
        ab2c = ab_to_b2c_b >>> app
        ab_to_b2c_b = first a2b2c

app2uncurry
    :: (Arrow arr, i ~ b, o ~ c)
    => arr (arr i o, i) o
    -> (arr a (arr b c) -> arr (a, b) c)
app2uncurry app a2b2c = ab2c where
    ab2c = ab_to_b2c_b >>> app
    ab_to_b2c_b = first a2b2c
uncurry2app
    :: (Category arr, a ~ arr i o, b ~ i, c ~ o)
    => (arr a (arr b c) -> arr (a, b) c)
    -> arr (arr i o, i) o
uncurry2app uncurry = app where
    app = i2o_i_to_o
    i2o_i_to_o = uncurry i2o_to_i2o
    i2o_to_i2o = id

{-
instance (Arrow arr, OpUncurry arr) => ArrowApply arr where
    app = uncurry2app uncurry
instance (Arrow arr, OpUncurry arr) => ArrowApply arr where
    app = i2o_i_to_o where
        i2o_i_to_o = uncurry i2o_to_i2o
        i2o_to_i2o = id
-}

