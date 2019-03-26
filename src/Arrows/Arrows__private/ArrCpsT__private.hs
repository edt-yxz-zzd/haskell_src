


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arrows.Arrows__private.ArrCpsT__private
    ( ArrCpsT(), mkArrCpsT, unArrCpsT, change_resultCpsT, relabelR_CpsT
    )
where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

import Seed.Op_catchCPS(catchCPS, EE)
import Seed.ArrowOps (constA, mk_app)
import Seed.ArrowOps
    (Arrow2Functor(..), Arrow2Applicative(..), Arrow2Monad(..))
import Seed.Boxed
import ADT.IArrowCC
import ADT.IArrowCatch
import ADT.IArrowSuccess
import Data.Semigroup
import Arrows.ArrCPS


newtype ArrCpsT e r arr i o
    = ArrCpsT {runArrCpsT :: ArrCPS arr (EE e r) i o}

mkArrCpsT :: Category arr => arr i o -> ArrCpsT e r arr i o
mkArrCpsT = box . mkArrCPS
unArrCpsT :: ArrowApply arr => ArrCpsT e r arr i r -> arr i (Either e r)
unArrCpsT a = (unArrCPS . unbox $ a >>^ Right) >>^ neEr2eEr where
    neEr2eEr (Left (0, e)) = Left e
    neEr2eEr (Left _) = error "logic error"
    neEr2eEr (Right r) = Right r
change_resultCpsT, relabelR_CpsT
    :: (ArrowChoice arr)
    => arr e' e -> arr e e' -> arr r' r -> arr r r'
    -> ArrCpsT e r arr i o -> ArrCpsT e' r' arr i o
change_resultCpsT a2e e2a s2r r2s = mapGBox $ change_resultCPS naEs2neEr neEr2naEs where
    naEs2neEr = mk a2e s2r
    neEr2naEs = mk e2a r2s
    mk = mk_neEr2naEs
    mk_neEr2naEs e2a r2s = second e2a +++ r2s
relabelR_CpsT = change_resultCpsT
instance Boxed (ArrCpsT e r arr i o) where
    type BoxedFrom (ArrCpsT e r arr i o) = ArrCPS arr (EE e r) i o
    box = ArrCpsT
    unbox = runArrCpsT


{-
-- not exported
box = ArrCpsT
unbox = runArrCpsT
-}

----------------------------
instance Category arr => Category (ArrCpsT e r arr) where
    id = box id
    (.) = opmapbGBox (.)
instance ArrowApply arr => Arrow (ArrCpsT e r arr) where
    arr = box . arr
    first = mapGBox first
instance (ArrowApply arr, ArrowChoice arr)
    => ArrowChoice (ArrCpsT e r arr) where
    left = mapGBox left
instance (ArrowApply arr) => ArrowApply (ArrCpsT e r arr) where
    app = mk_app box unbox

instance (ArrowApply arr, ArrowZero arr) => ArrowZero (ArrCpsT e r arr) where
    zeroArrow = box zeroArrow
instance (ArrowApply arr, ArrowPlus arr) => ArrowPlus (ArrCpsT e r arr) where
    (<+>) = opmapbGBox (<+>)


-------------------------------
instance ArrowPlus arr => Semigroup (ArrCpsT e r arr i o) where
    (<>) = opmapbGBox (<>)
instance (ArrowApply arr, ArrowPlus arr)
    => Monoid (ArrCpsT e r arr i o) where
    mappend = opmapbGBox mappend
    mempty = zeroArrow
instance ArrowApply arr => Functor (ArrCpsT e r arr i) where
    fmap = fmapA2M
instance ArrowApply arr => Applicative (ArrCpsT e r arr i) where
    pure = pureA2M
    (<*>) = (<**>)
instance (ArrowApply arr) => Monad (ArrCpsT e r arr i) where
    (>>=) = (>>==)



---------------------------------------
instance (ArrowApply arr) => IArrowExitBase (ArrCpsT e r arr) where
    type ArrowExit_Result (ArrCpsT e r arr) = r
instance (ArrowApply arr) => IArrowExit (ArrCpsT e r arr) where
instance (ArrowApply arr) => IArrowCC (ArrCpsT e r arr) where
    exitCC = box $ Right ^>> exitCC

instance (ArrowApply arr, ArrowChoice arr)
    => IArrowCatchBase (ArrCpsT e r arr) where
    type ArrowExceptionType (ArrCpsT e r arr) = e
instance (ArrowApply arr, ArrowChoice arr)
    => IArrowCatch (ArrCpsT e r arr) where
    throwA = box $ Left . (,) 0 ^>> exitCC

    catchA_ex i2o@(ArrCpsT (ArrCPS o2neEr_to_i2neEr)) = i2eEo where
        i2eEo = ArrCpsT $ ArrCPS eEo2neEr_to_i2neEr
        eEo2neEr_to_i2neEr = catchCPS catch catch throw o2neEr_to_i2neEr

        -- catch :: arr i (Either ne r) -> arr i (EE e r)
        -- throw :: arr (EE e r) (Either ne r)
        catch = id
        throw = id



--}
--}
--}
--}
--}
