


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arrows.ArrCpsT
    ( ArrCpsT(), mkArrCpsT, unArrCpsT, change_resultCpsT, relabelR_CpsT
    -- , EE, CPS, catchCPS
    )
where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

import Seed.EitherOps (a_E_bEc2aEb_E_c, aEb_E_c2a_E_bEc)
import Seed.ArrowOps (constA, withInput, catchCPS, mk_app, EE)
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
unArrCpsT a = (unArrCPS . unbox $ a >>^ Right) >>^ eEeEr2eEr where
    eEeEr2eEr (Left (Left _)) = error "logic error"
    eEeEr2eEr (Left (Right e)) = Left e
    eEeEr2eEr (Right r) = Right r
change_resultCpsT = undefined
relabelR_CpsT = undefined
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
instance (ArrowApply arr) => IArrowExit (ArrCpsT e r arr) where
instance (ArrowApply arr) => IArrowCC (ArrCpsT e r arr) where
    type ArrowCC_Result (ArrCpsT e r arr) = r
    exitCC = box $ Right ^>> exitCC

instance (ArrowApply arr, ArrowChoice arr)
    => IArrowCatch (ArrCpsT e r arr) where
    type ArrowExceptionType (ArrCpsT e r arr) = e
    throwA = box $ Left . Right ^>> exitCC

    catchA_ex i2o@(ArrCpsT (ArrCPS o2eEr_to_i2eEr)) = i2eEo where
        i2eEo = ArrCpsT (ArrCPS eEo2eEr_to_i2eEr)
        eEo2eEr_to_i2eEr = catchCPS catch catch throw o2eEr_to_i2eEr
        -- catch :: arr i (EE e r) -> arr i (EE e r)
        -- throw :: arr (EE e r) (EE e r)
        catch = id
        throw = id


--}
--}
--}
--}
--}
