{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- to remove Boxed instance
module Arrows.ArrCpsDetectT
where
import Arrows.ArrCpsT
    (ArrCpsT(), mkArrCpsT, unArrCpsT, change_resultCpsT, relabelR_CpsT)
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

import Seed.Op_catchCPS(catchCPS, EE)
import Seed.ArrowOps (constA, mk_app)
import Seed.ArrowOps
    (Arrow2Functor(..), Arrow2Applicative(..), Arrow2Monad(..))
--import Seed.Boxed
import ADT.IArrowCC
import ADT.IArrowCatch
import ADT.IArrowSuccess
import Data.Semigroup
import ADT.IFunctorA (fmapA)
import Seed.MaybeOps (unJust)


newtype ArrCpsDetectT e r arr i o
    = ArrCpsDetectT { runArrCpsDetectT :: ArrCpsT e (Maybe r) arr i o }
    deriving
        ( Category, Arrow, ArrowChoice
        -- , ArrowApply
        , ArrowZero, ArrowPlus, Semigroup, Monoid
        , Functor, Applicative, Monad
        -- , IArrowExit, IArrowCC
        , IArrowCatch
        -- , IArrowSuccess
        )
box = ArrCpsDetectT
unbox = runArrCpsDetectT

mkArrCpsDetectT :: Category arr => arr i o -> ArrCpsDetectT e r arr i o
unArrCpsDetectT
    :: ArrowApply arr => ArrCpsDetectT e r arr i r -> arr i (Either e r)
_unArrCpsDetectT
    :: ArrowApply arr
    => ArrCpsDetectT e r arr i (Maybe r) -> arr i (Either e (Maybe r))
change_resultCpsDetectT, relabelR_CpsDetectT
    :: (ArrowChoice arr)
    => arr e' e -> arr e e' -> arr r' r -> arr r r'
    -> ArrCpsDetectT e r arr i o -> ArrCpsDetectT e' r' arr i o
_change_resultCpsDetectT
    :: (ArrowChoice arr)
    => ArrCpsDetectT e r arr i o -> ArrCpsDetectT e r' arr i o
mkArrCpsDetectT = box . mkArrCpsT
    -- the only constructor
    -- to ensure the o2r is the tail part of i2r
unArrCpsDetectT a = (unArrCpsT . unbox $ a >>^ Just) >>^ fmap unJust
_unArrCpsDetectT = unArrCpsT . unbox

change_resultCpsDetectT a2e e2a s2r r2s =
    box . change_resultCpsT a2e e2a (fmapA s2r) (fmapA r2s) . unbox
relabelR_CpsDetectT = change_resultCpsDetectT
_change_resultCpsDetectT =
    box . change_resultCpsT id id (constA Nothing) (constA Nothing) . unbox



instance ArrowApply arr => ArrowApply (ArrCpsDetectT e r arr) where
    app = mk_app box unbox

instance (ArrowApply arr) => IArrowExitBase (ArrCpsDetectT e r arr) where
    type ArrowExit_Result (ArrCpsDetectT e r arr) = r
instance (ArrowApply arr) => IArrowExit (ArrCpsDetectT e r arr) where
instance (ArrowApply arr) => IArrowCC (ArrCpsDetectT e r arr) where
    exitCC = box $ Just ^>> exitCC

instance (ArrowApply arr, ArrowChoice arr)
    => IArrowCatchBase (ArrCpsDetectT e r arr) where
    type ArrowExceptionType (ArrCpsDetectT e r arr) = e
instance (ArrowApply arr, ArrowChoice arr, IArrowReset arr)
    => IArrowReset (ArrCpsDetectT e r arr) where
instance (ArrowApply arr, ArrowChoice arr, OpDetectSuccessA arr)
    => OpDetectSuccessA (ArrCpsDetectT e r arr) where
    detect_successA i2o = mkArrCpsDetectT i2b where
        i2r = unArrCpsT $ unbox i2o >>> constA Nothing
        i2b = detect_successA i2r
instance (ArrowApply arr, ArrowChoice arr, OpLookAheadA arr)
    => OpLookAheadA (ArrCpsDetectT e r arr) where
    look_aheadA i2o = mkArrCpsDetectT i2b where
        i2r = unArrCpsT $ unbox i2o >>> constA Nothing
        i2b = detect_successA i2r





