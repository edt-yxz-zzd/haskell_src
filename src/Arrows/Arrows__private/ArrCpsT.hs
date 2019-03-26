{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- diff
--      from ArrCpsT__private: to remove Boxed instance
--      from ArrCPS: add IArrowCatch
--      see: ArrCPSx, ArrCpsDetectT which add OpDetectSuccess
module Arrows.ArrCpsT
    ( ArrCpsT(), mkArrCpsT, unArrCpsT, change_resultCpsT, relabelR_CpsT
    )
where
import qualified Arrows.Arrows__private.ArrCpsT__private as A
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


newtype ArrCpsT e r arr i o = ArrCpsT { runArrCpsT :: A.ArrCpsT e r arr i o }
    deriving
        ( Category, Arrow, ArrowChoice
        -- , ArrowApply
        , ArrowZero, ArrowPlus, Semigroup, Monoid
        , Functor, Applicative, Monad
        , IArrowExit, IArrowCC, IArrowCatch
        -- , IArrowSuccess
        )
box = ArrCpsT
unbox = runArrCpsT

mkArrCpsT :: Category arr => arr i o -> ArrCpsT e r arr i o
unArrCpsT :: ArrowApply arr => ArrCpsT e r arr i r -> arr i (Either e r)
change_resultCpsT, relabelR_CpsT
    :: (ArrowChoice arr)
    => arr e' e -> arr e e' -> arr r' r -> arr r r'
    -> ArrCpsT e r arr i o -> ArrCpsT e' r' arr i o

mkArrCpsT = box . A.mkArrCpsT
    -- the only constructor
    -- to ensure the o2r is the tail part of i2r
unArrCpsT = A.unArrCpsT . unbox
change_resultCpsT a2e e2a s2r r2s =
    box . A.change_resultCpsT a2e e2a s2r r2s . unbox
relabelR_CpsT = change_resultCpsT
instance ArrowApply arr => ArrowApply (ArrCpsT e r arr) where
    app = mk_app box unbox

instance (ArrowApply arr) => IArrowExitBase (ArrCpsT e r arr) where
    type ArrowExit_Result (ArrCpsT e r arr) = r

instance (ArrowApply arr, ArrowChoice arr)
    => IArrowCatchBase (ArrCpsT e r arr) where
    type ArrowExceptionType (ArrCpsT e r arr) = e
{-
instance IArrowExitBase (ArrCpsT e r arr) where
    type ArrowExit_Result (ArrCpsT e r arr)
        = ArrowExit_Result (A.ArrCpsT e r arr)
instance IArrowCatchBase (ArrCpsT e r arr) where
    type ArrowExceptionType (ArrCpsT e r arr)
        = ArrowExceptionType (A.ArrCpsT e r arr)
-}


