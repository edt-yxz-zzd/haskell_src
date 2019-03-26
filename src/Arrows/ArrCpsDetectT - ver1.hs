{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- diff
--      from ArrCpsDetectT__private: to remove Boxed instance
--      from ArrCPSx: add IArrowCatch
module Arrows.ArrCpsDetectT
    ( ArrCpsDetectT(), mkArrCpsDetectT, unArrCpsDetectT
    , change_resultCpsDetectT, relabelR_CpsDetectT
    )
where
import qualified Arrows.Arrows__private.ArrCpsDetectT__private as A
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

--import Seed.Op_catchCPS(catchCPS, EE)
import Seed.ArrowOps (constA, mk_app, unJustA)
import ADT.IFunctorA (fmapA)
{-
import Seed.ArrowOps
    (Arrow2Functor(..), Arrow2Applicative(..), Arrow2Monad(..))
--import Seed.Boxed
-}
import ADT.IArrowCC
import ADT.IArrowCatch
import ADT.IArrowSuccess
import Data.Semigroup


newtype ArrCpsDetectT e r arr i o =
    ArrCpsDetectT { runArrCpsDetectT :: A.ArrCpsDetectT (Maybe e) r arr i o }
    deriving
        ( Category, Arrow, ArrowChoice
        -- , ArrowApply
        , ArrowZero, ArrowPlus, Semigroup, Monoid
        , Functor, Applicative, Monad
        , IArrowExit, IArrowCC
        , IArrowCatch
        -- , IArrowSuccess
        , IArrowReset, OpDoOrNopA, OpDetectSuccessA, OpLookAheadA
        )
box = ArrCpsDetectT
unbox = runArrCpsDetectT

mkArrCpsDetectT :: Arrow arr => arr i o -> ArrCpsDetectT e r arr i o
unArrCpsDetectT
    :: Arrow arr => ArrCpsDetectT e r arr i r -> arr i (Either (Maybe e) r)
change_resultCpsDetectT, relabelR_CpsDetectT
    :: (ArrowChoice arr)
    => arr e e' -> arr r r'
    -> ArrCpsDetectT e r arr i o -> ArrCpsDetectT e' r' arr i o

mkArrCpsDetectT = box . A.mkArrCpsDetectT
    -- the only constructor
    -- to ensure the o2r is the tail part of i2r
unArrCpsDetectT = A.unArrCpsDetectT . unbox
change_resultCpsDetectT e2a r2s =
    box . A.change_resultCpsDetectT (fmapA e2a) r2s . unbox
relabelR_CpsDetectT = change_resultCpsDetectT
instance ArrowApply arr => ArrowApply (ArrCpsDetectT e r arr) where
    app = mk_app box unbox

instance (Arrow arr) => IArrowExitBase (ArrCpsDetectT e r arr) where
    type ArrowExit_Result (ArrCpsDetectT e r arr) = r


instance (Arrow arr, ArrowChoice arr)
    => IArrowCatchBase (ArrCpsDetectT e r arr) where
    type ArrowExceptionType (ArrCpsDetectT e r arr) = Maybe e
{-
instance (Arrow arr, ArrowChoice arr)
    => IArrowCatchBase (ArrCpsDetectT e r arr) where
    type ArrowExceptionType (ArrCpsDetectT e r arr) = e
instance (Arrow arr, ArrowChoice arr, ArrowZero arr)
    => IArrowCatch (ArrCpsDetectT e r arr) where
    throwA = box $ Just ^>> throwA
    catchA_ex a = box $ catchA_ex (unbox a) >>> left unJustA
-}


--}
--}
--}
