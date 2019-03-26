{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- diff
--      from ArrCpsDetectT__private: to remove Boxed instance
--      from ArrCPSx: add IArrowCatch
module Arrows.ArrCpsDetectT
    ( ArrCpsDetectT(), mkArrCpsDetectT, unArrCpsDetectT
    , change_resultCpsDetectT, relabelR_CpsDetectT
    , ByWrapped_ArrCpsDetectT
    )
where
import qualified Arrows.Arrows__private.ArrCpsDetectT__private as A
import Arrows.Arrows__private.ArrCpsDetectT__private (ByWrapped_ArrCpsDetectT)
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

--import Seed.Op_catchCPS(catchCPS, EE)
import Seed.ArrowOps (constA, mk_app, unJustA, maybe2eitherA)
import ADT.IFunctorA (fmapA)
{-
import Seed.ArrowOps
    (Arrow2Functor(..), Arrow2Applicative(..), Arrow2Monad(..))
--import Seed.Boxed
-}
--import ADT.IArrowExit
import ADT.IArrowCatch
import ADT.IArrowSuccessBy
import Data.Semigroup
import Seed.ProxyOps (Proxy(..), withBy)


type AA e = A.ArrCpsDetectT (Maybe e)
newtype ArrCpsDetectT e r arr i o =
    ArrCpsDetectT { runArrCpsDetectT :: AA e r arr i o }
    deriving
        ( Category, Arrow, ArrowChoice
        -- , ArrowApply
        , ArrowZero, ArrowPlus, Semigroup, Monoid
        , Functor, Applicative, Monad
        -- , IArrowExit, IArrowCC
        -- , IArrowThrowBy OnExit_Arrow, IArrowCC
        -- , IArrowResetBy (ByWrapped_ArrCpsDetectT by)
        -- , IArrowThrowBy (ByWrapped_ArrCpsDetectT by)
        -- , IArrowThrowBaseBy (ByWrapped_ArrCpsDetectT by)
        {-
        , IArrowThrowBy by
        , IArrowCatchBy by
        , IArrowThrowBy OnException_Arrow
        , IArrowCatchBy OnException_Arrow
        -- , IArrowSuccess
        , IArrowResetBy OnException_Arrow
        , OpDoOrNopABy OnException_Arrow
        , OpDetectSuccessABy OnException_Arrow
        , OpLookAheadABy OnException_Arrow
        -}
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



-- Exit
instance Arrow arr => IArrowThrowBaseBy OnExit_Arrow (ArrCpsDetectT e r arr) where
    type ArrowExceptionTypeBy OnExit_Arrow (ArrCpsDetectT e r arr) = r
instance Arrow arr => IArrowThrowBy OnExit_Arrow (ArrCpsDetectT e r arr) where
    throwABy_ = box throwABy_
    -- no catch!
instance Arrow arr => IArrowCC (ArrCpsDetectT e r arr) where


-- Exception
instance (Arrow arr, ArrowChoice arr)
    => IArrowThrowBaseBy OnException_Arrow (ArrCpsDetectT e r arr) where
    type ArrowExceptionTypeBy OnException_Arrow (ArrCpsDetectT e r arr) = e
instance (Arrow arr, ArrowChoice arr)
    => IArrowThrowBy OnException_Arrow (ArrCpsDetectT e r arr) where
    throwABy_ = box $ Just ^>> throwABy_
instance (Arrow arr, ArrowChoice arr)
    => IArrowCatchBy OnException_Arrow (ArrCpsDetectT e r arr) where
    catchABy_ a = (box . catchABy_ $ unbox a) >>> left me2e where
        me2e = maybe2eitherA >>> (zeroArrow ||| id)


-- Zero
instance (Arrow arr, ArrowChoice arr)
    => IArrowThrowBaseBy OnZero_Arrow (ArrCpsDetectT e r arr) where
    type ArrowExceptionTypeBy OnZero_Arrow (ArrCpsDetectT e r arr) = ()
instance (Arrow arr, ArrowChoice arr)
    => IArrowThrowBy OnZero_Arrow (ArrCpsDetectT e r arr) where
    throwABy_ = zeroArrow
instance (Arrow arr, ArrowChoice arr)
    => IArrowCatchBy OnZero_Arrow (ArrCpsDetectT e r arr) where
    catchABy_ a = (box . catchA $ unbox a) >>> left me2_ where
        me2_ = maybe2eitherA >>> (id ||| throwABy p)
        -- p = Proxy :: Proxy OnZero_Arrow
        p = proxy_OnException_Arrow

-- Zero or Exception
instance (Arrow arr, ArrowChoice arr)
    => IArrowThrowBaseBy OnZeroOrException_Arrow (ArrCpsDetectT e r arr) where
    type ArrowExceptionTypeBy OnZeroOrException_Arrow (ArrCpsDetectT e r arr)
        = Maybe e
instance (Arrow arr, ArrowChoice arr)
    => IArrowThrowBy OnZeroOrException_Arrow (ArrCpsDetectT e r arr) where
    throwABy_ = box throwA
instance (Arrow arr, ArrowChoice arr)
    => IArrowCatchBy OnZeroOrException_Arrow (ArrCpsDetectT e r arr) where
    catchABy_ = box . catchA . unbox




{- IArrowResetBy -}
class (IArrowThrowBy by arr, IArrowResetBy by arr, OpLookAheadABy by arr
    , ArrowChoice arr)
    => IArrCpsDetectT_WrappedBase by e r arr
instance (IArrowThrowBy by arr, IArrowResetBy by arr, OpLookAheadABy by arr
    , ArrowChoice arr)
    => IArrCpsDetectT_WrappedBase by e r arr
instance (IArrCpsDetectT_WrappedBase by e r arr)
    => IArrowThrowBaseBy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr) where
    type ArrowExceptionTypeBy
            (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr)
            = ArrowExceptionTypeBy by arr
instance (IArrCpsDetectT_WrappedBase by e r arr)
    => IArrowThrowBy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr) where
    throwABy_ = box throwABy_
    -- no catch! just to support IArrowResetBy
instance (IArrCpsDetectT_WrappedBase by e r arr)
    => IArrowResetBy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr) where
instance (IArrCpsDetectT_WrappedBase by e r arr)
    => OpDetectSuccessABy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr) where
instance (IArrCpsDetectT_WrappedBase by e r arr)
    => OpDoOrNopABy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr) where
instance (IArrCpsDetectT_WrappedBase by e r arr)
    => OpLookAheadABy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr) where
    look_aheadABy_ = box . look_aheadABy_ . unbox
    look_aheadABy = withBy look_aheadABy_
    -- without catch zero/exception


--}
--}
--}
