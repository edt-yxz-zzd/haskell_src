


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Arrows.Arrows__private.ArrCpsDetectT__private
    ( ArrCpsDetectT()
    , mkArrCpsDetectT, unArrCpsDetectT
    , change_resultCpsDetectT, relabelR_CpsDetectT
    , ByWrapped_ArrCpsDetectT(..)
    )
where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

import Seed.Op_catchCPSx(catchCPS, EE, CPS)
import Seed.ArrowOps (constA, mk_app, mk_arr_plus)
import Seed.ArrowOps
    (Arrow2Functor(..), Arrow2Applicative(..), Arrow2Monad(..))
import Seed.Boxed
--import ADT.IArrowExit
import ADT.IArrowCatch
import ADT.IArrowSuccessBy
import Data.Semigroup
import Arrows.ArrCPSx
import Numeric.Natural
import ADT.OpDefaultValue
import Seed.ProxyOps (withBy, Proxy(..))


newtype ArrCpsDetectT e r arr i o
    = ArrCpsDetectT {runArrCpsDetectT :: ArrCPSx arr (EE e r) i o}
    deriving
        ( Category, Arrow, ArrowChoice
        -- , ArrowApply
        -- , ArrowZero, ArrowPlus, Semigroup, Monoid
        , Functor, Applicative, Monad
        -- , IArrowExit, IArrowCC, IArrowCatch
        -- , IArrowSuccess
        )
mkArrCpsDetectT :: Arrow arr => arr i o -> ArrCpsDetectT e r arr i o
mkArrCpsDetectT = box . mkArrCPSx
unArrCpsDetectT :: Arrow arr => ArrCpsDetectT e r arr i r -> arr i (Either e r)
unArrCpsDetectT a = (unArrCPSx . unbox $ a >>^ Right) >>^ neEr2eEr where
    neEr2eEr (Right r) = Right r
    neEr2eEr (Left (0, e)) = Left e
    neEr2eEr (Left _) = error "logic error"
change_resultCpsDetectT, relabelR_CpsDetectT
    :: (ArrowChoice arr)
    => arr e e' -> arr r r'
    -> ArrCpsDetectT e r arr i o -> ArrCpsDetectT e' r' arr i o
change_resultCpsDetectT e2a r2s = mapGBox $ change_resultCPSx neEr2naEs where
    neEr2naEs = mk e2a r2s
    mk = mk_neEr2naEs
    mk_neEr2naEs e2a r2s = second e2a +++ r2s
relabelR_CpsDetectT = change_resultCpsDetectT
instance Boxed (ArrCpsDetectT e r arr i o) where
    type BoxedFrom (ArrCpsDetectT e r arr i o) = ArrCPSx arr (EE e r) i o
    box = ArrCpsDetectT
    unbox = runArrCpsDetectT


{-
-- not exported
box = ArrCpsDetectT
unbox = runArrCpsDetectT
-}

----------------------------
instance (ArrowApply arr) => ArrowApply (ArrCpsDetectT e r arr) where
    app = mk_app box unbox


---------------------------------------
instance Arrow arr => IArrowThrowBaseBy OnExit_Arrow (ArrCpsDetectT e r arr) where
    type ArrowExceptionTypeBy OnExit_Arrow (ArrCpsDetectT e r arr) = r
instance Arrow arr => IArrowThrowBy OnExit_Arrow (ArrCpsDetectT e r arr) where
    throwABy_ = box $ Right ^>> exitCC
instance (Arrow arr) => IArrowCC (ArrCpsDetectT e r arr) where

instance (Arrow arr, ArrowChoice arr)
    => IArrowThrowBaseBy OnException_Arrow (ArrCpsDetectT e r arr) where
    type ArrowExceptionTypeBy OnException_Arrow (ArrCpsDetectT e r arr) = e

type A arr e i o = forall env r. F env r arr e i o
type F env r arr e i o = G env r arr e o -> G env r arr e i
type G env r arr e x = arr (env, x) (Either e r)
instance (Arrow arr, ArrowChoice arr)
    => IArrowThrowBy OnException_Arrow (ArrCpsDetectT e r arr) where
    throwABy_ = box $ Left . (,) 0 ^>> exitCC
instance (Arrow arr, ArrowChoice arr)
    => IArrowCatchBy OnException_Arrow (ArrCpsDetectT e r arr) where
    catchABy_ = mapGBox _catchA__ArrCPSx
    {-
    catchABy
        :: forall i o ne eEo neEr proxy by
        . (ne ~ (Natural, e), eEo ~ Either e o, neEr ~ EE e r)
        => proxy by -> ArrCpsDetectT e r arr i o -> ArrCpsDetectT e r arr i eEo
    catchABy _ i2o@(ArrCpsDetectT (ArrCPSx vo2neEr_E_s__to__vi2neEr_E_s)) = i2eEo where
        i2eEo = ArrCpsDetectT $ ArrCPSx v_eEo2neEr_E_s__to__vi2neEr_E_s

        v_eEo2neEr_E_s__to__vi2neEr_E_s
            :: forall env s. F env s arr neEr i eEo
        v_eEo2neEr_E_s__to__vi2neEr_E_s =
            catchCPS catch throw vo2neEr_E_s__to__vi2neEr_E_s

        --catch :: arr vi (Either (EE e r) s) -> arr vi (EE e rEs)
        --throw :: arr (EE e rEs) (Either (EE e r) s)
        catch
            :: forall vi s rEs ne_E_rEs
            . (rEs ~ Either r s, ne_E_rEs ~ EE e rEs)
            => arr vi (Either neEr s) -> arr vi ne_E_rEs
        throw
            :: forall s rEs ne_E_rEs
            . (rEs ~ Either r s, ne_E_rEs ~ EE e rEs)
            => arr ne_E_rEs (Either neEr s)
        catch = (>>^ neEr_E_s__to__ne_E_rEs)
        throw = (arr ne_E_rEs__to__neEr_E_s)
        neEr_E_s__to__ne_E_rEs (Right s)        = Right (Right s)
        neEr_E_s__to__ne_E_rEs (Left (Right r)) = Right (Left r)
        neEr_E_s__to__ne_E_rEs (Left (Left ne)) = (Left ne)
        ne_E_rEs__to__neEr_E_s (Right (Right s))= (Right s)
        ne_E_rEs__to__neEr_E_s (Right (Left r)) = (Left (Right r))
        ne_E_rEs__to__neEr_E_s (Left ne)        = (Left (Left ne))
    -}




-------------------------------------------
class (IArrowThrowBy by (ArrCpsDetectT e r arr)
    , IArrowResetBy by arr, OpDefaultValue e, ArrowChoice arr)
    => IArrCpsDetectT_Base by e r arr
instance (IArrowThrowBy by (ArrCpsDetectT e r arr)
    , IArrowResetBy by arr, OpDefaultValue e, ArrowChoice arr)
    => IArrCpsDetectT_Base by e r arr


-- can throw, but cannot be catch, useless!!
-- here, only to support OpDetectSuccessABy...
data ByWrapped_ArrCpsDetectT (by :: *)
class (IArrowThrowBy by arr, IArrowResetBy by arr, OpLookAheadABy by arr
    , OpDefaultValue e, ArrowChoice arr)
    => IArrCpsDetectT_WrappedBase by e r arr
instance (IArrowThrowBy by arr, IArrowResetBy by arr, OpLookAheadABy by arr
    , OpDefaultValue e, ArrowChoice arr)
    => IArrCpsDetectT_WrappedBase by e r arr
{-
instance (IArrCpsDetectT_Base (ByWrapped_ArrCpsDetectT by) e r arr
    , OpLookAheadABy by arr)
    => IArrCpsDetectT_WrappedBase by e r arr
-}
instance IArrowThrowBaseBy by arr
    => IArrowThrowBaseBy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr)
    where
    type ArrowExceptionTypeBy
            (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr)
            = ArrowExceptionTypeBy by arr

instance IArrowThrowBy by arr
    => IArrowThrowBy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr) where
    throwABy_ = box $ throwABy p where
        p = Proxy :: Proxy (ByWrapped_ArrCPSx by)

instance (IArrCpsDetectT_WrappedBase by e r arr)
    => IArrowResetBy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr) where
instance (IArrCpsDetectT_WrappedBase by e r arr)
    => OpDoOrNopABy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr) where
    -- using look_aheadA
instance (IArrCpsDetectT_WrappedBase by e r arr)
    => OpDetectSuccessABy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr) where
    -- using look_aheadA
instance (IArrCpsDetectT_WrappedBase by e r arr)
    => OpLookAheadABy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr) where
    look_aheadABy p (ArrCpsDetectT i2o) = (box $ look_aheadABy p' i2o) where
        p' = Proxy :: forall . Proxy (ByWrapped_ArrCPSx by)
    --look_aheadABy = withBy look_aheadABy_


{-
instance (IArrCpsDetectT_Base by e r arr
    , IArrowResetBy by arr, OpDefaultValue e, ArrowChoice arr)
    => IArrowResetBy by (ArrCpsDetectT e r arr) where
instance (IArrCpsDetectT_Base by e r arr
    , OpLookAheadABy by arr, OpDefaultValue e, ArrowChoice arr)
    => OpDoOrNopABy by (ArrCpsDetectT e r arr) where
    -- using look_aheadA
instance (IArrCpsDetectT_Base by e r arr, OpLookAheadABy by arr
    , OpDefaultValue e, ArrowChoice arr)
    => OpDetectSuccessABy by (ArrCpsDetectT e r arr) where
    -- using look_aheadA
instance (IArrCpsDetectT_Base by e r arr, OpLookAheadABy by arr
    , OpDefaultValue e, ArrowChoice arr)
    => OpLookAheadABy (ByWrapped_ArrCpsDetectT by) (ArrCpsDetectT e r arr) where
    look_aheadABy p (ArrCpsDetectT i2o) = (box $ look_aheadABy p' i2o) where
        p' = Proxy :: forall . Proxy (ByWrapped_ArrCPSx by)
    --look_aheadABy = withBy look_aheadABy_
{-
instance (IArrCpsDetectT_Base by e r arr, OpLookAheadABy by arr
    , OpDefaultValue e, ArrowChoice arr)
    => OpLookAheadABy by (ArrCpsDetectT e r arr) where
    look_aheadABy p (ArrCpsDetectT i2o) =
        catchA (box $ look_aheadABy p' i2o) >>> (constA Nothing ||| id) where
        p' = Proxy :: forall . Proxy (ByWrapped_ArrCPSx by)
-}
-}






-------------------
instance (Arrow arr, ArrowChoice arr, OpDefaultValue e)
    => ArrowZero (ArrCpsDetectT e r arr) where
    --zeroArrow = throwA_ default_value
    zeroArrow = throwAtA default_value
instance (Arrow arr, ArrowChoice arr, OpDefaultValue e, ArrowPlus arr)
    => ArrowPlus (ArrCpsDetectT e r arr) where
    (<+>) = mk_arr_plus catchWithoutInputA box box unbox
    {-
    a <+> b = box $ f a <+> f b where
        f a = unbox . catchA_ a $ box zeroArrow
        --f a = unbox (catchA_ a) >>> (zeroArrow ||| id)
    -}

-------------------------------
instance (Arrow arr, ArrowChoice arr, OpDefaultValue e, ArrowPlus arr)
    => Semigroup (ArrCpsDetectT e r arr i o) where
    (<>) = (<+>)
instance (Arrow arr, ArrowChoice arr, OpDefaultValue e, ArrowPlus arr)
    => Monoid (ArrCpsDetectT e r arr i o) where
    mappend = (<+>)
    mempty = zeroArrow
--}
--}
--}
--}
--}
