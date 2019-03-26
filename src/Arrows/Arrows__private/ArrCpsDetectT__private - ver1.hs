


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

module Arrows.Arrows__private.ArrCpsDetectT__private
    ( ArrCpsDetectT(), mkArrCpsDetectT, unArrCpsDetectT, change_resultCpsDetectT, relabelR_CpsDetectT
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
import ADT.IArrowCC
import ADT.IArrowCatch
import ADT.IArrowSuccess
import Data.Semigroup
import Arrows.ArrCPSx
import Numeric.Natural
import ADT.OpDefaultValue


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
instance (Arrow arr) => IArrowExitBase (ArrCpsDetectT e r arr) where
    type ArrowExit_Result (ArrCpsDetectT e r arr) = r
instance (Arrow arr) => IArrowExit (ArrCpsDetectT e r arr) where
instance (Arrow arr) => IArrowCC (ArrCpsDetectT e r arr) where
    exitCC = box $ Right ^>> exitCC

instance (Arrow arr, ArrowChoice arr)
    => IArrowCatchBase (ArrCpsDetectT e r arr) where
    type ArrowExceptionType (ArrCpsDetectT e r arr) = e

type A arr e i o = forall env r. F env r arr e i o
type F env r arr e i o = G env r arr e o -> G env r arr e i
type G env r arr e x = arr (env, x) (Either e r)
instance (Arrow arr, ArrowChoice arr)
    => IArrowCatch (ArrCpsDetectT e r arr) where
    throwA = box $ Left . (,) 0 ^>> exitCC

    catchA_ex
        :: forall i o ne eEo neEr
        . (ne ~ (Natural, e), eEo ~ Either e o, neEr ~ EE e r)
        => ArrCpsDetectT e r arr i o -> ArrCpsDetectT e r arr i eEo
    catchA_ex i2o@(ArrCpsDetectT (ArrCPSx vo2neEr_E_s__to__vi2neEr_E_s)) = i2eEo where
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




-------------------------------------------
instance (IArrowReset arr, ArrowZero arr, OpDefaultValue e, ArrowChoice arr)
    => IArrowReset (ArrCpsDetectT e r arr) where
instance (OpLookAheadA arr, ArrowZero arr, ArrowChoice arr, OpDefaultValue e, ArrowChoice arr)
    => OpDoOrNopA (ArrCpsDetectT e r arr) where
    -- using look_aheadA
instance (OpLookAheadA arr, ArrowZero arr, ArrowChoice arr, OpDefaultValue e, ArrowChoice arr)
    => OpDetectSuccessA (ArrCpsDetectT e r arr) where
    -- using look_aheadA
instance (OpLookAheadA arr, ArrowZero arr, ArrowChoice arr, OpDefaultValue e, ArrowChoice arr)
    => OpLookAheadA (ArrCpsDetectT e r arr) where
    look_aheadA (ArrCpsDetectT i2o) =
        catchA_ex (box $ look_aheadA i2o) >>> (constA Nothing ||| id)






-------------------
instance (Arrow arr, ArrowChoice arr, OpDefaultValue e)
    => ArrowZero (ArrCpsDetectT e r arr) where
    zeroArrow = throwA_ default_value
instance (Arrow arr, ArrowChoice arr, OpDefaultValue e, ArrowPlus arr)
    => ArrowPlus (ArrCpsDetectT e r arr) where
    (<+>) = mk_arr_plus catchA_ box box unbox
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
