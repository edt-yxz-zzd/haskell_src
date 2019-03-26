{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Arrows.ArrCatchT
    ( ArrCatchT(..), mkArrCatchT, unArrCatchT
    )
where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Seed.EitherOps (mayRight)
import Seed.ArrowOps (constA, withInput, mk_arr_plus)
import Seed.ArrowOps
    (Arrow2Functor(..), Arrow2Applicative(..), Arrow2Monad(..))
import Seed.Boxed
import ADT.IArrowExit
import ADT.IArrowCatch
import ADT.IArrowSuccessBy
--import ADT.OpDefaultValue
import Data.Semigroup
import Seed.ProxyOps (last1P)



{-
import Seed.MaybeOps
-}


type A e arr i o = B e e arr i o
type B e e' arr i o = arr (Either e i) (Either e' o)
newtype ArrCatchT e arr i o =
    ArrCatchT {runArrCatchT :: arr (Either e i) (Either e o)}
instance Boxed (ArrCatchT e arr i o) where
    type BoxedFrom (ArrCatchT e arr i o) = arr (Either e i) (Either e o)
    box = ArrCatchT
    unbox = runArrCatchT

unArrCatchT :: ArrowChoice arr => ArrCatchT e arr i o -> arr e o -> arr i o
unArrCatchT eEi2eEo e2o = Right ^>> unbox eEi2eEo >>> (e2o ||| id)
mkArrCatchT :: ArrowChoice arr => arr i o -> ArrCatchT e arr i o
mkArrCatchT = box . right


instance Category arr => Category (ArrCatchT e arr) where
    id = box id
    (.) = opmapbGBox (.)
instance ArrowChoice arr => Arrow (ArrCatchT e arr) where
    arr = box . arr . fmap
    first (ArrCatchT eEi2eEo) = ArrCatchT eEix2eEox where
        eEix2eEox = e2eEox ||| ix2eEox
        e2eEox = arr Left
        ix2eEox = ix2eEo_x >>^ eEo_x_to_eEox
        i2eEo = Right ^>> eEi2eEo
        ix2eEo_x = first i2eEo
        eEo_x_to_eEox (Left e, _) = Left e
        eEo_x_to_eEox (Right o, x) = Right (o, x)


instance ArrowChoice arr => ArrowChoice (ArrCatchT e arr) where
    left (ArrCatchT eEi2eEo) = ArrCatchT eEiEx2eEoEx where
        eEiEx2eEoEx = e2eEoEx ||| iEx2eEoEx
        iEx2eEoEx = i2eEoEx ||| x2eEoEx
        i2eEo = Right ^>> eEi2eEo
        e2eEoEx = arr Left
        i2eEoEx = i2eEo >>> right (arr Left)
        x2eEoEx = arr $ Right . Right



instance (ArrowChoice arr, ArrowApply arr)
    => ArrowApply (ArrCatchT e arr) where
    app = ArrCatchT e_E_iAo_i_to_eEo where
        e_E_iAo_i_to_eEo = e2eEo ||| iAo_i_to_eEo where
            e2eEo = arr Left
            iAo_i_to_eEo = unbox_iAo_i ^>> eEiAeEo_eEi_to_eEo
            unbox_iAo_i (ArrCatchT eEiAeEo, i) = (eEiAeEo, Right i)
            eEiAeEo_eEi_to_eEo = app

instance (ArrowChoice arr, ArrowZero arr)
    => ArrowZero (ArrCatchT e arr) where
    zeroArrow = mkArrCatchT zeroArrow
instance (ArrowChoice arr, ArrowPlus arr)
    => ArrowPlus (ArrCatchT e arr) where
    (<+>) = opmapbGBox (<+>)
{-
instance (ArrowChoice arr, OpDefaultValue e)
    => ArrowZero (ArrCatchT e arr) where
    zeroArrow = throwA_ default_value
instance (ArrowChoice arr, ArrowPlus arr, OpDefaultValue e)
    => ArrowPlus (ArrCatchT e arr) where
    (<+>) = mk_arr_plus catchA_ box box unbox
    {-
    a <+> b = box $ f a <+> f b where
        f a = unbox (catchA_ex a) >>> (zeroArrow ||| id)
    -}
-}

instance (ArrowChoice arr)
    => IArrowThrowBaseBy OnException_Arrow (ArrCatchT e arr) where
    type ArrowExceptionTypeBy OnException_Arrow (ArrCatchT e arr) = e
instance (ArrowChoice arr)
    => IArrowThrowBy OnException_Arrow (ArrCatchT e arr) where
    throwABy_ = box eEe2eEo where
        e2eEo = arr Left -- i is e
        eEe2eEo = (id ||| id) >>> e2eEo
instance (ArrowChoice arr)
    => IArrowCatchBy OnException_Arrow (ArrCatchT e arr) where
    catchABy_ (ArrCatchT eEi2eEr) = box $ eEi2eEr >>^ Right where



---------------
mk_catchXxxA :: Arrow arr
    => (A e arr i o -> B e e' arr i (Either e o))
    -> (ArrCatchT e arr i o -> ArrCatchT e arr i (Either e' o))
mk_catchXxxA catchA = box . (>>^ f) . catchA . unbox where
        f (Left z) = Right (Left z)
        f (Right (Left e)) = Left e
        f (Right (Right o)) = Right (Right o)

--------- zero
instance (ArrowChoice arr, IArrowCatchZero arr)
    => IArrowCatchZeroBase (ArrCatchT e arr) where
    catchZeroA = mk_catchXxxA catchZeroA
--------- exit
instance (ArrowChoice arr, IArrowExitBase arr)
    => IArrowExitBase (ArrCatchT e arr) where
    type ArrowExit_Result (ArrCatchT e arr) = ArrowExit_Result arr
instance (ArrowChoice arr, IArrowExit arr)
    => IArrowExit (ArrCatchT e arr) where
    exitA = mkArrCatchT exitA
instance (ArrowChoice arr, IArrowCatchExit arr)
    => IArrowCatchExitBase (ArrCatchT e arr) where
    catchExitA = mk_catchXxxA catchExitA



------------- wrapped
data ByWrapped_ArrCatchT by
instance (ArrowChoice arr, IArrowThrowBaseBy by arr)
    => IArrowThrowBaseBy (ByWrapped_ArrCatchT by) (ArrCatchT e arr) where
    type ArrowExceptionTypeBy (ByWrapped_ArrCatchT by) (ArrCatchT e arr)
        = ArrowExceptionTypeBy by arr
instance (ArrowChoice arr, IArrowThrowBy by arr)
    => IArrowThrowBy (ByWrapped_ArrCatchT by) (ArrCatchT e arr) where
    throwABy = mkArrCatchT . throwABy . last1P


-- (ByWrapped_ArrCatchT OnZero_Arrow)

instance (ArrowChoice arr, IArrowResetBy by arr)
    => IArrowResetBy (ByWrapped_ArrCatchT by) (ArrCatchT e arr) where
instance (ArrowChoice arr, OpDetectSuccessABy by arr)
    => OpDetectSuccessABy (ByWrapped_ArrCatchT by) (ArrCatchT e arr) where
    detect_successABy by a = mkArrCatchT $
        detect_successABy (last1P by) (Right ^>> unbox a)


_MeEo__to__eEmo :: Maybe (Either e o) -> Either e (Maybe o)
_MeEo__to__eEmo = f where
    f (Just (Left e)) = Left e
    f (Just (Right o)) = Right (Just o)
    f Nothing = Right Nothing
instance (ArrowChoice arr, OpLookAheadABy by arr)
    => OpLookAheadABy (ByWrapped_ArrCatchT by) (ArrCatchT e arr) where
    look_aheadABy by a = box $
        look_aheadABy (last1P by) (unbox a) >>^ _MeEo__to__eEmo
instance (ArrowChoice arr, OpDoOrNopABy by arr)
    => OpDoOrNopABy (ByWrapped_ArrCatchT by) (ArrCatchT e arr) where
    do_or_nopABy by a = box $
        do_or_nopABy (last1P by) (unbox a) >>^ _MeEo__to__eEmo
instance (ArrowChoice arr, OpDoOrNopABy by arr)
    => IArrowBiasedPlusBy (ByWrapped_ArrCatchT by) (ArrCatchT e arr) where

{-
    look_aheadA a = catchA_ex a >>^ mayRight

--}
--}
--}
--}

