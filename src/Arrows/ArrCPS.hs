{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arrows.ArrCPS
    ( ArrCPS(..), mkArrCPS, unArrCPS, change_resultCPS, relabelR_CPS
    -- , EE, CPS, catchCPS
    )
where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Seed.ArrowOps (constA, withInput) -- , catchCPS)
    -- swap_e_E_eEr, swap_eEe_E_r
import Seed.EitherOps (a_E_bEc2aEb_E_c, aEb_E_c2a_E_bEc)
import Seed.ArrowOps
    (Arrow2Functor(..), Arrow2Applicative(..), Arrow2Monad(..))
import Seed.Boxed
import ADT.IArrowExit
import ADT.IArrowCatch
--import ADT.IArrowSuccess
import Data.Semigroup
import Numeric.Natural


{-
import Seed.MaybeOps
-}


newtype ArrCPS arr r i o =
    ArrCPS {runArrCPS :: arr o r -> arr i r}

change_resultCPS, relabelR_CPS
    :: (Category arr, ArrCPS arr ~ ar)
    => arr s r -> arr r s -> ar r i o -> ar s i o
change_resultCPS s2r r2s (ArrCPS o2r_to_i2r) = ArrCPS o2s_to_i2s where
    o2s_to_i2s = (r2s .) . o2r_to_i2r . (s2r .)
relabelR_CPS = change_resultCPS
instance Boxed (ArrCPS arr r i o) where
    type BoxedFrom (ArrCPS arr r i o) = arr o r -> arr i r
    box = ArrCPS
    unbox = runArrCPS
unArrCPS :: Category arr => ArrCPS arr r i r -> arr i r
unArrCPS a = unbox a id
mkArrCPS :: Category arr => arr i o -> ArrCPS arr r i o
mkArrCPS a = box $ (. a)


chainCPS
    :: (Category arr, ar ~ ArrCPS arr r) => ar i x -> ar x o -> ar i o
chainCPS (ArrCPS x2r_to_i2r) (ArrCPS o2r_to_x2r)
    = ArrCPS $ x2r_to_i2r . o2r_to_x2r
idCPS :: Category arr => ArrCPS arr r a a
--idCPS = ArrCPS (>>>)
idCPS = box id
arrCPS :: Arrow arr => (i->o) -> ArrCPS arr r i o
arrCPS = box . (>>>) . arr
exitCPS :: (Arrow arr, ar ~ ArrCPS arr r) => r -> ar x y
exitCPS r = ArrCPS y2r_to_x2r where
    y2r_to_x2r y2r = constA r

firstCPS
    :: (ArrowApply arr, ar ~ ArrCPS arr r) => ar i o -> ar (i, x) (o, x)
firstCPS (ArrCPS o2r_to_i2r) = ArrCPS ox2r_to_ix2r where
    ox2r_to_ix2r ox2r = ix2r where
        -- ox2r, o2r_to_i2r
        x_ox2r_to_o2r x ox2r = (id &&& constA x) >>> ox2r
        x_ox2r_to_i2r x ox2r = o2r_to_i2r $ x_ox2r_to_o2r x ox2r
        ox2r_ix_to_i2r_i ox2r (i,x) = (x_ox2r_to_i2r x ox2r, i)

        ix_to_i2r_i = arr $ ox2r_ix_to_i2r_i ox2r
        ix2r = ix_to_i2r_i >>> app

leftCPS
    :: (ArrowChoice arr, ar ~ ArrCPS arr r)
    => ar i o -> ar (Either i x) (Either o x)
leftCPS (ArrCPS o2r_to_i2r) = ArrCPS oEx2r_to_iEx2r where
    oEx2r_to_iEx2r oEx2r = iEx2r where
        -- oEx2r, o2r_to_i2r
        o2r = arr Left >>> oEx2r
        x2r = arr Right >>> oEx2r
        i2r = o2r_to_i2r o2r
        iEx2r = i2r ||| x2r

appCPS
    :: (ArrowApply arr, ar ~ ArrCPS arr r)
    => ar (ar i o, i) o
appCPS = ArrCPS f where
    f o2r = i2oCPS_i_to_r where
        --i2rARR_i_to_r :: arr (arr i r, i) r
        i2rARR_i_to_r = app
        --i2oCPS_i_to_r :: arr (ar i o, i) r
        i2oCPS_i_to_r = first (arr $ \i2oCPS -> unbox i2oCPS o2r)
                        >>> i2rARR_i_to_r

plusArrCPS :: (ArrowPlus arr, a ~ ArrCPS arr r i o) => a -> a -> a
ArrCPS a `plusArrCPS` ArrCPS b = ArrCPS $ \o2r -> a o2r <+> b o2r
zeroArrCPS :: (ArrowPlus arr, a ~ ArrCPS arr r i o) => a
--zeroArrCPS = mkArrCPS zeroArrow
zeroArrCPS = box $ const zeroArrow






---------------------------
instance Category arr => Category (ArrCPS arr r) where
    id = idCPS
    (.) = flip chainCPS
instance ArrowApply arr => Arrow (ArrCPS arr r) where
    arr = arrCPS
    first = firstCPS
instance (Arrow (ArrCPS arr r), ArrowChoice arr)
    => ArrowChoice (ArrCPS arr r) where
    left = leftCPS
instance (ArrowApply arr) => ArrowApply (ArrCPS arr r) where
    app = appCPS

instance (ArrowApply arr, ArrowZero arr) => ArrowZero (ArrCPS arr r) where
    zeroArrow = box $ const zeroArrow
instance (ArrowApply arr, ArrowPlus arr) => ArrowPlus (ArrCPS arr r) where
    (<+>) = plusArrCPS


-------------------------------
instance ArrowPlus arr => Semigroup (ArrCPS arr r i o) where
    (<>) = plusArrCPS
instance ArrowPlus arr => Monoid (ArrCPS arr r i o) where
    mappend = plusArrCPS
    mempty = zeroArrCPS
instance Arrow arr => Functor (ArrCPS arr r i) where
    fmap f a = mkArrCPS (arr f) . a
instance ArrowApply arr => Applicative (ArrCPS arr r i) where
    pure = mkArrCPS . arr . const
    (<*>) = (<**>)
instance (ArrowApply arr) => Monad (ArrCPS arr r i) where
    (>>=) = (>>==)




---------------------------------------
instance (ArrowApply arr) => IArrowExitBase (ArrCPS arr r) where
    type ArrowExit_Result (ArrCPS arr r) = r
instance (ArrowApply arr) => IArrowExit (ArrCPS arr r) where
instance (ArrowApply arr) => IArrowCC (ArrCPS arr r) where
    exitCC = box $ const id
{-
instance (ArrowApply arr) => IArrowCC (ArrCPS arr r) where
    type ArrowCC_Result (ArrCPS arr r) = r
    exitCC = box . const . unArrCPS
-}

{-
instance (ArrowApply arr, ArrowChoice arr -- , IArrowCatch arr
        , ArrowExceptionType arr ~ e, ne ~ (Natural, e))
    => IArrowCatch (ArrCPS arr (Either (Natural, e) r)) where
    type ArrowExceptionType (ArrCPS arr (Either (Natural, e) r)) = e
    throwA = Left . (,) 0 ^>> exitCC -- so the original arr without error

    catchA_ex i2o@(ArrCPS o2neEr_to_i2neEr) = i2neEo where
        i2neEo = ArrCPS neEo2neEr_to_i2neEr
        neEo2neEr_to_i2neEr = catchCPS catch catch throw o2neEr_to_i2neEr

        -- catch :: arr i (Either ne r) -> arr i (EE e r)
        -- throw :: arr (EE e r) (Either ne r)
        catch = id
        throw = id
-}

{- catchCPS ver1, error implement
instance (ArrowApply arr, ArrowChoice arr, IArrowCatch arr
        , ArrowExceptionType arr ~ e)
    => IArrowCatch (ArrCPS arr (Either e r)) where
    type ArrowExceptionType (ArrCPS arr (Either e r)) = e
    throwA = Left ^>> exitCC -- so the original arr without error

    catchA_ex i2o@(ArrCPS o2eEr_to_i2eEr) = i2eEo where
        i2eEo = ArrCPS eEo2eEr_to_i2eEr
        eEo2eEr_to_i2eEr = catchCPS catch catch throw o2eEr_to_i2eEr
        {-
        catch = catchA_ex
        throw = throwA ||| id
        -}
        -- catch :: arr i (Either e r) -> arr i (EE e r)
        -- throw :: arr (EE e r) (Either e r)
        catch a = catchA_ex a >>^ a_E_bEc2aEb_E_c
        throw = aEb_E_c2a_E_bEc ^>> (throwA ||| id)
-}

{-
instance (ArrowApply arr, ArrowChoice arr, IArrowCatch arr
        , ArrowExceptionType arr ~ e)
    => IArrowSuccess (ArrCPS arr (Either e r)) where
-}
{-
instance (ArrowApply arr, ArrowChoice arr)
    => IArrowCatch (ArrCPS arr (Either e r)) where
    type ArrowExceptionType (ArrCPS arr (Either e r)) = e
    throwA = exit . Left
    catchA i2o@(ArrCPS o2r_to_i2r) ei2o@(ArrCPS o2r_to_ei2r) = i2o' where
        i2o' = ArrCPS o2r_to_i2r'
        o2r_to_i2r' o2r = i2r' where
            i2r = o2r_to_i2r o2r
            ei2r = o2r_to_ei2r o2r
            -- r is e or r'
            i2eEr = i2r  -------- catch error raise in o2r!!
            ei2eEr = ei2r

            eEr_i_to_eiEr (eEr, i) = case eEr of
                Left e -> Left (e,i)
                Right r -> Right r
            eErEr2eEr eErEr = case eErEr of
                Right r -> Right r
                Left eEr -> eEr
            i2eErEr = withInput i2eEr >>> arr eEr_i_to_eiEr >>> left ei2eEr
            i2r' = i2eErEr >>> arr eErEr2eEr
-}


{-
instance (ArrowApply arr, ArrowZero arr)
    => OpDetectSuccessA (ArrCPS arr (Either e r)) where
    -- detect_successA :: arr i o -> arr i (Maybe o)
    catchA i2o@(ArrCPS o2r_to_i2r) = i2mo' where
        i2mo' = ArrCPS mo2r_to_i2r'
        mo2r_to_i2r' mo2r = i2r' where
            o2r = arr Just >>> mo2r
            x2r = constA Nothing >>> mo2r
            i2r = o2r_to_i2r o2r
            _i2r_ = o2r_to_i2r $ constA (Right undefined)
            ----- !!!!!!!!!!!!                 ???????????????????
            -- r is e or r'
            i2eEr = i2r
            _i2eEr_ = _i2r_
            _i2eEr_ >>> (x2r ||| o2r)

-}


