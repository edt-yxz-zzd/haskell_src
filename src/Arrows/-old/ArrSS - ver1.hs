{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}



module Arrows.ArrSS
    ( ArrSS()
    , pureArrSS
    , setArrSS
    , getArrSS
    )
where

import ADT.IArrowSuccess
import ADT.IArrowCatch
import ADT.IArrowCC

import Control.Arrow
import Seed.ArrowOps (mk_app, fmapA2M, pureA2M, (<**>), (>>==))
import Control.Category
import Prelude hiding ((.), id)
import Seed.CategoryData -- (CategoryPair(..))
import Data.Semigroup

{-
import Seed.MaybeOps (isJust)
import Seed.EitherOps (mayRight)
-}


{-
type P = (->)
type PR arr = (arr :: * -> * -> *)
{-
ArrSS arr i o = id SS P | id SS PR P | PR SS P | PR SS PR P
    = (Maybe PR, Maybe PR, P)
-}
type MPR arr i o = Maybe (PR arr i o)
data ArrSS arr i o = P (i->o) | PR (arr i o) | (U
-}


newtype ArrSS arr i o = ArrSS { runArrSS :: CategoryPair arr i o }
pureArrSS :: Arrow arr => (i->o) -> ArrSS arr i o
setArrSS :: Category arr => arr i x -> arr x o -> ArrSS arr i o
setArrSS a = ArrSS . Chain2 a
pureArrSS = setArrSS id . arr
getArrSS :: Category arr => ArrSS arr i o -> arr i o
getArrSS = unCategoryPair . _getSS
_getSS = runArrSS
instance (Category arr, OpDetectSuccessA arr)
    => OpGetSS (ArrSS arr) where
    getSS a = case _getSS a of
        Chain2 ss a' -> Chain2 (setArrSS ss id) (setArrSS id a')
instance IArrowReset arr => IArrowReset (ArrSS arr) where
instance OpDetectSuccessA arr => OpDetectSuccessA (ArrSS arr) where
    detect_successA = setArrSS id . detect_successA . getArrSS
instance (OpDoOrNopA arr) => OpDoOrNopA (ArrSS arr) where
    do_or_nopA = setArrSS id . do_or_nopA . getArrSS
instance OpDetectSuccessA arr => IArrowSuccess (ArrSS arr) where
    setSS a b = setArrSS (getArrSS a) (getArrSS b)
    usingSS a b = case _getSS a of
        Chain2 ss a' -> setArrSS ss $ a' >>> getArrSS b
    detectSS a = case _getSS a of
        Chain2 ss _ -> setArrSS id (detect_successA ss)
instance (OpDetectSuccessA arr, OpDoOrNopA arr, ArrowChoice arr)
    => IArrowBiasedPlusSS (ArrSS arr) where
instance (OpDetectSuccessA arr, OpDoOrNopA arr, ArrowChoice arr)
    => IArrowBiasedPlus arr where

------------------
instance (IArrowCatch arr, ArrowChoice arr)
    => IArrowCatch (ArrSS arr) where
    type ArrowExceptionType (ArrSS arr) = ArrowExceptionType arr
    throwA = setArrSS throwA id
    catchA_ex = setArrSS id . catchA_ex . getArrSS
instance (IArrowExit arr) => IArrowExit (ArrSS arr) where
    type ArrowExit_Result (ArrSS arr) = ArrowExit_Result arr
    exitA = setArrSS id exitA
instance (IArrowCC arr) => IArrowCC (ArrSS arr) where
    type ArrowCC_Result (ArrSS arr) = ArrowCC_Result arr
    exitCC = setArrSS id exitCC









---------------------
instance Category arr => Category (ArrSS arr) where
    id = ArrSS id
    a . b = case _getSS a of
        Chain2 ss a' -> setArrSS (getArrSS b >>> ss) $ a'
instance Arrow arr => Arrow (ArrSS arr) where
    arr = pureArrSS
    (***) = _liftOpSS (***)
instance ArrowChoice arr => ArrowChoice (ArrSS arr) where
    (+++) = _liftOpSS (+++)
_liftOpSS op a b = ArrSS $ _getSS a `op` _getSS b
instance ArrowApply arr => ArrowApply (ArrSS arr) where
    app = mk_app (flip setArrSS id) getArrSS
instance ArrowZero arr => ArrowZero (ArrSS arr) where
    zeroArrow = setArrSS zeroArrow id
instance (ArrowPlus arr, ArrowChoice arr) => ArrowPlus (ArrSS arr) where
    (<+>) = _liftOpSS (<+>)



---------------------
instance (ArrowChoice arr, ArrowPlus arr) => Semigroup (ArrSS arr i o) where
    (<>) = (<+>)
instance (ArrowChoice arr, ArrowPlus arr) => Monoid (ArrSS arr i o) where
    mappend = (<+>)
    mempty = zeroArrow
instance Arrow arr => Functor (ArrSS arr i) where
    fmap = fmapA2M
instance Arrow arr => Applicative (ArrSS arr i) where
    pure = pureA2M
    (<*>) = (<**>)
instance (ArrowApply arr) => Monad (ArrSS arr i) where
    (>>=) = (>>==)



