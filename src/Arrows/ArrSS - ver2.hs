{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE MultiParamTypeClasses #-}
module Arrows.ArrSS
    ( ArrSS()
    , pureArrSS, impureArrSS, success_impureArrSS, removeSS_
    )
where

import ADT.IArrowSuccessBy
import ADT.IArrowCatch
import ADT.IArrowExit

import Control.Arrow
import Seed.ArrowOps (mk_app, fmapA2M, pureA2M, (<**>), (>>==), bothA, constA)
import Control.Category
import Prelude hiding ((.), id)
import Seed.CategoryData -- (CategoryPair(..))
import Data.Semigroup
import ADT.IFunctorA
import Seed.Types (Op)
import Seed.ProxyOps (withBy, withBy_)
{-
import Seed.MaybeOps (isJust)
import Seed.EitherOps (mayRight)
-}



data R_ID arr i o where
    R :: arr i o -> R_ID arr i o
    RI :: R_ID arr i i
newtype P arr i o = P (arr i o)
    deriving (Category, Arrow, ArrowChoice -- , ArrowApply
            , ArrowZero, ArrowPlus, ArrowLoop
            --, Arrow
            )
data U arr i o where
    U :: R_ID arr i x -> P arr x o -> U arr i o
data ArrSS arr i o where
    ArrSS :: R_ID arr i x -> U arr x o -> ArrSS arr i o
    -- === R_ID >>> SS >>> R_ID >>> P
($$) = ArrSS
infixl 1 $$
pattern U_RP r p = U r (P p)
pattern SS_RRP r r' p = ArrSS r (U r' p)

pureArrSS :: arr i o -> ArrSS arr i o
impureArrSS :: Category arr => arr i o -> ArrSS arr i o
success_impureArrSS :: Category arr => arr i o -> ArrSS arr i o
pureArrSS a = RI $$ U RI (P a)
impureArrSS a = R a $$ U RI (P id)
success_impureArrSS a = RI $$ U (R a) (P id)
removeSS :: Category arr => ArrSS arr i o -> U arr i o
removeSS (ArrSS r (U r' p)) = U (chainR_ID r r') p
u2a :: Category arr => U arr i o -> arr i o
u2a (U (R r) (P p)) = r >>> p
u2a (U RI (P p)) = p
r2a :: Category arr => R_ID arr i o -> arr i o
r2a (R r) = r
r2a RI = id
ss2a :: Category arr => ArrSS arr i o -> arr i o
ss2a (ArrSS r u) = r2a r >>> u2a u
r2ss :: Category arr => Bool -> R_ID arr i o -> ArrSS arr i o
r2ss False (R r) = impureArrSS r
r2ss True (R r) = success_impureArrSS r
r2ss _ RI = id

removeSS_ :: Category arr => ArrSS arr i o -> arr i o
removeSS_ = u2a . removeSS

chainR_ID :: Category arr => R_ID arr i x -> R_ID arr x o -> R_ID arr i o
chainR_ID (R a) (R b) = R $ a >>> b
chainR_ID RI r = r
chainR_ID r RI = r
chainU :: Category arr => U arr i x -> U arr x o -> U arr i o
chainU (U r (P p)) (U RI (P p')) = U r . P $ p >>> p'
chainU u (U (R r) p) = U (R $ u2a u >>> r) p

--pattern ($$) = ArrSS

chainArrSS :: Category arr => ArrSS arr i x -> ArrSS arr x o -> ArrSS arr i o
lhs `chainArrSS` ArrSS (R r) u = R (removeSS_ lhs >>> r) $$ u
ArrSS r u `chainArrSS` ArrSS RI u' = ArrSS r (chainU u u')


mul_R_ID
    :: Arrow arr => R_ID arr a a' -> R_ID arr b b' -> R_ID arr (a,b) (a', b')
mul_R_ID RI RI = RI
mul_R_ID lhs rhs = R (r2a lhs *** r2a rhs)
add_R_ID
    :: ArrowChoice arr => R_ID arr a a' -> R_ID arr b b'
    -> R_ID arr (Either a b) (Either a' b')
add_R_ID RI RI = RI
add_R_ID lhs rhs = R (r2a lhs +++ r2a rhs)
add_U
    :: ArrowChoice arr => U arr a a' -> U arr b b'
    -> U arr (Either a b) (Either a' b')
add_U (U r1 p1) (U r2 p2) = U (add_R_ID r1 r2) (p1 +++ p2)
or_U :: ArrowChoice arr => U arr a o -> U arr b o -> U arr (Either a b) o
or_U (U r1 p1) (U r2 p2) = U (add_R_ID r1 r2) (p1 ||| p2)
plus_U :: (ArrowChoice arr, ArrowPlus arr) => Op (U arr i o)
plus_U (U r1 p1) (U r2 p2) = case both_R_ID r1 r2 of
    (True, a) -> U RI $ P a >>> tail
    (False, a) -> U (R a) tail
    where tail = p1 ||| p2

both_R_ID
    :: (ArrowChoice arr, ArrowPlus arr)
    => R_ID arr i a -> R_ID arr i b -> (Bool, arr i (Either a b))
        -- (is_pure, arr)
both_R_ID RI RI = (True, bothA)
both_R_ID r1 r2 = (False, bothA >>> (r2a r1 +++ r2a r2))

{-
first_R_ID :: Arrow arr => R_ID arr i o -> R_ID arr (i,x) (o,x)
first_U :: Arrow arr => U arr i o -> U arr (i,x) (o,x)
first_R_ID (R r) = R (first r)
first_R_ID RI = RI
first_U (U r p) = U (first_R_ID r) (first p)
instance Category arr => Category (R_ID arr) where
    id = RI
    (.) = flip chainR_ID
instance Category arr => Category (U arr) where
    id = U RI id
    (.) = flip chainU
instance Arrow arr => Arrow (R_ID arr) where
    arr = R . arr
    first = first_R_ID
    RI *** RI = RI
    lhs *** rhs R (r2a lhs *** r2a rhs)
instance Arrow arr => Arrow (U arr) where
    arr = U RI . arr
    first = first_U
    U r1 p1 *** U r2 p2 = U ()
-}
instance Category arr => Category (ArrSS arr) where
    id = pureArrSS id
    (.) = flip chainArrSS
instance Arrow arr => Arrow (ArrSS arr) where
    arr = pureArrSS . arr
    SS_RRP r1x r1y p1 *** SS_RRP r2x r2y p2 = SS_RRP r3x r3y p3 where
            r3x = (r1x `chainR_ID` r1y) `mul_R_ID` r2x
            r3y =                    RI `mul_R_ID` r2y
            p3  =                    p1    ***     p2
instance ArrowChoice arr => ArrowChoice (ArrSS arr) where
    ArrSS r1 u1 +++ ArrSS r2 u2 = ArrSS r3 u3 where
            r3 = r1 `add_R_ID` r2
            u3 = u1  `add_U`   u2

instance ArrowApply arr => ArrowApply (ArrSS arr) where
    app = mk_app impureArrSS ss2a
instance ArrowZero arr => ArrowZero (ArrSS arr) where
    zeroArrow = impureArrSS zeroArrow
instance (ArrowPlus arr, ArrowChoice arr) => ArrowPlus (ArrSS arr) where
    ArrSS (R r1) u1 <+> ArrSS (R r2) u2 = ArrSS (R r3) u3 where
            r3 = bothA >>> (r1 +++ r2)
            u3 = u1 `or_U` u2
    lhs <+> rhs = ArrSS RI u3 where
        u3 = removeSS lhs `plus_U` removeSS rhs


class (IArrowThrowBy by (ArrSS arr)) => IArrSS_Base by arr
instance (IArrowThrowBy by (ArrSS arr)) => IArrSS_Base by arr

instance (IArrowResetBy by arr, IArrSS_Base by arr)
    => IArrowResetBy by (ArrSS arr) where
instance (OpDetectSuccessABy by arr, IArrSS_Base by arr)
    => OpDetectSuccessABy by (ArrSS arr) where
    detect_successABy_ ss = case removeSS ss of
        U (R r) p -> success_impureArrSS . detect_successABy_ $ r
        U RI p -> constA True
    detect_successABy = withBy detect_successABy_
instance (OpLookAheadABy by arr, ArrowChoice arr, IArrSS_Base by arr)
    => OpLookAheadABy by (ArrSS arr) where
    look_aheadABy_ ss = case removeSS ss of
        U (R r) p -> SS_RRP RI (R $ look_aheadABy_ r) (fmapA p)
        U RI (P p) -> pureArrSS $ p >>^ Just
    look_aheadABy = withBy look_aheadABy_
instance (OpDoOrNopABy by arr, ArrowChoice arr, IArrSS_Base by arr)
    => OpDoOrNopABy by (ArrSS arr) where
    do_or_nopABy_ ss = case removeSS ss of
        U (R r) p -> SS_RRP RI (R $ do_or_nopABy_ r) (fmapA p)
        U RI (P p) -> pureArrSS $ p >>^ Just
    do_or_nopABy = withBy do_or_nopABy_
instance (OpDetectSuccessABy by arr, IArrSS_Base by arr)
    => IArrowSuccessBy by (ArrSS arr) where
    setSSBy _ a b = case (removeSS a, removeSS b) of
        (U r1 p1, u2) -> ArrSS r1 $ (U RI p1 `chainU` u2)
instance (Category arr, OpDetectSuccessABy by arr, IArrSS_Base by arr)
    => OpGetSSBy by (ArrSS arr) where
    getSSBy_ (ArrSS (R r) u) = Chain2 (impureArrSS r) (ArrSS RI u)
    getSSBy_ (ArrSS RI u) = Chain2 id (ArrSS RI u)
instance (Category arr, OpDetectSuccessABy by arr, IArrSS_Base by arr)
    => OpGetSSBy_Ex by (ArrSS arr) where
    getSSBy_ex_ (SS_RRP r1 r2 (P p))
        = Chain3 (r2ss False r1) (r2ss True r2) (pureArrSS p)

instance (OpDetectSuccessABy by arr, OpDoOrNopABy by arr, ArrowChoice arr
    , IArrSS_Base by arr)
    => IArrowBiasedPlusSSBy by (ArrSS arr) where
instance (OpDetectSuccessABy by arr, OpDoOrNopABy by arr, ArrowChoice arr
    , IArrSS_Base by arr)
    => IArrowBiasedPlusBy by arr where




------------------
{-
instance (IArrowThrowBaseBy by arr, ArrowChoice arr)
    => IArrowThrowBaseBy by (ArrSS arr) where
    type ArrowExceptionTypeBy by (ArrSS arr) = ArrowExceptionTypeBy by arr
-}
instance (IArrowThrowBaseBy OnException_Arrow arr, ArrowChoice arr)
    => IArrowThrowBaseBy OnException_Arrow (ArrSS arr) where
    type ArrowExceptionTypeBy OnException_Arrow (ArrSS arr) = ArrowExceptionTypeBy OnException_Arrow arr
{-
instance (IArrowThrowBaseBy OnZero_Arrow arr, ArrowChoice arr)
    => IArrowThrowBaseBy OnZero_Arrow (ArrSS arr) where
    type ArrowExceptionTypeBy OnZero_Arrow (ArrSS arr) = ArrowExceptionTypeBy OnZero_Arrow arr
instance (IArrowThrowBaseBy OnExit_Arrow arr, ArrowChoice arr)
    => IArrowThrowBaseBy OnExit_Arrow (ArrSS arr) where
    type ArrowExceptionTypeBy OnExit_Arrow (ArrSS arr) = ArrowExceptionTypeBy OnExit_Arrow arr
-}
instance (IArrowThrowBy OnException_Arrow arr, ArrowChoice arr)
    => IArrowThrowBy OnException_Arrow (ArrSS arr) where
    throwABy_ = impureArrSS throwABy_
instance (IArrowCatchBy OnException_Arrow arr, ArrowChoice arr)
    => IArrowCatchBy OnException_Arrow (ArrSS arr) where
    catchABy_ ss = case removeSS ss of
        U RI (P p) -> pureArrSS p >>^ Right
        U (R r) p -> SS_RRP RI (R $ catchABy_ r) (fmapA p)
instance (IArrowExit arr) => IArrowExitBase (ArrSS arr) where
    type ArrowExit_Result (ArrSS arr) = ArrowExit_Result arr
instance (IArrowExit arr) => IArrowExit (ArrSS arr) where
    exitA = impureArrSS exitA
instance (IArrowCC arr) => IArrowCC (ArrSS arr) where
    exitCC = impureArrSS exitCC





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













--}
--}
--}
--}
--}
--}
