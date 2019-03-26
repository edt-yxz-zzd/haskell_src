{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Arrows.ArrCPSx
    ( ArrCPSx() -- hiding constructor
    , mkArrCPSx, unArrCPSx, change_resultCPSx, relabelR_CPSx
    , ByWrapped_ArrCPSx
    , _catchA__ArrCPSx
    )
where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Seed.ArrowOps (constA, withInput)
--import Seed.EitherOps
import Seed.ArrowOps
    (Arrow2Functor(..), Arrow2Applicative(..), Arrow2Monad(..))
import Seed.OpSwap
import Seed.ProxyOps (last1P)

--import Seed.Boxed
import ADT.IArrowExit
import ADT.IArrowCatch
import ADT.IArrowSuccessBy
import Data.Semigroup
import Numeric.Natural
import Seed.UnsafeUnpack (unsafe_unRight, unsafe_unLeft)


import Seed.Op_catchCPSx(catchCPS, EE, CPS)


{-
import Seed.MaybeOps
-}


type A arr e i o = forall env r. F env r arr e i o
type F env r arr e i o = G env r arr e o -> G env r arr e i
type G env r arr e x = arr (env, x) (Either e r)
newtype ArrCPSx arr e i o =
    ArrCPSx {runArrCPSx :: A arr e i o}



change_resultCPSx, relabelR_CPSx
    :: (ArrowChoice arr, ArrCPSx arr ~ ar)
    => arr e e' -> ar e i o -> ar e' i o
change_resultCPSx r2t (ArrCPSx o2s_to_i2rEs) = ArrCPSx (_f r2t o2s_to_i2rEs) where
    _f :: ArrowChoice arr => arr e e' -> A arr e i o -> A arr e' i o
    _f r2t o2rEs_to_i2rEs o2tEs =
        o2rEs_to_i2rEs (o2tEs >>^ Right) >>> left r2t >>^ tEtEs2tEs where
        tEtEs2tEs (Left t) = Left t
        tEtEs2tEs (Right tEs) = tEs
relabelR_CPSx = change_resultCPSx
{-
instance Boxed (ArrCPSx arr e i o) where
    type BoxedFrom (ArrCPSx arr e i o) = forall s. arr o s -> arr i (Either r s)
    box = ArrCPSx
    unbox = runArrCPSx
-}

box = ArrCPSx
unbox = runArrCPSx
unArrCPSx :: Arrow arr => ArrCPSx arr e i e -> arr i e
unArrCPSx a = (,) () ^>> unbox a (arr $ Left . snd) >>^ unsafe_unLeft

-- the only constructor; to support look_ahead
mkArrCPSx :: Arrow arr => arr i o -> ArrCPSx arr e i o
mkArrCPSx a = box $ (second a >>>)


chainCPSx
    :: (Category arr, ar ~ ArrCPSx arr e) => ar i x -> ar x o -> ar i o
chainCPSx (ArrCPSx x2r_to_i2r) (ArrCPSx o2r_to_x2r)
    = ArrCPSx $ x2r_to_i2r . o2r_to_x2r
idCPSx :: ArrCPSx arr e a a
idCPSx = box id
arrCPSx :: Arrow arr => (i->o) -> ArrCPSx arr e i o
arrCPSx = mkArrCPSx . arr
exitCPSx
    :: forall arr e x y ar. (Arrow arr, ar ~ ArrCPSx arr e)
    => e -> ar x y
exitCPSx e = ArrCPSx y2r_to_x2r where
    y2r_to_x2r :: forall. A arr e x y
    y2r_to_x2r y2r = constA (Left e)

firstCPSx
    :: forall ar arr i o x e. (Arrow arr, ar ~ ArrCPSx arr e)
    => ar i o -> ar (i, x) (o, x)
firstCPSx (ArrCPSx vo2eEr__to__vi2eEr) = ArrCPSx v_ox2eEr__to__v_ix2eEr where
    vx_o2eEr__to__vx_i2eEr :: forall env r. F (env,x) r arr e i o
    vx_o2eEr__to__vx_i2eEr = vo2eEr__to__vi2eEr
    v_ox2eEr__to__v_ix2eEr :: forall env r. F env r arr e (i,x) (o,x)
    v_ox2eEr__to__v_ix2eEr = f vx_o2eEr__to__vx_i2eEr
    f vx_o2eEr__to__vx_i2eEr = v_ox2eEr__to__v_ix2eEr where
        v_ox2eEr__to__v_ix2eEr v_ox2eEr = v_ix2eEr where
            vx_o2eEr = rotateRA >>> arr (fmap swap) >>> v_ox2eEr
            vx_i2eEr = vx_o2eEr__to__vx_i2eEr vx_o2eEr
            v_ix2eEr = arr (fmap swap) >>> rotateLA >>> vx_i2eEr
leftCPSx
    :: forall ar arr e i o x. (ArrowChoice arr, ar ~ ArrCPSx arr e)
    => ar i o -> ar (Either i x) (Either o x)

leftCPSx (ArrCPSx vo2eEr__to__vi2eEr) = ArrCPSx v_oEx2eEr__to__v_iEx2eEr where
    v_oEx2eEr__to__v_iEx2eEr
        :: forall env r. F env r arr e (Either i x) (Either o x)
    v_oEx2eEr__to__v_iEx2eEr v_oEx2eEr = v_iEx2eEr where
        vo2eEr = fmap Left ^>> v_oEx2eEr
        vx2eEr = fmap Right ^>> v_oEx2eEr
        vi2eEr = vo2eEr__to__vi2eEr vo2eEr
        v_iEx2eEr = v_iEx2viEvx ^>> viEvx2eEr
        viEvx2eEr = vi2eEr ||| vx2eEr
        v_iEx2viEvx (v, Left i) = Left (v, i)
        v_iEx2viEvx (v, Right x) = Right (v, x)


appCPSx
    :: (ArrowApply arr, ar ~ ArrCPSx arr e)
    => ar (ar i o, i) o
appCPSx = ArrCPSx f where
    f vo2eEr = v__i2oCPSx_i___to_eEr where
        --v__i2oCPSx_i___to_eEr :: arr (v, (ar i o, i)) eEr
        --v__vo2eErARR2vi2eErARR_i___to_eEr
        --      :: arr (v, (arr vo eEr -> arr vi eEr, i)) eEr
        --vo2eErARR2vi2eErARR_vi_to_eEr
        --      :: arr (arr vo eEr -> arr vi eEr, vi) eEr
        --vi2eErARR_vi_to_eEr
        --      :: arr (arr vi eEr, vi) eEr
        v__i2oCPSx_i___to_eEr =
            second (first $ arr unbox) >>> v__vo2eErARR2vi2eErARR_i___to_eEr
        v__vo2eErARR2vi2eErARR_i___to_eEr =
            a_bc2b_ac ^>> vo2eErARR2vi2eErARR_vi_to_eEr
        vo2eErARR2vi2eErARR_vi_to_eEr =
            first (arr ($ vo2eEr)) >>> vi2eErARR_vi_to_eEr
        vi2eErARR_vi_to_eEr = app
        a_bc2b_ac (a,(b,c)) = (b,(a,c))

plusArrCPSx :: (ArrowPlus arr, a ~ ArrCPSx arr e i o) => a -> a -> a
ArrCPSx a `plusArrCPSx` ArrCPSx b = ArrCPSx $ \o2r -> a o2r <+> b o2r
zeroArrCPSx :: (ArrowPlus arr, a ~ ArrCPSx arr e i o) => a
zeroArrCPSx = mkArrCPSx zeroArrow
--zeroArrCPSx = box $ const zeroArrow






---------------------------
instance Category arr => Category (ArrCPSx arr e) where
    id = idCPSx
    (.) = flip chainCPSx
instance Arrow arr => Arrow (ArrCPSx arr e) where
    arr = arrCPSx
    first = firstCPSx
instance (ArrowChoice arr) => ArrowChoice (ArrCPSx arr e) where
    left = leftCPSx
instance (ArrowApply arr) => ArrowApply (ArrCPSx arr e) where
    app = appCPSx

instance (ArrowZero arr) => ArrowZero (ArrCPSx arr e) where
    zeroArrow = box $ const zeroArrow
instance (ArrowPlus arr) => ArrowPlus (ArrCPSx arr e) where
    (<+>) = plusArrCPSx


-------------------------------
instance ArrowPlus arr => Semigroup (ArrCPSx arr e i o) where
    (<>) = plusArrCPSx
instance ArrowPlus arr => Monoid (ArrCPSx arr e i o) where
    mappend = plusArrCPSx
    mempty = zeroArrCPSx
instance Arrow arr => Functor (ArrCPSx arr e i) where
    fmap f a = mkArrCPSx (arr f) . a
instance Arrow arr => Applicative (ArrCPSx arr e i) where
    pure = mkArrCPSx . arr . const
    (<*>) = (<**>)
instance (ArrowApply arr) => Monad (ArrCPSx arr e i) where
    (>>=) = (>>==)




---------------------------------------
instance (Arrow arr) => IArrowExitBase (ArrCPSx arr e) where
    type ArrowExit_Result (ArrCPSx arr e) = e
instance (Arrow arr) => IArrowExit (ArrCPSx arr e) where
instance (Arrow arr) => IArrowCC (ArrCPSx arr e) where
    exitCC = box $ const . arr $ Left . snd





--------------------------
data ByWrapped_ArrCPSx on_err

instance (IArrowThrowBaseBy by arr)
    => IArrowThrowBaseBy (ByWrapped_ArrCPSx by) (ArrCPSx arr e) where
    type ArrowExceptionTypeBy (ByWrapped_ArrCPSx by) (ArrCPSx arr e)
        = ArrowExceptionTypeBy by arr
instance (IArrowThrowBy by arr)
    => IArrowThrowBy (ByWrapped_ArrCPSx by) (ArrCPSx arr e) where
    throwABy = mkArrCPSx . throwABy . last1P
{- cannot catch!!
instance (IArrowCatchBy by arr)
    => IArrowCatchBy (ByWrapped_ArrCPSx by) (ArrCPSx arr e) where
    catchABy p = mkArrCPSx . (catchABy $ last1P p) . unArrCPSx
-}

instance (IArrowResetBy by arr) --, ArrowZero arr)
    => IArrowResetBy (ByWrapped_ArrCPSx by) (ArrCPSx arr e) where
instance (OpDetectSuccessABy by arr) -- , ArrowZero arr)
    => OpDetectSuccessABy (ByWrapped_ArrCPSx by) (ArrCPSx arr e) where
    detect_successABy
        :: forall ar i o proxy. (ar ~ ArrCPSx arr e)
        => proxy (ByWrapped_ArrCPSx by) -> ar i o -> ar i Bool
    detect_successABy by i2o@(ArrCPSx vo2eEr_to_vi2eEr) = i2b where
        -- let r ~ Maybe o
        i2b = ArrCPSx vb2eEr_to_vi2eEr
        vi2eEo :: forall env. G env o arr e i
        vi2eEo = vo2eEr_to_vi2eEr $ arr $ Right . snd
        vi2b :: forall env. arr (env,i) Bool
        vi2b = detect_successABy (last1P by) vi2eEo
        vi2vb :: forall env. arr (env,i) (env, Bool)
        vi2vb = arr fst &&& vi2b
        vb2eEr_to_vi2eEr :: forall env r. F env r arr e i Bool
        vb2eEr_to_vi2eEr = (vi2vb >>>)
instance (OpDoOrNopABy by arr) -- , ArrowZero arr)
    => OpDoOrNopABy (ByWrapped_ArrCPSx by) (ArrCPSx arr e) where
    --do_or_nopABy = _may . do_or_nopABy . last1P
    do_or_nopABy by = _may $ do_or_nopABy $ last1P by

_may
    :: forall ar arr e i o . (ar ~ ArrCPSx arr e, Arrow arr)
    => (forall i o. arr i o -> arr i (Maybe o))
    -> (ar i o -> ar i (Maybe o))
_may f i2o@(ArrCPSx vo2eEr_to_vi2eEr) = i2mo where
    -- let r ~ Maybe o
    i2mo = ArrCPSx vmo2eEr_to_vi2eEr
    vi2eEo :: forall env. G env o arr e i
    vi2eEo = vo2eEr_to_vi2eEr $ arr $ Right . snd
    vi2m_eEo :: forall env. arr (env,i) (Maybe (Either e o))
    vi2m_eEo = f vi2eEo
    vi2v_mo :: forall env. arr (env,i) (env, Maybe o)
    vi2v_mo = arr fst &&& (vi2m_eEo >>^ m_eEo2mo)
    vmo2eEr_to_vi2eEr :: forall env r. F env r arr e i (Maybe o)
    vmo2eEr_to_vi2eEr = (vi2v_mo >>>)
    m_eEo2mo (Just (Right o)) = Just o
    m_eEo2mo _ = Nothing
instance (OpLookAheadABy by arr) -- , ArrowZero arr)
    => OpLookAheadABy (ByWrapped_ArrCPSx by) (ArrCPSx arr e) where
    look_aheadABy by = _may (look_aheadABy $ last1P by)
    --look_aheadA :: arr i o -> arr i (Maybe o) -- no side-effect
    {-
    look_aheadA
        :: forall ar i o . (ar ~ ArrCPSx arr e) => ar i o -> ar i (Maybe o)
    look_aheadA i2o@(ArrCPSx vo2eEr_to_vi2eEr) = i2mo where
        -- let r ~ Maybe o
        i2mo = ArrCPSx vmo2eEr_to_vi2eEr
        vi2eEo :: forall env. G env o arr e i
        vi2eEo = vo2eEr_to_vi2eEr $ arr $ Right . snd
        vi2m_eEo :: forall env. arr (env,i) (Maybe (Either e o))
        vi2m_eEo = look_aheadA vi2eEo
        vi2v_mo :: forall env. arr (env,i) (env, Maybe o)
        vi2v_mo = arr fst &&& (vi2m_eEo >>^ m_eEo2mo)
        vmo2eEr_to_vi2eEr :: forall env r. F env r arr e i (Maybe o)
        vmo2eEr_to_vi2eEr = (vi2v_mo >>>)
        m_eEo2mo (Just (Right o)) = Just o
        m_eEo2mo _ = Nothing
    -}







------------- for ArrCpsDetectT__private::catchABy
type ArrCpsDetectT e r arr i o = ArrCPSx arr (EE e r) i o
_catchA__ArrCPSx
    :: forall e r arr
    {- -} i o ne eEo neEr
    . (Arrow arr, ArrowChoice arr
    , {- -} ne ~ (Natural, e), eEo ~ Either e o, neEr ~ EE e r)
    => ArrCpsDetectT e r arr i o -> ArrCpsDetectT e r arr i eEo
_catchA__ArrCPSx i2o@(ArrCPSx vo2neEr_E_s__to__vi2neEr_E_s) = i2eEo where
    i2eEo = ArrCPSx v_eEo2neEr_E_s__to__vi2neEr_E_s

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



--}
--}
--}
--}
