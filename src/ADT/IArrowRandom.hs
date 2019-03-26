{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ADT.IArrowRandom
where

import Seed.ArrowOps (withInput, ab_c2ac_bA, ab2baA)
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Seed.Boxed
import ADT.IGenerater
import ADT.IArrowLift
import ADT.IArrowState
import Seed.ProxyOps


class IArrowRandomBy (ArrowRandom2By arr) arr
    => IArrowRandomDefaultBy arr where
    type ArrowRandom2By arr
    type ArrowRandom2By arr = ()
type Arrow2RandomValue arr = Arrow2RandomValueBy (ArrowRandom2By arr) arr
class IArrowRandomDefaultBy arr => IArrowRandom arr where
    randomA :: arr i (Arrow2RandomValue arr)
    randomA_ :: arr i (Arrow2RandomValue arr, i)
instance (by ~ ArrowRandom2By arr, IArrowRandomDefaultBy arr)
    => IArrowRandom arr where
    randomA = randomByA (Proxy :: Proxy by)
    randomA_ = randomByA_ (Proxy :: Proxy by)
class Arrow arr => IArrowRandomBy by arr where
    type Arrow2RandomValueBy by arr
    randomByA :: proxy by -> arr i (Arrow2RandomValueBy by arr)
    randomByA_ :: proxy by -> arr i (Arrow2RandomValueBy by arr, i)
    randomByA_ = withInput . randomByA
    {-# MINIMAL randomByA #-}
class (IArrowRandomBy by arr, a ~ Arrow2RandomValueBy by arr)
    => IArrowRandomByEx by arr a | by arr -> a where
instance (IArrowRandomBy by arr, a ~ Arrow2RandomValueBy by arr)
    => IArrowRandomByEx by arr a
class (IArrowRandom arr, a ~ Arrow2RandomValue arr)
    => IArrowRandomEx arr a | arr -> a where
instance (IArrowRandom arr, a ~ Arrow2RandomValue arr)
    => IArrowRandomEx arr a



data ArrRandomT v arr i o = ArrRandomT { runArrRandomT :: arr i o }
instance Boxed (ArrRandomT v arr i o) where
    type BoxedFrom (ArrRandomT v arr i o) = arr i o
    box = ArrRandomT
    unbox = runArrRandomT
instance Category arr => Category (ArrRandomT v arr) where
    (.) = opmapbGBox (.)
    id = box id
instance Arrow arr => Arrow (ArrRandomT v arr) where
    arr = box . arr
    first = mapGBox first

instance (IArrowLift arr_g arr, IArrowStateByEx by_st arr g
    , IGeneraterBy by_g arr_g g
    )
    => IArrowRandomBy (arr_g by_st by_g) (ArrRandomT v arr) where
    type Arrow2RandomValueBy (arr_g by_st by_g) (ArrRandomT v arr)
        = Gen2ValueBy by_g arr_g (Arrow2StateBy by_st arr)
    randomByA _ = box i2r where
        i2gen = get_stateByA (Proxy :: Proxy by_st)
        gen2r_gen = lift_generateByAI (Proxy :: Proxy (arr_g by_g x))
        gen_r2r = set_stateByA_ (Proxy :: Proxy by_st)
        i2r = i2gen >>> gen2r_gen >>> ab2baA >>> gen_r2r
instance (IArrowStateDefaultBy arr, IGeneraterDefaultBy arr g
    , ArrowState2By arr ~ by_st, Generater2By arr_g g ~ by_g
    , Arrow2State arr ~ st, st ~ g, arr_g ~ arr
    , v ~ Gen2Value arr_g g
    )
    => IArrowRandomDefaultBy (ArrRandomT v arr) where
    --type ArrowRandom2By (ArrRandomT v arr) = arr by_st by_g
    type ArrowRandom2By (ArrRandomT v arr)
        = arr (ArrowState2By arr) (Generater2By arr (Arrow2State arr))


