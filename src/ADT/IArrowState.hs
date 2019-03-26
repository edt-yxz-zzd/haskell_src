{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
-}

{-
take care:
    not "Proxy ()"
    but "Proxy :: Proxy ()"
-}
module ADT.IArrowState
where

import Control.Arrow
import Data.Proxy
import Seed.ArrowOps (withInput)
import Seed.Types (Transform)
{-
import Control.Category
import Prelude hiding ((.), id)
import Seed.IArrowOps (withInput, ab_c2ac_bA, ab2bbA, ab2_aA)
import Seed.Boxed
import Seed.Kind
import Seed.NamedTuple
-}

class IArrowStateBy (ArrowState2By arr) arr
    => IArrowStateDefaultBy arr where
    type ArrowState2By arr
    type ArrowState2By arr = ()
type Arrow2State arr = Arrow2StateBy (ArrowState2By arr) arr
class IArrowStateDefaultBy arr => IArrowState arr where
    set_stateA :: arr (Arrow2State arr) ()
    get_stateA :: arr i (Arrow2State arr)
    set_stateA_ :: arr (Arrow2State arr, i) i
    get_stateA_ :: arr i (Arrow2State arr, i)
instance (by ~ ArrowState2By arr, IArrowStateDefaultBy arr)
    => IArrowState arr where
    set_stateA = set_stateByA (Proxy :: Proxy by)
    get_stateA = get_stateByA (Proxy :: Proxy by)
    set_stateA_ = set_stateByA_ (Proxy :: Proxy by)
    get_stateA_ = get_stateByA_ (Proxy :: Proxy by)


class Arrow arr => IArrowStateBy by arr where
    type Arrow2StateBy by arr
    set_stateByA :: proxy by -> arr (Arrow2StateBy by arr) ()
    get_stateByA :: proxy by -> arr i (Arrow2StateBy by arr)
    fmap_stateByA
        :: proxy by -> Transform (Arrow2StateBy by arr) -> arr i i
    {-
    fmap_stateByA p f = (>>> arr snd) . withInput $
                get_stateByA p >>> arr f >>> set_stateByA p
    -}
    fmap_stateByA p f =
        get_stateByA_ p >>> first (arr f) >>> set_stateByA_ p

    fmap_stateByA_
        :: proxy by -> Transform (Arrow2StateBy by arr)
        -> arr i (Arrow2StateBy by arr)
    set_stateByA_ :: proxy by -> arr (Arrow2StateBy by arr, i) i
    get_stateByA_ :: proxy by -> arr i (Arrow2StateBy by arr, i)
    set_stateByA_ p = first (set_stateByA p) >>> arr snd
    get_stateByA_ = withInput . get_stateByA
    fmap_stateByA_ p f = fmap_stateByA p f >>> get_stateByA p
    {-# MINIMAL (get_stateByA, set_stateByA) #-}
class (IArrowStateBy by arr, a ~ Arrow2StateBy by arr)
    => IArrowStateByEx by arr a | by arr -> a
instance (IArrowStateBy by arr, a ~ Arrow2StateBy by arr)
    => IArrowStateByEx by arr a

class (IArrowState arr, a ~ Arrow2State arr)
    => IArrowStateEx arr a | arr -> a
instance (IArrowState arr, a ~ Arrow2State arr)
    => IArrowStateEx arr a



