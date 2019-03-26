{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}


{-
take care:
    not "Proxy ()"
    but "Proxy :: Proxy ()"
-}
module Arrows.ArrStateT
where

import ADT.IArrowState
import Seed.ArrowOps (withInput, ab_c2ac_bA, ab2bbA, ab2_aA)
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Seed.Boxed
import Data.Proxy
import Seed.Kind
import Seed.NamedTuple


---
data ArrStateT (by :: *) st arr i o
    = ArrStateT { runArrStateT :: arr (i, st) (o, st) }
instance Boxed (ArrStateT by st arr i o) where
    type BoxedFrom (ArrStateT by st arr i o) = arr (i, st) (o, st)
    box = ArrStateT
    unbox = runArrStateT
instance Category arr => Category (ArrStateT by st arr) where
    (.) = opmapbGBox (.)
    id = box id
instance Arrow arr => Arrow (ArrStateT by st arr) where
    arr = box . first . arr
    first = mapGBox $ (\a -> ab_c2ac_bA >>> a >>> ab_c2ac_bA) . first
instance Arrow arr => IArrowStateBy () (ArrStateT by st arr) where
    type Arrow2StateBy () (ArrStateT by st arr) = st
    get_stateByA _ = box ab2bbA
    set_stateByA _ = box ab2_aA
instance (Arrow arr, IArrowStateBy by (ArrStateT by st arr))
    => IArrowStateDefaultBy (ArrStateT by st arr) where
    type ArrowState2By (ArrStateT by st arr) = by





--newtype ArrStatesT by (attrs :: [Dict k *]) arr i o =
newtype ArrStatesT by attrs arr i o =
    ArrStatesT { runArrStatesT :: ArrStateT by (NamedTuple attrs) arr i o }
instance Boxed (ArrStatesT by attrs arr i o) where
    type BoxedFrom (ArrStatesT by attrs arr i o)
        = ArrStateT by (NamedTuple attrs) arr i o
    box = ArrStatesT
    unbox = runArrStatesT
instance Category arr => Category (ArrStatesT by attrs arr) where
    (.) = opmapbGBox (.)
    id = box id
instance Arrow arr => Arrow (ArrStatesT by attrs arr) where
    arr = box . arr
    first = mapGBox first
--data Attr (key :: k)
data Attr key
instance (Arrow arr, AccessAttr attrs key)
    => IArrowStateBy (Attr key) (ArrStatesT by attrs arr) where
    type Arrow2StateBy (Attr key) (ArrStatesT by attrs arr)
        = LookupE key attrs
    get_stateByA by = box $ a >>> arr namedtuple2val where
        a :: ArrStateT by (NamedTuple attrs) arr i (NamedTuple attrs)
        a = get_stateByA by'
        namedtuple2val :: NamedTuple attrs -> LookupE key attrs
        namedtuple2val = getattr p'
        by' = Proxy :: Proxy ()
        p' = _get_stateByA__proxy_cast by
    fmap_stateByA by f = box $ fmap_stateByA by' namedtupleT where
        namedtupleT = fmapattr p' f
        by' = Proxy :: Proxy ()
        p' = _get_stateByA__proxy_cast by
    set_stateByA by = box $
        get_stateByA_ by' >>> arr _STst2ST >>> set_stateByA by' where
        _STst2ST (_ST, st) = setattr p' st _ST
        by' = Proxy :: Proxy ()
        p' = _get_stateByA__proxy_cast by
_get_stateByA__proxy_cast :: proxy (Attr key) -> Proxy key
_get_stateByA__proxy_cast _ = Proxy

instance (Arrow arr, IArrowStateBy by (ArrStatesT by st arr))
    => IArrowStateDefaultBy (ArrStatesT by st arr) where
    type ArrowState2By (ArrStatesT by st arr) = by


