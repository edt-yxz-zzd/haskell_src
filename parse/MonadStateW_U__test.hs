{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , DefaultSignatures
            , Rank2Types
            , KindSignatures
            , GeneralizedNewtypeDeriving
            , ScopedTypeVariables #-}

module MonadStateW_U__test where

import ValueStateB_R
import ValueStateB_R__test_head
import MonadStateB_R
--import MonadStateB_R__test
import ValueStateW_U
import ValueStateW_U__test_head
import MonadStateW_U

import Boxed
import WrapMonadState
import qualified Control.Monad.State as M


d = D{size = 222, time = -111}

s6 = g $ unbox $ (toSizeM mget_self >> mset 444 >> mget)
s7 = g $ unbox $ (toSizeM mget_self >> mset 444 >> mupdate (+3) >> mget)



---------------

type Mo = WrapMonadState
type MoD = Mo D
toSizeM :: MGetAttr SizeF
toSizeM = mcase



mget_self :: MPropertyR (MPropertyBox m) SelfF v => MPropertyBox m SelfF v v
mget_self = mget
unbox_mself :: MStateB m s => MPropertyBox m SelfF s a -> m a
unbox_mself = unbox
s1 = M.runState (unWrapMonadState mgetb) d
g m = M.runState (unWrapMonadState $ m) d
s2 = g $ unbox_mself mget
s3 = g $ unbox mget_self
s4 = g $ unbox $ toSizeM mget_self
s5 = g $ unbox $ (toSizeM mget_self >> mget)

--}
--}
--}

