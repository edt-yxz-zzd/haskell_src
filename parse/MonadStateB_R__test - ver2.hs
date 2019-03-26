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

module MonadStateB_R__test where

import ValueStateB_R
import ValueStateB_R__test
import MonadStateB_R
import Boxed
import WrapMonadState
import qualified Control.Monad.State as M


type Mo = WrapMonadState
type MoD = Mo D

mget_self :: MPropertyR (MPropertyBox m) SelfF v => MPropertyBox m SelfF v v
mget_self = mget
unbox_mself :: MStateB m s => MPropertyBox m SelfF s a -> m a
unbox_mself = unbox
s1 = M.runState (unWrapMonadState mgetb) d
g m = M.runState (unWrapMonadState $ m) d
s2 = g $ unbox_mself mget
s3 = g $ unbox mget_self
s4 = g $ unbox $ toSizeM mget_self
s5 = g $ unbox $ (toSizeM mget_self >> mget) -- dead loop!!! why???

main = print s5
--toSizeM :: MGetAttr SizeF
toSizeM :: (MPropertyB (MPropertyBox s) (Child p SizeF) v
                ,MPropertyB (MPropertyBox s) p u)
                => MPropertyBox s p u a -> MPropertyBox s (Child p SizeF) v a
toSizeM = mcase

type MXX = MoD (M.State D)
-- R MXX D
-- PR (B MXX SelfF) D
xx :: MPropertyBox MXX SelfF D D
xx = mget_self

-- yy :: MPropertyBox MXX (Child SelfF SizeF) Int D
yy = toSizeM xx
zz :: MPropertyBox MXX (Child SelfF SizeF) Int Int
zz = mget -- seq zz () -- dead!!!
dead = seq zz ()

aa :: MPropertyBox MXX SelfF D D
aa = mget
bb :: MPropertyBox MXX SelfF D Int
bb = aa >>= 
        return . vget . (vcase :: VGetAttr SizeF) . as_vself
zz' :: MPropertyBox MXX (Child SelfF SizeF) Int Int
zz' = mcase bb -- seq zz' () -- well!!!
well = seq zz' ()

{-
--toSelfP :: s -> VPropertyBox s SelfF s
--toSelfP = box
toSelfM = as_mself
sd = mget . toSelfM $ d



-- toSizeP :: VPropertyBox s p u -> VPropertyBox s (Child SizeF p) v


-- self :: VPropertyBox D SelfF D
self = toSelfP d
-- ss :: VPropertyBox D (Child SelfF SizeF) v
ss = toSizeP self
sv = vget $ (vcase :: VGetAttr SizeF) self -- vget ss
self' = to_parent ss -- == self


















--}
--}
--}

