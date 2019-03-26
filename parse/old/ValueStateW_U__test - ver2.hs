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

module ValueStateW_U__test where

import ValueStateB_R
import ValueStateW_U
import Boxed



instance VStateB (VPropertyBox DD SizeF) Int
instance VStateR (VPropertyBox DD SizeF) Int where
    vget = size2 . unbox
instance VStateW (VPropertyBox DD SizeF) Int where
    vset s d = liftB (\d -> d {size2=s}) d -- box $ (unbox d) {size=s}
instance VStateU (VPropertyBox DD SizeF) Int

instance VStateB (VPropertyBox DD DF) D
instance VStateR (VPropertyBox DD DF) D where
    vget = getD . unbox
instance VStateW (VPropertyBox DD DF) D where
    vset s d = liftB (\d -> d {getD=s}) d
instance VStateU (VPropertyBox DD DF) D

data DF

dd = DD {getD = d, size2=777, time2= -999}
ddself = to_self dd
ddsize = toSizeP ddself
ddsize' = vset 0 ddsize
dds'' = vupdate (flip(-)1) ddsize'

toDP = vcaseb :: AsAttr DF
dd_d = toDP ddself
dd_dsize = toSizeP dd_d
dd_dsize' = vset 343436 dd_dsize











data D = D {size::Int, time::Int} deriving (Show, Eq, Ord)
data DD = DD {getD::D, size2::Int, time2::Int} deriving (Show, Eq, Ord)

d = D {size = 100, time = -333}

data SizeF
instance VStateB (VPropertyBox D SizeF) Int
instance VStateR (VPropertyBox D SizeF) Int where
    vget = size . unbox
instance VStateW (VPropertyBox D SizeF) Int where
    vset s d = liftB (\d -> d {size=s}) d -- box $ (unbox d) {size=s}
instance VStateU (VPropertyBox D SizeF) Int



--toSelfP :: s -> VPropertyBox s SelfF s
--toSelfP = box
toSelfP = to_self
sd = vget . toSelfP $ d



-- toSizeP :: VPropertyBox s p u -> VPropertyBox s (Child SizeF p) v
toSizeP = vcaseb :: AsAttr SizeF


-- self :: VPropertyBox D SelfF D
self = toSelfP d
-- ss :: VPropertyBox D (Child SizeF SelfF) v
ss = toSizeP self
sv = vget ss
self' = to_parent ss -- == self

ss' = vset 3 ss
ss'' = vupdate (+1) ss'












-- NOTE: p is unknown ==>> {p:v} if without p->v
toSizeV :: (VStateR (VPropertyBox s (Child SizeF p)) v)
         => VPropertyBox s (Child SizeF p) v -> v
toSizeV = vget



