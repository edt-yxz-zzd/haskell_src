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
import ValueStateW_U__test_head


{-
instance VStatePB (VPropertyBox DD SizeF) Int
instance VStatePR (VPropertyBox DD SizeF) Int where
    vget = size2 . unbox
instance VStatePW (VPropertyBox DD SizeF) Int where
    vset s d = liftB (\d -> d {size2=s}) d -- box $ (unbox d) {size=s}
instance VStatePU (VPropertyBox DD SizeF) Int

instance VStatePB (VPropertyBox DD DF) D
instance VStatePR (VPropertyBox DD DF) D where
    vget = getD . unbox
instance VStatePW (VPropertyBox DD DF) D where
    vset s d = liftB (\d -> d {getD=s}) d
instance VStatePU (VPropertyBox DD DF) D

data DF

data SizeF
instance VStatePB (VPropertyBox D SizeF) Int
instance VStatePR (VPropertyBox D SizeF) Int where
    vget = size . unbox
instance VStatePW (VPropertyBox D SizeF) Int where
    vset s d = liftB (\d -> d {size=s}) d -- box $ (unbox d) {size=s}
instance VStatePU (VPropertyBox D SizeF) Int


data DD = DD {getD::D, size2::Int, time2::Int} deriving (Show, Eq, Ord)
data D = D {size::Int, time::Int} deriving (Show, Eq, Ord)
-}






d = D {size = 100, time = -333}
dd = DD {getD = d, size2=777, time2= -999}
-- ddself :: VPropertyBox DD SelfF DD
ddself = as_vself dd


-- toSizeP :: VPropertyBox s p u -> VPropertyBox s (Child SizeF p) v
-- fail : toSizeP :: VGetAttr SizeF = vcase
-- fail : toSizeP = vcase :: VGetAttr SizeF
--     when call on diff types : toSizeP ddself; toSizeP dself
toSizeP :: VGetAttr SizeF
toSizeP = vcase
-- ddsize :: VPropertyBox DD (Child SelfF SizeF) Int
ddsize = toSizeP ddself
ddsize' = vset 0 ddsize
dds'' = vupdate (flip(-)1) ddsize'

toDP = vcaseb :: VGetAttr DF
dd_d = toDP ddself
dd_dsize = toSizeP dd_d
dd_dsize' = vset 343436 dd_dsize
--}












--toSelfP :: s -> VPropertyBox s SelfF s
--toSelfP = box
toSelfP = as_vself
sd = vget . toSelfP $ d





-- self :: VPropertyBox D SelfF D
self = toSelfP d
-- ss :: VPropertyBox D (Child SizeF SelfF) v
ss = toSizeP self
sv = vget ss
self' = to_vparent ss -- == self

ss' = vset 3 ss
ss'' = vupdate (+1) ss'
--}











-- NOTE: p is unknown ==>> {p:v} if without p->v
toSizeV :: (VStateR (VPropertyBox s (Child SizeF p) v) v)
         => VPropertyBox s (Child SizeF p) v -> v
toSizeV = vgetb


--}

