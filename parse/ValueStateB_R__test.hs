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

module ValueStateB_R__test where

import ValueStateB_R
import Boxed
import ValueStateB_R__test_head

{-
data D = D {size::Int, time::Int} deriving (Show, Eq, Ord)

data SizeF
instance VStatePB (VPropertyBox D SizeF) Int
instance VStatePR (VPropertyBox D SizeF) Int where
    vget = size . unbox
-}


d = D {size = 100, time = -333}



--toSelfP :: s -> VPropertyBox s SelfF s
--toSelfP = box
toSelfP = as_vself
sd = vget . toSelfP $ d



-- toSizeP :: VPropertyBox s p u -> VPropertyBox s (Child SizeF p) v
toSizeP :: VGetAttr SizeF
toSizeP = vcase


-- self :: VPropertyBox D SelfF D
self = toSelfP d
-- ss :: VPropertyBox D (Child SelfF SizeF) v
ss = toSizeP self
sv = vget $ (vcase :: VGetAttr SizeF) self -- vget ss
self' = to_vparent ss -- == self















-- NOTE: p is unknown ==>> {p:v} if without p->v
toSizeV :: (VStateR (VPropertyBox s (Child SizeF p) v) v)
         => VPropertyBox s (Child SizeF p) v -> v
toSizeV = vgetb



{-

fixed bug: forgot class VState ... | p->v where vget :: p v -> v

toSizeB = vcase -- resp to SelfF
-- d :: D
-- self :: VPropertyBox D SelfF D
self = toSelfP d
-- ss :: VPropertyBox D (Child SizeF SelfF) v
ss = toSizeP $ self
ss' = toSizeB self
    No instances for (VStateB (VPropertyBox D SizeF) v0,
                      VStateB (VPropertyBox v SizeF) v0,
                      VStateB (VPropertyBox D SelfF) v)
      arising from a use of `toSizeB'
    In the expression: toSizeB self
    In an equation for ss': ss' = toSizeB self


why cannot deduce v = D and v0 = Int
    instance ... s SelfF) s)
    instance ... D SizeF) Int)
    class VStateB p v -- NO p->v !!!!!!!!!!!!!!
-}
--}
--}
--}

