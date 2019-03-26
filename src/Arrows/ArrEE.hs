{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Arrows.ArrEE
where
import ADT.IArrowEE
import ADT.IArrowExit
import Arrows.ArrCPS
import Seed.Boxed
import Seed.ArrowOps (mk_app)
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)


newtype ArrEE arr e r i o = ArrEE {unArrEE :: ArrCPS arr (Either e r) i o}
instance Boxed (ArrEE arr e r i o) where
    type BoxedFrom (ArrEE arr e r i o) = ArrCPS arr (Either e r) i o
    box = ArrEE
    unbox = unArrEE

instance Category arr => Category (ArrEE arr e r) where
    id = box id
    (.) = opmapbGBox (.)
instance ArrowApply arr => Arrow (ArrEE arr e r) where
    arr = box . arr
    first = mapGBox first
    (***) = opmapbGBox (***)
instance (ArrowChoice arr, ArrowApply arr)
    => ArrowChoice (ArrEE arr e r) where
    left = mapGBox left
    (+++) = opmapbGBox (+++)
instance ArrowApply arr => ArrowApply (ArrEE arr e r) where
    app = mk_app box unbox


instance (ArrowApply arr, ArrowZero arr) => ArrowZero (ArrEE arr e r) where
    zeroArrow = box zeroArrow
instance (ArrowApply arr, ArrowPlus arr) => ArrowPlus (ArrEE arr e r) where
    (<+>) = opmapbGBox (<+>)



----------------------

instance (ArrowApply arr, ArrowChoice arr) => IArrowChoiceEE (ArrEE arr) where
instance (ArrowApply arr, ArrowChoice arr) => IArrowEE (ArrEE arr) where
    type ToArrow (ArrEE arr) e r = ArrCPS arr (Either e r)
    to_arr = unbox
    from_arr = box
    quitEE = box exitCC
    catchEE_ex i2eErR = i2eErR' where
        i2eEr = unArrCPS $ unbox i2eErR
        i2eErR' = box $ mkArrCPS i2eEr


