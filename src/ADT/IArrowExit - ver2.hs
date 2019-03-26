{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ADT.IArrowExit
where

--import Control.Monad.Trans.Cont
import Control.Arrow
import Seed.ArrowOps (constA) -- , mk_app)
--import Seed.EitherOps (eEr_i_to_eiEr)

class Arrow arr => IArrowExitBase arr where
    type ArrowExit_Result arr
class IArrowExitBase arr => IArrowExit arr where
    exitA :: (ArrowExit_Result arr ~ r) => arr r y
    exitA_ :: (ArrowExit_Result arr ~ r) => r -> arr x y
    default exitA
        :: (IArrowCC arr, ArrowCC_Result arr ~ r, ArrowExit_Result arr ~ r)
        => arr r y
    exitA = exitCC
    exitA_ r = constA r >>> exitA

type ArrowCC_Result arr = ArrowExit_Result arr
class IArrowExit arr => IArrowCC arr where
    exitCC :: (ArrowCC_Result arr ~ r) => arr r y
    exitCC_ :: (ArrowCC_Result arr ~ r) => r -> arr x y
    callCC :: (ArrowCC_Result arr ~ r) => (arr r y -> arr i r) -> arr i r
    callCC f = f exitCC
    exitCC_ r = constA r >>> exitCC


