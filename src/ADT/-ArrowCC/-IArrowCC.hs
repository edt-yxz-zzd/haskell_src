{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


module ADT.IArrowCC
where

--import Control.Monad.Trans.Cont
import Control.Arrow
import Seed.ArrowOps (constA, mk_app, eEr_i_to_eiEr)
{-
class Arrow arr => IArrowCC arr where
    callCC :: ((r -> arr x y) -> arr i r) -> arr i r
    exitCC :: r -> arr x y -- error!
-}

{-
class Arrow arr => IArrowCC arr where
    type ArrowCC_Result arr
    exitCC
        :: (ArrowCC_Result arr ~ r) => arr i r -> arr i y
    exit :: (ArrowCC_Result arr ~ r) => r -> arr x y
    exit = exitCC . constA
    callCC
        :: (ArrowCC_Result arr ~ r)
        => ((r -> arr x y) -> arr i r) -> arr i r
    callCC f = f exit



-}


class Arrow arr => IArrowCC arr where
    type ArrowCC_Result arr
    exitCC
        :: (ArrowCC_Result arr ~ r) => arr r y
    callCC
        :: (ArrowCC_Result arr ~ r)
        => (arr r y -> arr i r) -> arr i r
    callCC f = f exitCC

-- catchCC :: arr (Either e r) i r -> arr (Either e r') (e,i) r -> arr (Either e r') i r
class IArrowEE arr where
    -- Either Exit; 2 channels
    -- TODO:
    --  using NamedTuple to encode (e,r,...)
    --  into [Result :> r, Error :> e, ...]
    --  n-exit-channels
    type ToArrow arr e r :: * -> * -> *
    to_arr :: Arrow (ToArrow arr e r) => arr e r i o -> ToArrow arr e r i o
    from_arr :: Arrow (ToArrow arr e r) => ToArrow arr e r i o -> arr e r i o
    -- Arrow
    idEE :: (ar ~ arr e r) => ar i i
    arrEE :: (ar ~ arr e r) => (i->o) -> ar i o
    (>>>>) :: (ar ~ arr e r) => ar i x -> ar x o -> ar i o
    (****) :: (ar ~ arr e r) => ar a a' -> ar x x' -> ar (a, x) (a', x')
    (&&&&) :: (ar ~ arr e r) => ar i a' -> ar i x' -> ar i (a', x')


    -- quit
    quitEE_ :: (ar ~ arr e r) => ar i (Either e r) -> ar i y

    quitEE :: (ar ~ arr e r) => (Either e r) -> ar i y
    exitEE_ :: (ar ~ arr e r) => ar i r -> ar i y
    exitEE :: (ar ~ arr e r) => r -> ar x y
    throwEE_ :: (ar ~ arr e r) => ar i e -> ar i y
    throwEE :: (ar ~ arr e r) => e -> ar x y

    -- catch
    catchEE_ex
        :: (ar ~ arr e r)
        => ar i (Either e r) -> arr e' r' i (Either e r)
    unRightEE
        :: (ar ~ arr e r)
        => ar i (Either e o) -> arr e r i o

    catchEE_ :: (ar' ~ arr e r') => arr e r i r -> ar' (e,i) r -> ar' i r
    catchEE :: (ar' ~ arr e r') => arr e r i r -> ar' e r -> ar' i r
    default catchEE
        :: (ar' ~ arr e r', IArrowChoiceEE arr)
        => arr e r i r -> ar' e r -> ar' i r
    default catchEE_
        :: (ar' ~ arr e r', IArrowChoiceEE arr)
        => arr e r i r -> ar' (e,i) r -> ar' i r
    catchEE i2rR e2rR' = i2rR' where
        i2eErR' = catchEE_ex $ i2rR >>>> arrEE Right
        i2rR' = i2eErR' >>>> (e2rR' |||| idEE)
    catchEE_ i2rR ei2rR' = i2rR' where
        i2eErR' = catchEE_ex $ i2rR >>>> arrEE Right
        i2eiErR' = (i2eErR' &&&& idEE) >>>> arrEE eEr_i_to_eiEr
        i2rR' = i2eiErR' >>>> (ei2rR' |||| idEE)
    relabelR_EE :: (ar ~ arr e r) => ar i r -> arr e r' i r
    relabelE_EE :: (ar ~ arr e r) => ar i e -> arr e' r i e
    -- :: (ar ~ arr e r) => 


    -----------
    default arrEE
        :: (ar ~ arr e r, Arrow (ToArrow arr e r))
        => (i->o) -> ar i o
    default (>>>>)
        :: (ar ~ arr e r, Arrow (ToArrow arr e r))
        => ar i x -> ar x o -> ar i o
    default (****)
        :: (ar ~ arr e r, Arrow (ToArrow arr e r))
        => ar a a' -> ar x x' -> ar (a, x) (a', x')
    default (&&&&)
        :: (ar ~ arr e r, Arrow (ToArrow arr e r))
        => ar i a' -> ar i x' -> ar i (a', x')
    idEE = arrEE id
    arrEE = from_arr . arr
    a >>>> b = from_arr $ to_arr a >>> to_arr b
    a **** b = from_arr $ to_arr a *** to_arr b
    a &&&& b = from_arr $ to_arr a &&& to_arr b


    --------
    quitEE = quitEE_ . constEE
    exitEE_ = quitEE_ . (>>>> arrEE Right)
    exitEE = exitEE_ . constEE
    throwEE_ = quitEE_ . (>>>> arrEE Left)
    throwEE = throwEE_ . constEE
constEE :: IArrowEE arr => a -> arr e r x a
constEE = arrEE . const
infixr 1 >>>>
infixr 3 ****
infixr 3 &&&&
infixr 2 ++++
infixr 2 ||||
infixr 5 <++>


class IArrowEE arr => IArrowChoiceEE arr where
    (++++)
        :: (ar ~ arr e r, ei ~ Either)
        => ar a a' -> ar x x' -> ar (ei a x) (ei a' x')
    (||||)
        :: (ar ~ arr e r, ei ~ Either)
        => ar a o -> ar x o -> ar (ei a x) o
    default (++++)
        :: (ar ~ arr e r, ArrowChoice (ToArrow arr e r), ei ~ Either)
        => ar a a' -> ar x x' -> ar (ei a x) (ei a' x')
    default (||||)
        :: (ar ~ arr e r, ArrowChoice (ToArrow arr e r), ei ~ Either)
        => ar a o -> ar x o -> ar (ei a x) o
    a ++++ b = from_arr $ to_arr a +++ to_arr b
    a |||| b = from_arr $ to_arr a ||| to_arr b
class IArrowEE arr => IArrowApplyEE arr where
    appEE :: (ar ~ arr e r) => ar (ar i o, i) o
    default appEE
        :: (ar ~ arr e r, ArrowApply (ToArrow arr e r))
        => ar (ar i o, i) o
    appEE = mk_app from_arr to_arr
class IArrowEE arr => IArrowZeroEE arr where
    zeroArrowEE :: (ar ~ arr e r) => ar x y
    default zeroArrowEE
        :: (ar ~ arr e r, ArrowZero (ToArrow arr e r)) => ar x y
    zeroArrowEE = from_arr zeroArrow
class IArrowEE arr => IArrowPlusEE arr where
    (<++>) :: (ar ~ arr e r) => ar i o -> ar i o -> ar i o
    default (<++>)
        :: (ar ~ arr e r, ArrowPlus (ToArrow arr e r))
        => ar i o -> ar i o -> ar i o
    a <++> b = from_arr $ to_arr a <+> to_arr b
class IArrowEE arr => IArrowLoopEE arr where
    loopEE :: (ar ~ arr e r) => ar (i,x) (o,x) -> ar i o
    default loopEE
        :: (ar ~ arr e r, ArrowLoop (ToArrow arr e r))
        => ar (i,x) (o,x) -> ar i o
    loopEE = from_arr . loop . to_arr


