{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ADT.IArrowEE
    -- module ADT.IArrowExit
where

--import Control.Monad.Trans.Cont
import Control.Arrow
import Seed.ArrowOps (constA, mk_app)
import Seed.EitherOps (eEr_i_to_eiEr)

--import ADT.IArrowExit
import ADT.IArrowCatch



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
    quitEE :: (ar ~ arr e r) => ar (Either e r) y

    exitEE :: (ar ~ arr e r) => ar r y
    throwEE :: (ar ~ arr e r) => ar e y

    -- catch
    catchEE_ex
        :: (ar ~ arr e r)
        => ar i (Either e r) -> arr e' r' i (Either e r)

    catchEE :: (ar' ~ arr e r') => arr e r i r -> ar' (e,i) r -> ar' i r
    catchEE_ :: (ar' ~ arr e r') => arr e r i r -> ar' e r -> ar' i r
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
    exitEE = arrEE Right >>>> quitEE
    throwEE = arrEE Left >>>> quitEE


    ------
    default relabelR_EE
        :: (ar ~ arr e r, IArrowChoiceEE arr) => ar i r -> arr e r' i r
    default relabelE_EE
        :: (ar ~ arr e r, IArrowChoiceEE arr) => ar i e -> arr e' r i e
    default catchEE_
        :: (ar' ~ arr e r', IArrowChoiceEE arr)
        => arr e r i r -> ar' e r -> ar' i r
    default catchEE
        :: (ar' ~ arr e r', IArrowChoiceEE arr)
        => arr e r i r -> ar' (e,i) r -> ar' i r
    catchEE_ i2rR e2rR' = i2rR' where
        i2eErR' = catchEE_ex $ i2rR >>>> arrEE Right
        i2rR' = i2eErR' >>>> (e2rR' |||| idEE)
    catchEE i2rR ei2rR' = i2rR' where
        i2eErR' = catchEE_ex $ i2rR >>>> arrEE Right
        i2eiErR' = (i2eErR' &&&& idEE) >>>> arrEE eEr_i_to_eiEr
        i2rR' = i2eiErR' >>>> (ei2rR' |||| idEE)
    relabelR_EE i2rR = i2rR' where
        i2eErR' = catchEE_ex $ i2rR >>>> arrEE Right
        i2rR' = i2eErR' >>>> unRightEE
    relabelE_EE i2eR = i2eR' where
        i2eErR' = catchEE_ex $ i2eR >>>> arrEE Left
        i2eR' = i2eErR' >>>> unLeftEE


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
    unRightEE :: (ar ~ arr e r) => ar (Either e o) o
    unRightEE = throwEE |||| idEE
    unLeftEE :: (ar ~ arr e r) => ar (Either x r) x
    unLeftEE = idEE |||| exitEE
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


