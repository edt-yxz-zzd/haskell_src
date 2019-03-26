{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module ADT.ArrowError
where

import Control.Exception
import Control.Arrow
import Control.Monad
import Data.Semigroup
import Data.Maybe
import Data.Foldable
import Control.Monad.Trans.Except
import ADT.ArrowCC
import Seed.Boxed
import Seed.MaybeOps
import Seed.ArrowOps (withInput, constA)




class Arrow arr => ArrowError arr where
    type ExceptionType arr
    throwA :: ExceptionType arr -> arr i o
    catchA :: arr i o -> arr (ExceptionType arr, i) o -> arr i o
    catchesA :: arr i o -> [HandlerA arr (ExceptionType arr, i) o] -> arr i o
    catchesA = catchA2catchesA catchA
    {-# MINIMAL (throwA, catchA) #-}





-----------------------------------
type ErrorA = ArrErrCC
instance Arrow arr => ArrowError (ErrorA arr err) where
    type ExceptionType (ErrorA arr err) = err
    throwA = exitCC
    catchA e_i2o e_ei2o = case unbox $ unbox e_i2o of
        Left i2e -> let i2ei = withInput i2e
                        e_i2ei = arrCC_ex i2ei
                        e_i2o' = e_i2ei >>> e_ei2o
                    in  e_i2o'
        _ -> e_i2o



-----------------------------------

type HandlerX arr i o = arr i (Maybe o)
data HandlerA arr i o =
    HandlerA { runHandlerA :: HandlerX arr i o }

instance Boxed (HandlerA arr i o) where
    type BoxedFrom (HandlerA arr i o) = HandlerX arr i o
    box = HandlerA
    unbox = runHandlerA

firstHandleA
    :: Arrow arr => HandlerA arr e a -> HandlerA arr e a -> HandlerA arr e a
firstHandleA = opmapbBox i2ma_i2ma_to_i2ma__first


i2ma_i2ma_to_i2ma__first
    :: (Arrow arr, HandlerX arr i o ~ i2ma) => i2ma -> i2ma -> i2ma
i2ma_i2ma_to_i2ma__first e2ma e2ma' = e2ma'' where
    e2ma_ma = e2ma &&& e2ma'
    ma_ma2ma = uncurry firstJust
    e2ma'' = e2ma_ma >>> arr ma_ma2ma



instance Arrow arr => Semigroup (HandlerA arr err a) where
    (<>) = firstHandleA
instance Arrow arr => Monoid (HandlerA arr err a) where
    mappend = (<>)
    mempty = box $ constA Nothing

mkHandlerA :: Arrow arr => arr e a -> HandlerA arr e a
mkHandlerA e2a = box $ e2a >>> arr Just
handlers2HandlerA :: Arrow arr => [HandlerA arr e a] -> HandlerA arr e a
handlers2HandlerA = fold

tryHandlerA :: Arrow arr => HandlerA arr e a -> arr (e,a) a
tryHandlerA (HandlerA e2ma) = ea2a where
    ea2e = arr fst
    ea2ma = ea2e >>> e2ma
    ea2ma_ea = withInput ea2ma
    ma_ea2a = arr _ma_ea2a where
        _ma_ea2a (Just a, _) = a
        _ma_ea2a (_, (e,a)) = a
    ea2a = ea2ma_ea >>> ma_ea2a
tryHandlersA :: Arrow arr => [HandlerA arr e a] -> arr (e,a) a
tryHandlersA = tryHandlerA . handlers2HandlerA



catchA2catchesA
    :: Arrow arr => (arr i o -> arr (e,i) o -> arr i o)
    -> (arr i o -> [HandlerA arr (e,i) o] -> arr i o)
catchA2catchesA catchA i2o hs = i2o' where
    ei2o_to_i2o = catchA i2o
    ei2o' = arr snd >>> i2o
    ei2eio = returnA &&& ei2o'
    eio2o = tryHandlersA hs
    ei2o = ei2eio >>> eio2o
    i2o' = ei2o_to_i2o ei2o



