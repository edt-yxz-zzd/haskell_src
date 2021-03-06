{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module ADT.IArrowCatch
    ( IArrowCatch(..), ArrowExceptionType(..)
    , throwAtA, catchWithInputA, catchWithoutInputA
    , IArrowCatchZero(..), IArrowCatchZeroBase(..)
    , IArrowCatchExit(..), IArrowCatchExitBase(..)
    , IArrowCatchBy(..)
    , IArrowThrowBy(..)
    , IArrowThrowBaseBy(..)
    , OnException_Arrow
    , OnZero_Arrow
    , OnExit_Arrow
    {-
    , HandlerX(..)
    , HandlerA(..)
    , firstHandleA
    , mkHandlerA
    , handlers2HandlerA
    , tryHandlerA
    , tryHandlersA
    , catchA2catchesA
    -}
    )
where

import Control.Exception
import Control.Arrow
import Seed.ArrowOps (withInput, constA)
import Seed.EitherOps (eEr_i_to_eiEr)
import Control.Category
import Prelude hiding ((.), id)
import Seed.ProxyOps (withBy, withBy_, Proxy(..))

import Data.Semigroup
import Seed.Boxed
import Seed.MaybeOps (firstJust)
import Data.Foldable
import ADT.IArrowExit

{-
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Except
-}

data OnException_Arrow
data OnZero_Arrow
data OnExit_Arrow


class Arrow arr => IArrowThrowBaseBy by arr where
    type ArrowExceptionTypeBy by arr
    type ArrowExceptionTypeBy by arr = SomeException
    -- why not merge into IArrowCatchBy?
    --  to support GeneralizedNewtypeDeriving
    --  we split the associate type: ArrowExceptionType
class IArrowThrowBaseBy by arr => IArrowThrowBy by arr where
    throwABy :: (ArrowExceptionTypeBy by arr ~ e)
        => proxy by -> arr e y
    throwABy_ :: (ArrowExceptionTypeBy by arr ~ e, (?by :: proxy by))
        => arr e y
    throwABy_ = withBy_ throwABy
    throwABy = withBy throwABy_
    {-# MINIMAL (throwABy | throwABy_) #-}
class IArrowThrowBy by arr => IArrowCatchBy by arr where
    catchABy :: (ArrowExceptionTypeBy by arr ~ e)
        => proxy by -> arr i o -> arr i (Either e o)
    catchABy_ :: (ArrowExceptionTypeBy by arr ~ e, (?by :: proxy by))
        => arr i o -> arr i (Either e o)
    catchABy_ = withBy_ catchABy
    catchABy = withBy catchABy_
    {-# MINIMAL (catchABy | catchABy_) #-}
    -- {-# MINIMAL ((throwABy | throwABy_), (catchABy | catchABy_)) #-}


-------------- IArrowCatch
type ArrowExceptionType arr = ArrowExceptionTypeBy OnException_Arrow arr
instance IArrowCatchBy OnException_Arrow arr => IArrowCatch arr where
class IArrowCatchBy OnException_Arrow arr => IArrowCatch arr where
    -- zeroArrow means no arrow at all ==>> no exception
    -- catchA zeroArrow === zeroArrow
    -- catchA (a <+> b) === catchA a <+> catchA b
    throwA :: (ArrowExceptionType arr ~ e) => arr e y
    catchA :: (ArrowExceptionType arr ~ e) => arr i o -> arr i (Either e o)
    throwA = throwABy (Proxy :: Proxy OnException_Arrow)
    catchA = catchABy (Proxy :: Proxy OnException_Arrow)
    {-
        old_throwA_ v = constA v >>> throwA
        old_catchA_ arr h = catchA arr >>> (h ||| id)
        old_catchA arr h = (catchA arr &&& id) >>> arr eEo_i2eiEo >>> (h ||| id)
        old_catchA_ex = catchA
    -}
throwAtA :: (ArrowExceptionType arr ~ e, IArrowCatch arr) => e -> arr x y
catchWithInputA
    :: (ArrowExceptionType arr ~ e, IArrowCatch arr, ArrowChoice arr)
    => arr i o -> arr (e,i) o -> arr i o
catchWithoutInputA
    :: (ArrowExceptionType arr ~ e, IArrowCatch arr, ArrowChoice arr)
    => arr i o -> arr e o -> arr i o
throwAtA e = constA e >>> throwA
catchWithoutInputA a h = catchA a >>> (h ||| id)
catchWithInputA a h =
    (catchA a &&& id) >>> arr eEo_i2eiEo >>> (h ||| id) where
    eEo_i2eiEo (Left e, i) = Left (e,i)
    eEo_i2eiEo (Right o, _) = Right o


------------- IArrowCatchZero
class (ArrowZero arr) => IArrowCatchZeroBase arr where
    catchZeroA :: arr i o -> arr i (Either () o)
    throwZeroA :: arr () y
    throwZeroA = zeroArrow
instance (ArrowZero arr) => IArrowThrowBaseBy OnZero_Arrow arr where
    type ArrowExceptionTypeBy OnZero_Arrow arr = ()
instance (ArrowZero arr) => IArrowThrowBy OnZero_Arrow arr where
    throwABy_ = zeroArrow
    --throwABy_ = throwZeroA
instance (IArrowCatchZeroBase arr) => IArrowCatchBy OnZero_Arrow arr where
    catchABy_ = catchZeroA
class (IArrowCatchZeroBase arr, IArrowCatchBy OnZero_Arrow arr)
    => IArrowCatchZero arr
instance (IArrowCatchZeroBase arr, IArrowCatchBy OnZero_Arrow arr)
    => IArrowCatchZero arr


------------- IArrowCatchExit
class (IArrowExit arr) => IArrowCatchExitBase arr where
    throwExitA :: (r ~ ArrowExit_Result arr) => arr r y
    throwExitA = exitA
    catchExitA :: (r ~ ArrowExit_Result arr) => arr i o -> arr i (Either r o)
instance (IArrowExit arr) => IArrowThrowBaseBy OnExit_Arrow arr where
    type ArrowExceptionTypeBy OnExit_Arrow arr = (ArrowExit_Result arr)
instance (IArrowExit arr) => IArrowThrowBy OnExit_Arrow arr where
    --throwABy_ = throwExitA
    throwABy_ = exitA
instance (IArrowCatchExitBase arr) => IArrowCatchBy OnExit_Arrow arr where
    catchABy_ = catchExitA
class (IArrowCatchExitBase arr, IArrowCatchBy OnExit_Arrow arr)
    => IArrowCatchExit arr
instance (IArrowCatchExitBase arr, IArrowCatchBy OnExit_Arrow arr)
    => IArrowCatchExit arr




{-
    throwA_
        :: (ArrowExceptionType arr ~ e)
        => e -> arr x y
    throwA_ e = constA e >>> throwA
    catchA, (<+/)
        :: (ArrowExceptionType arr ~ e)
        => arr i o -> arr (e, i) o -> arr i o
    catchA_
        :: (ArrowExceptionType arr ~ e)
        => arr i o -> arr e o -> arr i o
    (<+/) = catchA
    catchesA
        :: (ArrowExceptionType arr ~ e)
        => arr i o -> [HandlerA arr (e, i) o] -> arr i o
    catchesA = catchA2catchesA catchA

    -----
    default catchA
        :: (ArrowExceptionType arr ~ e, ArrowChoice arr)
        => arr i o -> arr (e, i) o -> arr i o
    default catchA_
        :: (ArrowExceptionType arr ~ e, ArrowChoice arr)
        => arr i o -> arr e o -> arr i o
    catchA i2rR ei2rR' = i2rR' where
        i2eErR' = catchA_ex i2rR
        i2eiErR' = (i2eErR' &&& id) >>^ eEr_i_to_eiEr
        i2rR' = i2eiErR' >>> (ei2rR' ||| id)
    catchA_ i2rR e2rR' = i2rR' where
        i2eErR' = catchA_ex i2rR
        i2rR' = i2eErR' >>> (e2rR' ||| id)
infixl 5 <+/, <+/^, <+/+, <+/+^
infixr 5 \+>
(\+>)
    :: (IArrowCatch arr, ArrowExceptionType arr ~ e)
    => arr (e, i) o -> arr i o -> arr i o
(\+>) = flip (<+/)
(<+/^)
    :: (IArrowCatch arr, ArrowExceptionType arr ~ e)
    => arr i o -> ((e, i)->o) -> arr i o
a <+/^ f = a <+/ arr f
(<+/+)
    :: (IArrowCatch arr, ArrowExceptionType arr ~ e)
    => arr i o -> arr i o -> arr i o
a <+/+ b = a <+/ (arr snd >>> b)
(<+/+^)
    :: (IArrowCatch arr, ArrowExceptionType arr ~ e)
    => arr i o -> (i->o) -> arr i o
a <+/+^ f = a <+/+ arr f
-}




-----------------------------------


-----------------------------------

{-
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
    e2ma_ma = e2ma &&& e2ma'     !!!!! TODO: use ArrowChoice
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
    TODO: rewrite
    {-
    ei2o_to_i2o = catchA i2o
    ei2o' = arr snd >>> i2o
    ei2eio = returnA &&& ei2o'
    eio2o = tryHandlersA hs
    ei2o = ei2eio >>> eio2o
    i2o' = ei2o_to_i2o ei2o
    -}


--}
--}
--}
--}
