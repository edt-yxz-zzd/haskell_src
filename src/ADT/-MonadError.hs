
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module ADT.MonadError
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
import Seed.ArrowOps (withInput)


{-
data HandlerE a =
    HandlerE { runHandlerE :: forall e a. Exception e => (e -> a) }
type HandlerE a = SomeException -> Maybe a
-}

data HandlerE err a =
    HandlerE { runHandlerE :: err -> Maybe a }
type HandlerE_ = HandlerE SomeException
instance Semigroup (HandlerE err a) where
    HandlerE e2ma <> HandlerE e2ma' = HandlerE $ \e ->
        e2ma e `firstJust` e2ma' e
instance Monoid (HandlerE err a) where
    mappend = (<>)
    mempty = HandlerE $ const Nothing

mkHandlerE :: (e -> a) -> HandlerE e a
mkHandlerE e2a = HandlerE $ Just . e2a
handlers2HandlerE :: [HandlerE e a] -> HandlerE e a
handlers2HandlerE = fold

tryHandlerE :: e -> HandlerE e a -> a -> a
tryHandlerE e (HandlerE e2ma) a = maybe a id $ e2ma e
appHandlerE :: HandlerE e a -> a -> (e -> a)
appHandlersE :: [HandlerE e a] -> a -> (e -> a)
appHandlerE h a = maybe a id . runHandlerE h
appHandlersE = appHandlerE . handlers2HandlerE
catchesHandlerE :: [HandlerE e a] -> e -> a -> a
catchesHandlerE handlers e = tryHandlerE e (handlers2HandlerE handlers)



catchE2catchesE
    :: (a -> (e -> a) -> a) -> (a -> [HandlerE e a] -> a)
catchE2catchesE catchE a hs = catchE a $ appHandlersE hs a


mkHandlerE_ :: Exception e => (e -> a) -> HandlerE_ a
mkHandlerE_ e2a = HandlerE (fmap e2a . fromException)
toHandler_ :: Exception e => a -> (e -> a) -> (SomeException -> a)
toHandler_ a e2a = maybe a e2a . fromException
catchE2catchE_
    :: Exception e => (a -> (SomeException -> a) -> a) -> (a -> (e->a) -> a)
catchE2catchE_ catchE a e2a = catchE a $ toHandler_ a e2a
throwE2throwE_ :: Exception e => (SomeException -> a) -> (e->a)
throwE2throwE_ throwE = throwE . toException
{-
class Error a where
    throwE_ :: SomeException -> a
    catchE_ :: a -> (SomeException -> a) -> a

    throwE :: Exception e => e -> a
    throwE = throwE_2throwE throwE_
    catchE :: Exception e => a -> (e -> a) -> a
    catchE = catchE_2catchE catchE_
    catchesE :: a -> [HandlerE a] -> a
    catchesE = catchE_2catchesE catchE_
    {-# MINIMAL (throwE_, catchE_) #-}
type HandlerM m a = HandlerE (m a)
type HandlerA arr i o = HandlerE (arr i o)
class Monad m => MonadError m where
    throwM_ :: SomeException -> m a
    catchM_ :: m a -> (SomeException -> m a) -> m a

    throwM :: Exception e => e -> m a
    throwM = throwE_2throwE throwM_
    catchM :: Exception e => m a -> (e -> m a) -> m a
    catchM = catchE_2catchE catchM_
    catchesM :: m a -> [HandlerM m a] -> m a
    catchesM = catchE_2catchesE catchM_

    default throwM_ :: Error (m a) => SomeException -> m a
    throwM_ = throwE_
    default catchM_ :: Error (m a) => m a -> (SomeException -> m a) -> m a
    catchM_ = catchE_

-}

{-
--newtype ErrorT m a = ErrorT { unErrorT :: (Either (m SomeException) (m a)) }
type ErrorT = ExceptT SomeException
mkErrorT = ExceptT
unErrorT (ExceptT mea) = mea
instance Monad m => MonadError (ErrorT m) where
instance Error (ErrorT m a) where
    throwE_ = ErrorT . Left . return
    catchE_ (ErrorT (Left e)) f = f e
    catchE_ ma _ = ma
-}


class Arrow arr => ArrowError arr where
    type ExceptionType arr
    throwA :: ExceptionType arr -> arr i o
    catchA :: arr i o -> arr (ExceptionType arr, i) o -> arr i o

{-
    throwA
        :: (ExceptionType arr ~ SomeException, Exception e) => e -> arr i o
    throwA = throwE_2throwE throwA_
    catchA
        :: (ExceptionType arr ~ SomeException, Exception e)
        => arr i o -> (e -> arr i o) -> arr i o
    catchA = catchE_2catchE catchA_
    catchesA :: arr i o -> [HandlerA arr i o] -> arr i o
    catchesA = catchE_2catchesE catchA_

    default throwA_ :: Error (arr i o) => SomeException -> arr i o
    throwA_ = throwE_
    default catchA_ :: Error (arr i o) => arr i o -> (SomeException -> arr i o) -> arr i o
    catchA_ = catchE_
-}

type ErrorA = ArrErrCC
--newtype ErrorA arr i o = ErrorA { unErrorA :: SomeException -> ArrErrCC arr SomeException i o }
--instance Arrow arr => Error (ArrErrCC arr err i o) where
instance Arrow arr => ArrowError (ErrorA arr err) where
    type ExceptionType (ErrorA arr err) = err
    throwA = exitCC
    catchA e_i2o e_ei2o = case unbox $ unbox e_i2o of
        Left i2e -> let i2ei = withInput i2e
                        e_i2ei = arrCC_ex i2ei
                        e_i2o' = e_i2ei >>> e_ei2o
                    in  e_i2o'
        _ -> e_i2o
    {-
    catchE_ e_i2a e2a = case unbox $ unbox a of
        Left e_i2e -> arr e2a let i2a = switchCC i2e
        _ -> a
    -}



{-
catchesHandlerM :: MonadError m => [HandlerM m a] -> SomeException -> m a
catchesHandlerM handlers e = catchesHandlerE handlers e (throwM e)
catchesHandlerA :: ArrowError arr => [HandlerA arr i o] -> SomeException -> arr i o
catchesHandlerA handlers e = catchesHandlerE handlers e (throwA e)


--}
--}
--}
--}
--}
--}
--}
