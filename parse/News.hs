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


module News where
import Control.Monad (liftM, ap)
import Boxed

class New n => New1 n where
    -- n a <==> w a (n a)
    new1 :: a -> n a
    new1 = wrap
    old1 :: n a -> a
    old1 = unwrap
instance New n => New1 n
type WrapNew1 = WrapNew
{-
lift1N :: New1 n => (a -> b) -> (n a -> n b)
lift1N f = new1 . f . old1
app1N :: New1 n => n (a -> b) -> (n a -> n b)
app1N = liftN . old1


newtype WrapNew1 n1 a = WrapNew1 { unWrapNew1 :: n1 a }
instance New1 n1 => New1 (WrapNew1 n1) where
    new1 = WrapNew1 . new1
    old1 = old1 . unWrapNew1
-}

class New2 n where
    -- n a b <==> w (a b) (n a b)
    new2 :: a b -> n a b
    old2 :: n a b -> a b
-- lift2N f = new2 . f . old2

newtype WrapNew2 n2 (a :: * -> *) b = WrapNew2 { unWrapNew2 :: n2 a b }
instance New2 n2 => New2 (WrapNew2 n2) where
    new2 = WrapNew2 . new2
    old2 = old2 . unWrapNew2
newtype TtoNew2 f a b = TtoNew2 { unTtoNew2 :: a b } deriving (Monad)
instance New2 (TtoNew2 p) where
    new2 = TtoNew2
    old2 = unTtoNew2



class New3k n where
    -- a b c
    new3 :: a b c -> n a b c
    old3 :: n a b c -> a b c

n2Tn1 :: (New1 n1, New2 n2) => n2 a b -> n1 (a b)
n2Tn1 = new1 . old2
n1Tn2 :: (New1 n1, New2 n2) => n1 (a b) -> n2 a b
n1Tn2 = new2 . old1

new_mn :: (New1 n1, Monad m) => (m a) -> m (n1 a)
new_mn = liftM new1
old_mn :: (New1 n1, Monad m) => m (n1 a) -> m a
old_mn = liftM old1
swap_nm :: (New1 n1, Monad m) => n1 (m a) -> m (n1 a)
swap_nm = new_mn . old1
swap_mn :: (New1 n1, Monad m) => m (n1 a) -> n1 (m a)
swap_mn = new1 . old_mn
push_n1m :: (New1 n1, Monad m) => n1 (m a) -> n1 (m (n1 a))
push_n1m = liftN new_mn -- new1 . swap_nm
n2mTmn1 :: (New1 n1, New2 n2, Monad m) => n2 m a -> m (n1 a)
n2mTmn1 = swap_nm . n2Tn1
mn1Tn2m :: (New1 n1, New2 n2, Monad m) =>  m (n1 a) -> n2 m a
mn1Tn2m = n1Tn2 . swap_mn




