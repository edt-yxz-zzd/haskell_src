
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}






{-
    class C x where
        type T x
        f :: T x -> a -- error : no x
        g :: T x -> Label x a -- fine
-}

module Label
    ( Label
    , Label1
    , getLabel
    , getLabelM
    )
where
import Boxed
import Control.Applicative


newtype Label x a = Label a
instance Functor (Label x) where
    fmap f (Label a) = Label (f a)
instance Boxed a (Label x a) where
    box = Label
    unbox (Label a) = a
instance New (Label x) where
    wrap = box
    unwrap = unbox

instance Applicative (Label x) where
    pure = box -- return
    Label f <*> Label a = Label (f a) -- ap
    (*>) = const id
    (<*) = const
instance Monad (Label x) where
    return = box
    Label a >>= f = f a

getLabel :: x -> Label x a -> a
getLabel _ = unbox
type Label1 m = Label (m())
getLabelM :: Monad m => Label1 m a -> m a
getLabelM = return . unbox



