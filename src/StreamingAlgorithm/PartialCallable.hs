

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module PartialCallable
    (PartialCallable(..)
    ,PartialTransform(..)
    ,Type4PartialTransform
    )
where

import Data.Semigroup
import Callable

class (Callable a
        , OutputType4Callable a ~ Maybe (PartialOutputType4Callable a)
    ) => PartialCallable a where
    type PartialOutputType4Callable a :: *
    partial_call :: a -> InputType4Callable a -> Maybe (PartialOutputType4Callable a)
    partial_call = call


type Type4PartialTransform a = PartialOutputType4Callable a
class (PartialCallable a
        , InputType4Callable a ~ PartialOutputType4Callable a
        , Semigroup a
        , Monoid a
    ) => PartialTransform a where
    partial_transform :: a -> Type4PartialTransform a -> Maybe (Type4PartialTransform a)
    partial_transform = partial_call

instance (PartialCallable a
        , InputType4Callable a ~ PartialOutputType4Callable a
        , Semigroup a
        , Monoid a
    ) => PartialTransform a where


