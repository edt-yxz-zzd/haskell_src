
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Callable
    (Callable(..)
    ,Transform(..)
    ,IO_Type4Transform
    )
where

import Data.Semigroup

class Callable a where
    type InputType4Callable a :: *
    type OutputType4Callable a :: *
    call :: a -> InputType4Callable a -> OutputType4Callable a

type IO_Type4Transform a = OutputType4Callable a
class (Callable a
        , InputType4Callable a ~ OutputType4Callable a
        , Semigroup a
        , Monoid a
    ) => Transform a where
    transform :: a -> IO_Type4Transform a -> IO_Type4Transform a
    transform = call

instance (Callable a
        , InputType4Callable a ~ OutputType4Callable a
        , Semigroup a
        , Monoid a
    ) => Transform a where


