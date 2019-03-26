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

module ValueStateB_R__test_head where

import ValueStateB_R
import Boxed

data D = D {size::Int, time::Int} deriving (Show, Eq, Ord)

data SizeF
instance VStatePB (VPropertyBox D SizeF) Int
instance VStatePR (VPropertyBox D SizeF) Int where
    vget = size . unbox


