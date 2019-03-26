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

module ValueStateW_U__test_head
    (module ValueStateB_R__test_head
    ,module ValueStateW_U__test_head
    )
where

import ValueStateB_R
import ValueStateW_U
import Boxed
import ValueStateB_R__test_head

{-
data D = D {size::Int, time::Int} deriving (Show, Eq, Ord)
data SizeF
instance VStatePB (VPropertyBox D SizeF) Int
instance VStatePR (VPropertyBox D SizeF) Int where
    vget = size . unbox
-}
instance VStatePW (VPropertyBox D SizeF) Int where
    vset s d = liftB (\d -> d {size=s}) d -- box $ (unbox d) {size=s}
instance VStatePU (VPropertyBox D SizeF) Int















instance VStatePB (VPropertyBox DD SizeF) Int
instance VStatePR (VPropertyBox DD SizeF) Int where
    vget = size2 . unbox

instance VStatePW (VPropertyBox DD SizeF) Int where
    vset s d = liftB (\d -> d {size2=s}) d -- box $ (unbox d) {size=s}
instance VStatePU (VPropertyBox DD SizeF) Int

instance VStatePB (VPropertyBox DD DF) D
instance VStatePR (VPropertyBox DD DF) D where
    vget = getD . unbox
instance VStatePW (VPropertyBox DD DF) D where
    vset s d = liftB (\d -> d {getD=s}) d
instance VStatePU (VPropertyBox DD DF) D

data DF



data DD = DD {getD::D, size2::Int, time2::Int} deriving (Show, Eq, Ord)






