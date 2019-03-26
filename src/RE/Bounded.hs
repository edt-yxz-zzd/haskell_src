{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module RE.Bounded where


import Numeric.Natural
import Prelude hiding (Bounded)


class Ord a => OpMinBound a where
    min_bound :: a
class Ord a => OpMaxBound a where
    max_bound :: a
class (OpMinBound a, OpMaxBound a) => Bounded a where
instance (OpMinBound a, OpMaxBound a) => Bounded a where
data InsertMin a = InsertMin_Min | InsertMin_Eq a
    deriving (Eq, Ord, Show, Read, Functor)
data InsertMax a = InsertMax_Eq a | InsertMax_Max
    deriving (Eq, Ord, Show, Read, Functor)
data InsertMinMax a
    = InsertMinMax_Min | InsertMinMax_Eq a | InsertMinMax_Max
    deriving (Eq, Ord, Show, Read, Functor)
instance Ord a => OpMinBound (InsertMin a) where
    min_bound = InsertMin_Min
instance OpMaxBound a => OpMaxBound (InsertMin a) where
    max_bound = InsertMin_Eq max_bound
instance OpMinBound a => OpMinBound (InsertMax a) where
    min_bound = InsertMax_Eq min_bound
instance Ord a => OpMaxBound (InsertMax a) where
    max_bound = InsertMax_Max
instance Ord a => OpMinBound (InsertMinMax a) where
    min_bound = InsertMinMax_Min
instance Ord a => OpMaxBound (InsertMinMax a) where
    max_bound = InsertMinMax_Max




instance OpMinBound Char where
    min_bound = minBound
instance OpMaxBound Char where
    max_bound = maxBound
instance OpMinBound Natural where
    min_bound = 0
instance OpMinBound String where
    min_bound = ""

{-
class Ord a => OpSucc a where
    -- succ max_bound = undefined
    succ :: a -> a
instance (OpMinBound a, OpSucc a) => OpSucc [a] where
    succ = (++ [min_bound])
instance OpSucc Char where
    succ = 
-}


{-
instance OpMinBound a => Label s a where
    min_bound = Label min_bound
instance OpMaxBound a => Label s a where
    max_bound = Label max_bound
instance Real s => RangePoint (Label s (InsertMinMax Rational)) where
    -- Real s => toRational
-}

