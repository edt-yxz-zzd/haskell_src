{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Explain.PartialOrd
    ( PartialOrd(..)
    , EqIsSetEq(..)
    , OrdIsTotalSetOrd(..)
    , TotalSetOrd(..)
    , SetOrdEx(..)
    , SetOrd(..)
    , SetEq(..)
    , module Explain.PartialOrdering
    )
where

import Explain.PartialOrdering
import Prelude hiding (Ordering(..))

{-
import qualified Prelude as P
import Explain.ExplainBase
-}




-- why not Ord??
--   Ord is for tree set key, not TotalOrd!
--   i.e. Ord a => Ord (Data.Set a):
--      s0 < s1 = S.toAscList s0 < S.toAscList s1
-- so, Ord (Set a), PartialOrd (Set a) but not TotalOrd!!
--      since PartialOrd is SetOrd
-- class (PartialOrd a, Ord a) => TotalOrd a where
-- rename as TotalSetOrd



{-
-- default version using in non-Ord instance
infix 4 ||~<~||, ||~<=~||
(||~<=~||), (||~<~||) :: PartialOrd a => a -> a -> Bool
default_partial_compare :: PartialOrd a => a -> a -> PartialOrdering
a ||~<=~|| b = a == b || a ~<~ b
a ||~<~|| b = LT == partial_compare a b
default_partial_compare a b
    | a == b = EQ
    | a ~<~ b = LT
    | b ~<~ a = GT
    | otherwise = NO
-}
{-
instance PartialOrd a where
    (~<=~) = (||~<=~||)
    (~<~) = (||~<~||)
    partial_compare = default_partial_compare
-}
-- -}


-- same as < ==
infix 4 ~<~, ~<=~, ~>~, ~>=~
infix 4 |<|, |<=|, |>|, |>=|, |==|, |/=|

class Eq a => PartialOrd a where
    -- ~<~ and ~<=~ and partial_compare
    -- NOTE: no ~==~, using == instead
    --      diff with SetOrd: |==|

    -- [a ~<~ b] ==>> [not (b ~<=~ a)]
    -- [not (b ~<=~ a)] =xx=>> [a ~<~ b]
    -- [not (b ~<=~ a)] ==>> [a ~<~ b] or [no_cmp a b]
    -- [a ~<~ b] <==> [a ~<=~ b][a /= b]
    -- [a ~<~ b] ==>> [a ~<=~ b]
    -- [a == b] ==>> [a ~<=~ b]
    -- [a ~<~ b][b ~<~ c] ==>> [a ~<~ c]
    -- [a ~<~ b][b == c] ==>> [a ~<~ c]
    (~<=~), (~>=~), (~>~), (~<~) :: a -> a -> Bool
    partial_compare :: a -> a -> PartialOrdering

    (~>=~) = flip (~<=~)
    (~>~) = flip (~<~)
    -- partial_compare = default_partial_compare

    a ~<=~ b = a == b || a ~<~ b
    a ~<~ b = LT == partial_compare a b
    partial_compare a b
        | a == b = EQ
        | a ~<~ b = LT
        | b ~<~ a = GT
        | otherwise = NO
    {-# MINIMAL (~<~) | partial_compare #-}



class (Eq a, SetEq a) => EqIsSetEq a
    -- [1, 1] /= [1]
    -- but [1, 1] |==| [1]

class (Ord a, SetOrd a, EqIsSetEq a) => OrdIsTotalSetOrd a
    -- sometimes SetOrd can be total
    -- when "a" is {s[i]| i=[0..?]} and s[i] |<| s[i+1]
    -- i.e. Bool = {{}, {()}}, where False = {}, True = {()}

{-
instance OrdIsTotalSetOrd a => TotalOrd a where
instance (EqIsSetEq a, SetOrd a) => PartialOrd a where
    (~<~) = (|<|)
    (~<=~) = (|<=|)
    partial_compare = set_compare
    -- partial_compare a = ord2pord . compare a
-}


{- error: Ord is not TotalOrd!!
instance Ord a => TotalOrd a
instance Ord a => PartialOrd a where
    (~<~) = (<)
    (~<=~) = (<=)
    partial_compare a = ord2pord . compare a
-- -}


























--------------------  SetOrd


-- {-
-- though SetOrd is a PartialOrd,
--   Ord set may be used as key, is not SetOrd
class SetEq a where
    (|==|), (|/=|) :: a -> a -> Bool
    (|/=|) a = not . (|==|) a

    default (|==|) :: EqIsSetEq a => a -> a -> Bool
    (|==|) = (==)

class (SetEq a, PartialOrdCmpResult (SetCmpExResult a)) => SetOrd a where
    -- |<=|, i.e. subset
    (|<=|), (|>=|), (|>|), (|<|) :: a -> a -> Bool
    set_compare :: a -> a -> PartialOrdering
    (|>=|) = flip (|<=|)
    (|>|) = flip (|<|)
    a |<| b = a |<=| b && not (b |==| a)
    -- set_compare = default_set_compare
    set_compare a b
        | a |==| b = EQ
        | a |<=| b = LT
        | b |<=| a = GT
        | otherwise = NO
    default (|<=|) :: OrdIsTotalSetOrd a => a -> a -> Bool
    (|<=|) = (<=)
    {-# MINIMAL (|<=|) | set_compare #-}
class (SetOrd a, PartialOrdCmpResult (SetCmpExResult a)) => SetOrdEx a where
    type SetCmpExResult a :: *
    type SetCmpExResult a = TotalOrdering
    set_compare_ex :: a -> a -> SetCmpExResult a
    default set_compare_ex :: OrdIsTotalSetOrd a => a -> a -> TotalOrdering
    set_compare_ex = compare

class (SetOrd a, TotalOrdCmpResult (SetCmpExResult a)) => TotalSetOrd a
    -- e.g. all objects of type a 
    --          = {{}, {0}, {0,1}, ... {Int i | 0 <= i < ?}}
instance (SetOrd a, TotalOrdCmpResult (SetCmpExResult a)) => TotalSetOrd a




-- -}

{-
-- default version using in non-Ord instance
infix 4 ||<||, ||<=||
(||<=||), (||<||) :: SetOrd a => a -> a -> Bool
default_set_compare :: SetOrd a => a -> a -> PartialOrdering
a ||<=|| b = let r = set_compare a b in r == LT || r == EQ
a ||<|| b = a |<=| b && not (b |<=| a)
default_set_compare a b
    | a |==| b = EQ
    | a |<=| b = LT
    | b |<=| a = GT
    | otherwise = NO
-}


