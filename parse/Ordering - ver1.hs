{-# LANGUAGE  DefaultSignatures
            , FlexibleInstances
            , UndecidableInstances
            , OverlappingInstances
            , IncoherentInstances
 #-}

module Ordering where
import Prelude hiding (Ordering(..))
import qualified Prelude as P
import Boxed
import SeedUtils (lift2)

data POrd = NO | LT | EQ | GT
    deriving (Show, Read, Eq, Ord)
ord2pord :: P.Ordering -> POrd
ord2pord P.LT = LT
ord2pord P.EQ = EQ
ord2pord P.GT = GT


-- same as < ==
infix 4 ~<~, ~<=~, ~>~, ~>=~
infix 4 |<|, |<=|, |>|, |>=|, |==|, |/=|
infix 4 ||~<~||, ||~<=~||
infix 4 ||<||, ||<=||


-- why not Ord??
--   Ord is for tree set key, not TotalOrd!
--   i.e. Ord a => Ord (Data.Set a):
--      s0 < s1 = S.toAscList s0 < S.toAscList s1
-- so, Ord (Set a), PartialOrd (Set a) but not TotalOrd!!
--      since PartialOrd is SetOrd
class (PartialOrd a, Ord a) => TotalOrd a where




-- {-
-- default version using in non-Ord instance
(||~<=~||), (||~<~||) :: PartialOrd a => a -> a -> Bool
default_partial_compare :: PartialOrd a => a -> a -> POrd
a ||~<=~|| b = a == b || a ~<~ b
a ||~<~|| b = LT == partial_compare a b
default_partial_compare a b
    | a == b = EQ
    | a ~<~ b = LT
    | b ~<~ a = GT
    | otherwise = NO
{-
instance PartialOrd a where
    (~<=~) = (||~<=~||)
    (~<~) = (||~<~||)
    partial_compare = default_partial_compare
-}
-- -}


class Eq a => PartialOrd a where
    -- ~<~ and ~<=~ and partial_compare
    -- NOTE: no ~==~, using == instead
    --      diff with SetOrd: |==|

    -- [a ~<~ b] ==>> [not (b ~<=~ a)] =xx=>> [a ~<~ b]
    --  |= [not (a ~<~ b)]or[not (b ~<=~ a)]
    --  |= not [a ~<~ b][b ~<=~ a]
    --  [b ~<=~ a]or[a ~<~ b] may be False
    -- [a ~<~ b] <==> [a ~<=~ b][a /= b]
    -- [a ~<~ b] ==>> [a ~<=~ b]
    -- [a == b] ==>> [a ~<=~ b]
    -- [a ~<~ b][b ~<~ c] ==>> [a ~<~ c]
    -- [a ~<~ b][b == c] ==>> [a ~<~ c]
    (~<=~), (~>=~), (~>~), (~<~) :: a -> a -> Bool
    partial_compare :: a -> a -> POrd

    (~>=~) = flip (~<=~)
    (~>~) = flip (~<~)
    partial_compare = default_partial_compare

    {-
    a ~<=~ b = a == b || a ~<~ b
    a ~<~ b = LT == partial_compare a b
    partial_compare a b
        | a == b = EQ
        | a ~<~ b = LT
        | b ~<~ a = GT
        | otherwise = NO
    -- -}

    {-
    default (~<=~) :: Ord a => a -> a -> Bool
    (~<=~) = (<=)
    default (~<~) :: Ord a => a -> a -> Bool
    (~<~) = (<)
    default partial_compare :: Ord a => a -> a -> POrd
    partial_compare a = ord2pord . compare a
    -- -}

class (Eq a, SetEq a) => EqIsSetEq a
    -- [1, 1] /= [1]
    -- but [1, 1] |==| [1]

class (Ord a, SetOrd a, EqIsSetEq a) => OrdIsTotalSetOrd a
    -- sometimes SetOrd can be total
    -- when "a" is {s[i]| i=[0..?]} and s[i] |<| s[i+1]
    -- i.e. Bool = {{}, {()}}, where False = {}, True = {()}


instance OrdIsTotalSetOrd a => TotalOrd a where
instance (EqIsSetEq a, SetOrd a) => PartialOrd a where
    (~<~) = (|<|)
    (~<=~) = (|<=|)
    partial_compare = set_compare
    -- partial_compare a = ord2pord . compare a

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

    default (|==|) :: SetOrd a => a -> a -> Bool
    a |==| b = a |<=| b && b |<=| a
class SetEq a => SetOrd a where
    -- |<=|, i.e. subset
    (|<=|), (|>=|), (|>|), (|<|) :: a -> a -> Bool
    set_compare :: a -> a -> POrd

    (|>=|) = flip (|<=|)
    (|>|) = flip (|<|)
    a |<| b = a |<=| b && not (b |<=| a)
    set_compare = default_set_compare
-- -}

-- {-
-- default version using in non-Ord instance
(||<=||), (||<||) :: SetOrd a => a -> a -> Bool
default_set_compare :: SetOrd a => a -> a -> POrd
a ||<=|| b = let r = set_compare a b in r == LT || r == EQ
a ||<|| b = a |<=| b && not (b |<=| a)
default_set_compare a b
    | a |==| b = EQ
    | a |<=| b = LT
    | b |<=| a = GT
    | otherwise = NO
{-
instance SetOrd a where
    (|<=|) = (||<=||)
    (|<|) = (||<||)
-}
-- -}
instance (Eq a, EqIsSetEq a) => SetEq a where
    (|==|) = (==)
instance OrdIsTotalSetOrd a => SetOrd a where
    (|<|) = (<)
    (|<=|) = (<=)
    set_compare a = ord2pord . compare a

newtype SetWrapper a = SetWrapper a
    deriving (Show, Read)
instance New SetWrapper where
    wrap = SetWrapper
    unwrap (SetWrapper a) = a
instance SetEq a => Eq (SetWrapper a) where
    (==) = lift2 unwrap (|==|)
instance SetEq a => EqIsSetEq (SetWrapper a)
    -- ==>> SetEq (SetWrapper a)
instance SetOrd a => SetOrd (SetWrapper a) where
    -- SetOrd a ==>> SetEq a
    -- ==>> Eq (SetWrapper a)
    -- ==>> EqIsSetEq (SetWrapper a)
    -- ==>> SetEq (SetWrapper a)
    -- +SetOrd ==>> PartialOrd (SetWrapper a)
    (|<|) = lift2 unwrap (|<|)
    (|<=|) = lift2 unwrap (|<=|)
instance OrdIsTotalSetOrd a => Ord (SetWrapper a) where
    (<) = lift2 unwrap (|<|)
    (<=) = lift2 unwrap (|<=|)
instance OrdIsTotalSetOrd a => OrdIsTotalSetOrd (SetWrapper a) where

























------------- instance Bool
instance EqIsSetEq Bool
instance OrdIsTotalSetOrd Bool

