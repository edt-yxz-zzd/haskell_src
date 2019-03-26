{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Container.ISet
where

import Container.IContainer
import Container.OpMember
--import Container.SetOps
import Explain.PartialOrd
import Prelude hiding (Ordering(..))
import qualified Prelude as P




class (OpMember a, SetConcept a, Key a ~ Element a) => ISetBase a where
    -- no Eq a, it should present in instance decl
    -- Eq a in not necessary for OpMember
    --      e.g. EmptySet or ISet of one value type
    -- allow duplicates; but they donot affect |==|
    (|<-|) :: Element a -> a -> Bool -- member
    (|<-|) = member -- in
class (ISetBase a, SetOrd a) => ISet a
class (ISet a, TotalSetOrd a) => ITotalOrdSet a
    -- cannot support SetDiff in general
    -- e.g. KeySetType of Seq : {{}, {0}, {0,1}, ...}
class (ISet a, ILinearElementsSetBase a) => ILinearElementsSet a
class (ISetBase a, Eq (Element a)) => ILinearElementsSetBase a where
    -- ITotalOrdSet : set are ordered
    -- ILinearElementsSet : elements per set are ordered
    --                      but diff sets may diff order
    --                      used as topological sort result
    --                      no (Ord a)
    -- preconditions: e in a
    before :: a -> Element a -> Element a -> TotalOrdering
class (ISet a, IClockwiseElementsSetBase a) => IClockwiseElementsSet a
class (ISetBase a, Eq (Element a)) => IClockwiseElementsSetBase a where
    -- elements per set are ordered in a cycle
    -- but diff sets may diff order
    -- used as value type of planar embedding
    -- no (Ord a)
    clockwise :: a -> Element a -> Element a -> Element a -> TotalOrdering
        -- LT - clockwise
        -- EQ - |{a,b,c}| < 3 - both clockwise and anti-clockwise
        -- GT - anti-clockwise

    default clockwise :: ILinearElementsSet a
        => a -> Element a -> Element a -> Element a -> TotalOrdering
    clockwise = default_clockwise__byOrd_ex . before
    {-
    clockwise c e1 e2 e3 =
        -- undefined behavior if e1 e2 e3 not in a
        if e1 == e2 || e2 == e3 || e3 == e1 then P.EQ else
        if e1 < e2 then if e3 < e1 || e2 < e3 then P.LT else P.GT else
        if e1 > e2 then if e3 > e1 || e2 > e3 then P.GT else P.LT else
        error "logic error @clockwise"
      where
        e1 == e2 = before c e1 e2 P.== P.EQ
        e1 < e2 = before c e1 e2 P.== P.LT
        e1 > e2 = before c e1 e2 P.== P.GT
        infix 4 ==, <, >
    -}

instance (OpMember a, SetConcept a) => ISetBase a
instance (ISetBase a, SetOrd a) => ISet a
instance (ISet a, TotalSetOrd a) => ITotalOrdSet a
instance (ISet a, ILinearElementsSetBase a) => ILinearElementsSet a
instance (ISet a, IClockwiseElementsSetBase a) => IClockwiseElementsSet a


default_clockwise__byOrd_ex
    :: (a -> a -> TotalOrdering) -> (a -> a -> a -> TotalOrdering)
default_clockwise__byOrd_ex compare e1 e2 e3 =
        -- undefined behavior if e1 e2 e3 not in a
        if e1 == e2 || e2 == e3 || e3 == e1 then P.EQ else
        if e1 < e2 then if e3 < e1 || e2 < e3 then P.LT else P.GT else
        if e1 > e2 then if e3 > e1 || e2 > e3 then P.GT else P.LT else
        error "logic error @default_clockwise__byOrd"
      where
        e1 == e2 = compare e1 e2 P.== P.EQ
        e1 < e2 = compare e1 e2 P.== P.LT
        e1 > e2 = compare e1 e2 P.== P.GT
        infix 4 ==, <, >
default_clockwise__byOrd :: Ord a => a -> a -> a -> TotalOrdering
default_clockwise__byOrd = default_clockwise__byOrd_ex compare




