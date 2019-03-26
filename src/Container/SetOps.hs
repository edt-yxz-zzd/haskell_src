{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Container.SetOps
where

import Container.OpEmpty
import Prelude as P hiding (null)


infixl 9 \-\, |-|, \-\~
infixr 3 /-\, //\\, -/\, /\-, -//\\, //\\-, /?\, /-\~, ~/-\
infixr 2 \-/, \+/, -\/, \/-, \-/~, ~\-/

class OpSetDisjoint s where
    (/?\), disjoint, not_disjoint :: s -> s -> Bool
    (/?\) = not_disjoint
    not_disjoint a = P.not . disjoint a
    default disjoint :: (OpIsEmpty s, OpSetIntersection s)
        => s -> s -> Bool
    disjoint a b = is_empty $ a /-\ b

class OpSetSymmetricDifference s where
    -- a |-| b = (a \-/ b) \-\ (a /-\ b) = (a\-\b) \+/ (b\-\a)
    (|-|), symmetric_difference :: s -> s -> s
    (|-|) = symmetric_difference
    default symmetric_difference
        :: (OpSetDifference s, OpSetAdd s) => s -> s -> s
    symmetric_difference a b = set_add (a\-\b) (b\-\a)
class OpSetDifference s where
    (\-\), difference :: s -> s -> s
    (\-\) = difference
    (\-\~) :: s -> [s] -> s
    (\-\~) = foldl (\-\)
class OpSetIntersection s where
    (/-\), intersection :: s -> s -> s
    (/-\) = intersection

    default intersection :: OpSetDifference s => s -> s -> s
    intersection a b = a \-\ (a \-\ b)
    (/-\~) :: s -> [s] -> s
    (~/-\) :: [s] -> s -> s
    (/-\~) = foldl (/-\)
    (~/-\) = flip $ foldr (/-\)
class OpSetIntersection s => OpSetBiasedIntersection s where
    (-/\) :: s -> s -> s -- left biased intersection
    (/\-) :: s -> s -> s -- right biased intersection
    (/\-) = flip (-/\)
    default (-/\) :: OpSetDifference s => s -> s -> s
    a -/\ b = a \-\ (a\-\b)
class OpSetAdd s where
    -- union a b while a /-\ b == {}
    -- if a /-\ b /= {} then undefined behavior
    (\+/), set_add :: s -> s -> s
    (\+/) = set_add

class OpSetUnion s where
    (\-/), union :: s -> s -> s
    (\-/) = union
    default union :: (OpSetAdd s, OpSetDifference s) => s -> s -> s
    union a b = a \+/ (b\-\a)
    (\-/~) :: s -> [s] -> s
    (~\-/) :: [s] -> s -> s
    (\-/~) = foldl (\-/)
    (~\-/) = flip $ foldr (\-/)
class OpSetUnion s => OpSetBiasedUnion s where
    (-\/) :: s -> s -> s -- left biased union
    (\/-) :: s -> s -> s -- right biased union
    (\/-) = flip (-\/)
    default (-\/) :: (OpSetAdd s, OpSetDifference s) => s -> s -> s
    a -\/ b = a \+/ (b\-\a)

class (OpSetIntersection s, OpSetDifference s)
    => OpSetSep s where
    sep :: s -> s -> (s, s, s) -- (0\-\1, 0&1, 1\-\0)
    sep a b = (a\-\b, a/-\b, b\-\a)
    (//\\) :: s -> s -> (s, s, s)
    (//\\) = sep

class (OpSetBiasedIntersection s, OpSetSep s)
    => OpSetBiasedSep s where
    (-//\\) :: s -> s -> (s, s, s)
    a -//\\ b = (a\-\b, a-/\b, b\-\a)
    (//\\-) :: s -> s -> (s, s, s)
    a //\\- b = (a\-\b, a/\-b, b\-\a)


class (OpSetSep s, OpSetUnion s, OpSetAdd s
    , OpSetSymmetricDifference s, OpSetDisjoint s)
    => SetOp s where
    -- any container (not neccesary set) may have these operations
    -- may not be container!!
class (OpSetBiasedSep s, SetOp s)
    => SetBiasedOp s where
instance (OpSetSep s, OpSetUnion s, OpSetAdd s
    , OpSetSymmetricDifference s, OpSetDisjoint s)
    => SetOp s where
instance (OpSetBiasedSep s, SetOp s)
    => SetBiasedOp s where





