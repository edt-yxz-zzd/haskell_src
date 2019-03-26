
module Sized
    ( Sized(..)
    , FiniteList(..)
    , PairAsFiniteList
    ) where


class Sized container where
    len :: container a -> Integer

class Sized ls => FiniteList ls where
    empty_list :: ls a
    -- len empty_list == 0

    prepend :: a -> ls a -> ls a
    maybe_head :: ls a -> Maybe a
    maybe_tail :: ls a -> Maybe (ls a)
    unsafe_fromList :: [a] -> ls a
    toList :: ls a -> [a]
    fromList :: Integer -> [a] -> ls a

-- type P a = (Integer, [a])
-- newtype N a = P a

newtype PairAsFiniteList a = P (Integer, [a]) 
    deriving (Show, Read, Eq, Ord)

-- data PairAsFiniteList a = M (P a)
instance Sized PairAsFiniteList where
    len (P (i, _)) = i
instance FiniteList PairAsFiniteList where
    empty_list = P (0, [])
    prepend a (P (i, ls)) = P (i+1, a:ls)
    maybe_head (P (_, h:_)) = Just h
    maybe_head _ = Nothing
    maybe_tail (P (i, h:t)) = Just $ P (i-1, t)
    maybe_tail _ = Nothing
    unsafe_fromList ls = P (toInteger $ length ls, ls)
    toList (P (_, ls)) = ls
    fromList i ls = unsafe_fromList $ take (fromInteger i) ls



