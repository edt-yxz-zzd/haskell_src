

-- file: ch13/Monoid.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid

newtype AInt = A { unA :: Int }
    deriving (Show, Eq, Num)
-- monoid under addition
instance Monoid AInt where
    mempty = 0
    mappend = (+)
newtype MInt = M { unM :: Int }
    deriving (Show, Eq, Num)
-- monoid under multiplication
instance Monoid MInt where
    mempty = 1
    mappend = (*)


class MonoidBy a by where
    mempty :: a
    mappend :: a->a->a
class DefaultClass d
data Default
instance DefaultClass Default
class (Monoid a, MonoidBy a Default) => Monoid a where
    mempty = mempty
    mappend = mappend






