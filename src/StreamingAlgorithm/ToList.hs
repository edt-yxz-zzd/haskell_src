
{-# LANGUAGE TypeFamilies #-}
module ToList
    (ToList(..)
    )
where

class ToList a where
    type ElementType4ToList a :: *
    toList :: a -> [ElementType4ToList a]


