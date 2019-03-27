
module UnsafeFromList
    (UnsafeFromList(..)
    )
where

import ToList

class ToList a => UnsafeFromList a where
    -- to allow infinite ... unsafe later...
    unsafe_fromList :: [ElementType4ToList a] -> a
    {-
    maybe_fromList :: [ElementType4ToList a] -> Maybe a
    unsafe_fromList ls = case maybe_fromList ls of
        Just a -> a
        Nothing -> undefined
    -}


