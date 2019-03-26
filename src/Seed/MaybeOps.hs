
module Seed.MaybeOps
    ( module Seed.MaybeOps
    , module Data.Maybe
    )
where

import Data.Maybe

bool2maybe :: Bool -> a -> Maybe a
bool2maybe b a = if b then Just a else Nothing

just_if :: (a -> Bool) -> a -> Maybe a
just_if pred a = bool2maybe (pred a) a
unJust :: Maybe a -> a
unJust = fromJust

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust lhs rhs = if isJust lhs then lhs else rhs
lastJust = flip firstJust

maybe2either :: Maybe a -> Either () a
maybe2either (Just a) = Right a
maybe2either _ = Left ()


