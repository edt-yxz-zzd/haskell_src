

module Bijection.BiArrow
    ( module Bijection.BiArrow
    , module Control.Category
    )
where


import Prelude hiding (id)
import Control.Category


class Category arr => BiArrow arr where
    first :: arr a b -> arr (a, c) (b, c)
    first a2b = a2b *** id
    second :: arr a b -> arr (c, a) (c, b)
    second a2b = id *** a2b
    (***) :: arr b c -> arr b' c' -> arr (b, b') (c, c')
    swap_pair :: arr (a, b) (b, a)
    first_ex :: (c -> arr a b) -> arr (a, c) (b, c)
    first_ex f = swap_pair >>> second_ex f >>> swap_pair
    second_ex :: (c -> arr a b) -> arr (c, a) (c, b)
class BiArrow arr => BiArrowChoice arr where
    left :: arr b c -> arr (Either b d) (Either c d)
    left = (+++ id)
    right :: arr b c -> arr (Either d b) (Either d c)
    right = (id +++)
    (+++) :: arr b c -> arr b' c' -> arr (Either b b') (Either c c')
    (|||) :: arr b d -> arr c d -> arr (Either b c) (Bool, d)
    swap_either :: arr (Either a b) (Either b a)


