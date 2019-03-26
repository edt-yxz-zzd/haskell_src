{-# LANGUAGE TypeFamilies #-}
module Container.OpDiscard
where
import Container.IContainer

class IContainer a => OpDiscard a where
    -- discard at most 1 element
    -- if not discard then Nothing
    discard_le1_ex :: Element a -> a -> Maybe a
    discard_le1 :: Element a -> a -> a
    discard_le1 e a = maybe a id $ discard_le1_ex e a
    discard_all :: Element a -> a -> a
    discard_all e a = maybe a g $ f a where
        f = discard_le1_ex e
        g = discard_all e
    discard_all_ex :: Element a -> a -> Maybe a
    discard_all_ex e = fmap (discard_all e) . discard_le1_ex e


