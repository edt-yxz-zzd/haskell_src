
module ListDefs.List2x
where
import Seed.UnsafeUnpack (unsafe_unjust)
import Control.Monad (ap)
import Data.Monoid
import Data.Semigroup
import Data.List.NonEmpty
import Data.Foldable
import Data.Traversable


newtype List2x a = List2x ((a, a), [a])
list2maybe_list2x :: [a] -> Maybe (List2x a)
list2maybe_list2x (h0:h1:ts) = Just (List2x ((h0,h1), ts))
list2maybe_list2x _ = Nothing

unsafe_list2list2x = unsafe_unjust . list2maybe_list2x
list2x2list (List2x ((h0,h1),ts)) = h0:h1:ts



instance Functor List2x where
    fmap f (List2x ((h0,h1), ts)) = List2x ((f h0, f h1), fmap f ts)

instance Semigroup (List2x a) where
    lhs <> rhs = unsafe_list2list2x $
        list2x2list lhs ++ list2x2list rhs

instance Foldable List2x where
    foldMap f = foldMap f . list2x2list
    foldr f z = foldr f z . list2x2list
instance Traversable List2x where
    -- traverse f (List2x ((h0,h1), ts))
    sequenceA (List2x ((h0,h1), ts)) = r where
        h01 = (,) <$> h0 <*> h1
        ls = (,) <$> h01 <*> sequenceA ts
        r = List2x <$> ls


--}
--}
--}
