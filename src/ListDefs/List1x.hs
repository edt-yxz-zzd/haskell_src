
module ListDefs.List1x
where
import Seed.UnsafeUnpack (unsafe_unjust)
import Control.Monad (ap)
import Data.Monoid
import Data.Semigroup
import Data.List.NonEmpty
type List1x = NonEmpty
list2maybe_list1x :: [a] -> Maybe (List1x a)
list2maybe_list1x (h:ts) = Just (h :| ts)
list2maybe_list1x _ = Nothing

unsafe_list2list1x = unsafe_unjust . list2maybe_list1x
list1x2list (h :| ts) = h:ts



{-
newtype List1x a = List1x (a, [a])
list2maybe_list1x :: [a] -> Maybe (List1x a)
list2maybe_list1x (h:ts) = Just (List1x (h, ts))
list2maybe_list1x _ = Nothing

unsafe_list2list1x = unsafe_unjust . list2maybe_list1x
list1x2list (List1x (h,ts)) = h:ts



instance Functor List1x where
    fmap f (List1x (h, ts)) = List1x (f h, fmap f ts)
instance Applicative List1x where
    pure = return
    (<*>) = ap
instance Monad List1x where
    return h = List1x (h, [])
    ls1x >>= f = unsafe_list2list1x (list1x2list ls1x >>= list1x2list . f)

{-
instance Monoid (List1x a) where
    mappend lhs rhs = unsafe_list2list1x $
        list1x2list lhs ++ list1x2list rhs

    mempty = ??
-}

instance Semigroup (List1x a) where
    lhs <> rhs = unsafe_list2list1x $
        list1x2list lhs ++ list1x2list rhs

--}
--}
--}
