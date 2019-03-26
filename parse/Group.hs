{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}



{-
Introduction to Knot Theory (Richard H. Crowell, Ralph H. Fox)(1963)

how to make a group?
1) element
2) product
3) 1)+2) ==>> semigroup
4) equivalence classes ==>> group


1) category
2) inverse ==>> groupoid
3) relative to one identity ==>> group



explain:
1) what's elements?
2) how to combine them? require associative.
3) 1)+2) ==>> semigroup
4-1) if not identity, then Maybe a, Nothing is identity
    ==>> semigroup with identity
4-2) if a*b=e and a*u=e then let [=b=] == [=u=]
    ==>> [=a=]\[=e=] = [=b=] ==>> semigroup with left or right inverser
    if b*a=c and u*a=c then let [=b=] == [=u=]
    ==>> [=a=] inversable
    ==>> group



1) category = {a->b, b->c, ...}
    semigroup
2) inversable ==>> groupoid = {a->b, b->a, b->c, c->b, ...}
    NOTE: a->b * b->a == a->a, but b->a * a->b == b->b!!
    select a->a as identity

3) group[a] =  {(a->x[1], x[1]->x[2],...,x[n-1]->a) | x[i]->x[i+1] in groupoid}
            \/ {(a->a,)}???


-}



module Group where
import Data.Monoid
import Prelude hiding ((/), (+), (-), (*))
-- import Explain
import SeedUtils (justif, unjust)


infixr 5 >< -- like ++ ??
infixl 6 >+<, >-<
infixl 7 >/<

class SemiGroup a where
    -- [(a >< b) >< c === a >< (b >< c)]
    (><) :: a -> a -> a -- */++/ monad () >>
    default (><) :: WithIdentity a => a -> a -> a
    (><) = mappend
class (SemiGroup a, Monoid a) => WithIdentity a where
    -- [identity >< a === a === a >< identity]
    -- G =/= {}
    identity :: a
    identity = mempty -- 1/[]/return ()

    -- SemiGroup a => Monoid (Maybe a)

class WithIdentity a => OpIsUnit a where
    -- THIS should be merge into WithIdentity
    is_unit :: a -> Bool
    -- [unit u] ::= exist w, v, s.t. w*u == e == u*v
    -- only w*u == e is not enough
    --
    -- ==>> w = w*e = w*(u*v) = (w*u)*v = v
    -- <==> exist w, s.t. w*u == e == u*w
    -- ==>> w is unit too!
    -- ==>> e/u = u\e = w
    -- units ==>> group
instance (SemiGroup a, Monoid a) => WithIdentity a


class (SemiGroup a) => WithZero a where
    -- [zero >< a === zero === a >< zero]
    -- mul zero not add zero which is identity
    zero :: a -- 0/undefined/fail ""
class (SemiGroup a) => Commutative a where
    -- [a >< b === b >< a]
    (>+<) :: a -> a -> a
    (>+<) = (><)

{-
class (SemiGroup a, Set a (DivResult a)) => OpSafeDiv a where
    type DivResult a :: *
    -- [a\b] ::= {c | a*c == b}
    left_div :: a -> a -> DivResult a
    -- [a/b] ::= {c | c*b == a}
    right_div :: a -> a -> DivResult a

    default left_div
        :: (Group a, DynSingleton a)
        => a -> a -> DivResult a
    left_div a b = singleton $ b / a
    default right_div :: Group a => a -> a -> DivResult a
    right_div = flip left_div
-}

class (WithIdentity a) --, OpSafeDiv a, Singleton a (DivResult a))
    => Group a where
    -- [inv a >< a === identity === a >< inv a]
    inv :: a -> a
    inv a = identity >/< a
    (>/<) :: a -> a -> a
    a >/< b = a >< inv b

class (Commutative a, Group a) => Abelian a where
    -- zero[>+<] = identity[><]
    -- neg[>+<] = inv[><]
    (>-<) :: a -> a -> a
    (>-<) = (>/<)
    abel_neg :: a -> a
    abel_neg = inv
instance (Commutative a, Group a) => Abelian a











