{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}



module Ring
    ( Ring (..)
    , RingWithIdentity (..)
    , CommutativeRing (..)
    , CommutativeRingWithIdentity (..)
    , DivisionRing (..)
    , Field (..)

    , IntegralDomain (..)
    , GCD_Domain (..)
    , UniqueFactorizationDomain (..)
    , PrincipalIdealDomain (..)
    , EuclideanDomain (..)


    , exclude0
    , unsafe_exclude0
    , OpIsZero (..)
    , ExcludeZero
    , OpDivides (..)
    , OpGCDSet (..)
    )
where



import Data.Monoid
import Prelude hiding ((/), (+), (-), (*))
import Explain
import SeedUtils (justif, unjust)
import Container_base_static
import Group



infixl 6 +, -
infixl 7 *, /
infixl 7 />
infixr 7 <\

class (Abelian (Sum a), WithZero a) --, OpDivides a) diff with below
    => Ring a where
    -- [(a+b)><c == a><c+b><c][c><(a+b)==c><a+c><b]
    -- R is group[+] and semigroup[*]
    -- zero[><] =?= zero[+] ; yes! ==>> WithZero a
    --   where zero[+] =[def]= zero[>+<][Sum] =[def]= identity[><][Sum]
    --      0[+] >< a = (0[+]+0[+]) >< a = 0[+]><a + 0[+]><a
    --      0[+] = (0><a+0><a)-0><a = 0[+]><a
    --      ==>> 0[+] == 0[><]
    -- not require identity[><]
    (+) :: a -> a -> a
    a + b = getSum $ Sum a >+< Sum b
    neg :: a -> a
    neg = getSum . abel_neg . Sum
    (-) :: a -> a -> a
    a - b = getSum $ Sum a >-< Sum b

instance (Commutative a, Ring a) => CommutativeRing a where
class (Commutative a, Ring a) => CommutativeRing a where
    (*) :: a -> a -> a
    (*) = (>+<)
instance (WithIdentity a, Ring a) => RingWithIdentity a where
class (WithIdentity a, Ring a) => RingWithIdentity a where
    one :: a
    one = identity
    neg_one :: a
    neg_one = neg one



class (CommutativeRing a, RingWithIdentity a)
    => CommutativeRingWithIdentity a where
    -- ideal<S> ::= {sum s*f(s) {s in S} | any f:S->R}}
    -- generated_ideal :: (Set a s, Set a out) => s -> out
    -- principal_ideal<r> ::= ideal<{r}>
    -- principal_ideal :: Set a out => a -> out






class WithZero a => OpIsZero a where
    is_zero :: a -> Bool -- {1} ==>> True; otherwise {True, False}
    not_zero :: a -> Bool
    not_zero = not . is_zero
newtype ExcludeZero a = ExcludeZero a
instance Ring a => To a (ExcludeZero a) where
instance Ring a => Explain a (ExcludeZero a) where
    explain (ExcludeZero a) = a
instance (Ring a, OpIsZero a) => From a (ExcludeZero a) where
    from a = justif (not_zero a) (ExcludeZero a)

exclude0 :: From a (ExcludeZero a) => a -> Maybe (ExcludeZero a)
exclude0 = from
unsafe_exclude0 :: From a (ExcludeZero a) => a -> ExcludeZero a
unsafe_exclude0 = unsafe_from




class (RingWithIdentity a, Group (ExcludeZero a))
    => DivisionRing a where
    -- require: [1=/=0]
    -- [a==zero] || [inv_but_zero a * a == identity == a * inv_but_zero a]
    -- inv_but_zero zero == zero
    -- R = Group \/ {0} | {}\/{0}
    -- -- zero ring ==>> not Group (ExcludeZero a) !!!
    --  should we have [1=/=0]??


    inv_but_0 :: a -> a
        -- 0 -> 0 or undefined or error or undefined behavior??
    -- default inv_but_0 :: OpIsZero a => a -> a
    -- inv_but_0 = maybe zero (explain . inv) . exclude0
    inv_but_0 = maybe zero id . safe_inv

    safe_inv :: a -> Maybe a
    default safe_inv :: OpIsZero a => a -> Maybe a
    safe_inv a = exclude0 a >>= return . explain . inv


    -- a = d*x ==>> x = (1/d)*a = d <\ a -- left_div a d -- flip!!
    -- a = x*d ==>> x = a*(1/d) = a /> d -- right_div a d
    left_div :: a -> a -> Maybe a
    right_div :: a -> a -> Maybe a
    left_div a b = safe_inv b >>= return . (>< a)
    right_div a b = safe_inv b >>= return . (a ><)

    (/>) :: a -> a -> a -- unsafe
    (<\) :: a -> a -> a -- unsafe
    a /> d = unjust $ right_div a d
    d <\ a = unjust $ left_div a d

class   ( DivisionRing a, CommutativeRing a
        -- or:
        , Ring a, Abelian (ExcludeZero a)

        -- not important:
        , EuclideanDomain a -- every element is a unit or 0
        )
    => Field a where
    -- require: [1=/=0]
    div :: a -> a -> Maybe a
    (/) :: a -> a -> a

    div = right_div
    (/) = (/>)










---------------------------------
-- [a|b] require Commutative
-- divides with gcd_set, but more difficult; seperated
class   ( SemiGroup a, Commutative a
        -- , FiniteSet a (GCD_InputType a), Set a (GCD_Result a)
        ) => OpDivides a where
    -- this should be merged into CommutativeSemiGroup
    --          (hence into CommutativeRing)
    -- [a|b] ::= exist c: a*c == b
    -- with zero: [a|0] == True <<== a*0 = 0
    -- with identity: [1|a] == True <<== 1*a = a
    divides :: a -> a -> Bool

    -- [a~~b] ::= [a|b][b|a]
    --      [a~~b] ==>> [b~~a]
    --      [a~~b][b~~c] ==>> [a~~c]
    --      but NO [a~~a]!!
    -- with zero: ~~0 = {0}
    -- with identity: ~~1 = units
    --      now associates is equivalence relation
    --      since [a~~a] <<== a*e = a = e*a
    associates :: a -> a -> Bool
    associates a b = divides a b && divides b a

class   ( OpDivides a
        , Finite a (GCD_InputType a)
        , Container a (GCD_Result a) -- OpNull??
        )
    => OpGCDSet a where
    -- this should be merged into OpDivides
    type GCD_Result a :: *
    type GCD_InputType a :: *
    -- cd_set s ::= {x | all [x|a] {a in s}}
    -- gcd_set s ::= {d | d in cd_set s
    --                    && all [x|d] {x in cd_set s}}
    -- gcd_set maybe emtpy
    gcd_set :: GCD_InputType a -> GCD_Result a













































































































































{-
instance New Sum where
    wrap = Sum
    unwrap = getSum
-}

class CommutativeRing a => IntegralDomain a where
    -- not with identity?? [1 =/= 0] and
    -- -- it seems IntegralDomain have different defs
    -- [[a=/=0][b=/=0] -->> [a*b=/=0]] -- no zero divisors


    -- commutative + with identity:
    --      [unit u] <==> exist w s.t. w*u == e


    -- [prime p] ::= [not unit p][p=a*b -->> [unit a] or [unit b]]
    -- [factorization into primes is essentially unique]
    --      r ::= u * II p {prime p} where [unit u]

    is_prime :: a -> Bool
        -- a =/= 0 && all [a|b] or [a|c] {(b,c)| [a|b*c]}
        -- is_prime ==>> irreducible =xx=>> is_prime
    irreducible :: a -> Bool
        -- a =/= 0 && not unit a && all any unit [b,c] {(b,c) | b*c==a}


class (IntegralDomain a, NonNull a (GCD_Result a)) => GCD_Domain a
class (GCD_Domain a, Finite (a, PInt) (UniqueFactorResult a))
    => UniqueFactorizationDomain a where
    type UniqueFactorResult a :: *
    unique_factor :: a -> UniqueFactorResult a
        -- a = II p^i {(p,i) in out}
        -- irreducible p
        -- unique up to associates
    -- is_prime <==> irreducible

class UniqueFactorizationDomain a => PrincipalIdealDomain a where
    -- PID ::= [every ideal is principal]
    norm_Dedekind_Hasse :: a -> UInt
        -- norm_Dedekind_Hasse a == 0 <==> a == zero
    divmod_Dedekind_Hasse :: a -> a -> (a, a)
        -- a /% b = (s, t)
        -- -- q = t, r = s*a - t*b
        -- -- s*a = t*b + r
        -- -- NOTE: s may not identity
        -- b == zero || [b|a] || 0 < normDH r < normDH b

class   ( IntegralDomain a
        , Natural (NormResult a)
        , PrincipalIdealDomain a
        )
    => EuclideanDomain a where
    -- EuclideanDomain
    type NormResult a :: *
    norm :: a -> NormResult a -- UInt
        -- norm zero == 0
        -- norm a == 0 =xx=>> a == zero
    divmod :: a -> a -> (a, a)
        -- a `divmod` b = (q, r) -- quotient, remainder
        -- a == q*b+r and
        --      b =/= zero || r == zero || norm r < norm b
        --   so, allow norm unit == 0??
        -- a `divmod` zero = (?, a)
        -- result is not unique
        -- maybe we should use a NonNullContainer in output??





-- -}
-- -}
-- -}


