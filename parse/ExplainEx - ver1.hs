{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module ExplainEx
    ( module Explain
    , module ExplainEx
    )
where
{-
import Prelude (Eq(..), Ord(..), Maybe(..), Bool(..), Monad(..)
                , (.), ($), Integer, error, not)
-- -}
import qualified Prelude as P
import Prelude hiding (Integral, fromInteger, toInteger, Rational, Real)
import Explain
import Ordering
import SeedUtils (fromInteger)



class OpDynTheOnlyValue a where
    the_only_value :: a



class Explain Bool a => Boolean a where
toBool :: Boolean a => a -> Bool
toBool = explain
-- bad idea: list can not explain as Bool
-- test should not be defined as below:
-- test :: Explain Bool a => a -> Bool
-- test = explain

class (Explain Value_True a, Boolean a) => Boolean_True a
class (Explain Value_False a, Boolean a) => Boolean_False a

class (POrdCmpResult a, Explain P.Ordering a) => OrdCmpResult a where
class Explain POrd a => POrdCmpResult a where


----- Bool
instance To Bool Bool
instance Explain Bool Bool where
    explain = id
instance Boolean Bool where









-- data NotValue_Zero
data Value_Zero
data Value_One
data Value_ZeroOrOne
data Value_True
data Value_False
data Value_EmptySet
-- data Value_TheNonnegativeRealSet
data Value_TheURationalSet
data Value_ThePRationalSet
data Value_Just a = Value_Just a
data Value_GE a = Value_GE a -- a <= Value_GE a

unValue_Just (Value_Just a) = a



class (Explain Integer a, Ord a) => Integral a where
    -- must be integer
toInteger :: Integral a => a -> Integer
toInteger = explain
instance Integral PInt
instance Integral UInt
instance Integral Integer



data Sign = Negative | Positive
data WithSign a = WithSign Sign a
drop_sign (WithSign _ a) = a
get_sign (WithSign s _) = s

class (Integral a, Explain UInt a) => Natural a where
    -- nonnegative integer
class (Natural a, Explain PInt a) => PositiveInt a where
class (Natural a, Explain (WithSign PInt) a) => IntegralBut0 a where
class (Natural a, Explain Value_Zero a) => NaturalEq0 a where
class (Natural a, Explain Value_ZeroOrOne a) => NaturalLe1 a where
class (Natural a, Explain Value_One a) => NaturalEq1 a where












------------- rational / real

-- class (Explain RationalNum a) => Rational a where
class   ( Explain (Div (Numerator a) (Denominator a)) a
        , Integral (Numerator a), PositiveInt (Denominator a)
        , Explain P.Rational a
        , Ord a
        )
    => Rational a where
    type Numerator a :: *
    type Denominator a :: *
    toDiv :: a -> Div (Numerator a) (Denominator a)
    toDiv = explain
    numerator :: a -> Numerator a
    numerator = _div2n . toDiv
    denominator :: a -> Denominator a
    denominator = _div2d . toDiv
toRational :: Rational a => a -> P.Rational
toRational = explain



class   (Rational a, IntegralBut0 (Numerator a))
    => RationalBut0 a
class   (Rational a, PositiveInt (Numerator a), Explain EpsBut0 a)
    => PositiveRational a where
class   (Rational a, Natural (Numerator a), Explain Eps a)
    => NonNegativeRational a where
toEpsBut0 :: PositiveRational a => a -> EpsBut0
toEpsBut0 = explain
toEps :: NonNegativeRational a => a -> Eps
toEps = explain

-- case explain a :: Div (Numerator a) (Denominator a) of
--    Div n d -> mkPRationalNum n d










----------------- real -- useless - since without Eq/Ord
class (Rational (RealAsFrac a)) -- no Eq/Ord
    => RealState a st | a -> st, st -> a where
    -- st = (a, eps, frac)
    {-
    Cycle in class declaration (via superclasses):
      Real -> RealState -> Real
    In the class declaration for `Real'
    -}
    type RealAsFrac a :: *
    state2real :: st -> a
    state2frac :: st -> RealAsFrac a
    state2error :: st -> EpsBut0
    real2state :: EpsBut0 -> a -> st

    -- |frac - a| <= error
    boundedBy :: EpsBut0 -> a -> RealAsFrac a
    boundedBy e = state2frac . real2state e
    boundedBy_ex :: EpsBut0 -> st -> st
class   ( RealState a (BoundedByState a)
        , Rational (RealAsFrac a)
        --, EpsBut0 -- PositiveRational (Error a)
        -- no Eq/Ord a
        )
    => Real a where -- | (BoundedByState a) -> a where
    type BoundedByState a :: *
    -- type Error a :: * -- using EpsBut0



class   ( Real a, Explain (Value_GE EpsBut0) a
        --, PositiveRational (PRationalLeType a)
        )
    => PositiveReal a where
    any_eps_but0_le :: a -> EpsBut0
    any_eps_but0_le a = let Value_GE eps = explain a in eps
    {-
    -- type PRationalLeType a :: * -- EpsBut0
    any_prational_le :: a -> PRationalLeType a
    any_prational_le = explain
    any_eps_but0_le :: a -> EpsBut0
    any_eps_but0_le = toEpsBut0 . any_prational_le
    -}

class   ( Real a, Explain (WithSign (PRealType a)) a
        , PositiveReal (PRealType a))
    => RealBut0 a where
    type PRealType a :: *
    preal_eq_abs :: a -> PRealType a
    preal_eq_abs = drop_sign . explain -- (explain :: a -> WithSign (PRealType a))
    any_eps_but0_le_abs :: a -> EpsBut0
    any_eps_but0_le_abs = any_eps_but0_le . preal_eq_abs

