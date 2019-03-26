{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Explain__Integer
{-
    ( Explain (..)
    , Make (..)
    , From (..)
    , To (..)
    , View (..)
    , UInt
    , PInt
    , IntBut0
    , Div
    , URationalNum -- (..)
    , PRationalNum -- (..)
    , Eps
    , EpsBut0
    , unURationalNum
    , unPRationalNum
    , mkURationalNum
    , mkPRationalNum
    --, toInteger
    , _div2n, _div2d
    , fromInteger
    )
-}
where
import Explain
import SeedUtils (justif, unjust) -- , fromInteger)
import qualified Data.Ratio as Ratio
import qualified Prelude as P
import Prelude hiding (Integral, fromInteger, toInteger, Rational, Real)



------------------ to/from Integer -- alias
class SafeTo Integer a => OpSafeToInteger a where
    unsafe_toInteger :: a -> Integer
    safe_toInteger :: a -> Maybe Integer
class To Integer a => OpToInteger a where
    toInteger :: a -> Integer
instance SafeTo Integer a => OpSafeToInteger a where
    unsafe_toInteger = unsafe_from
    safe_toInteger = safe_from
instance To Integer a => OpToInteger a where
    toInteger = from

class SafeFrom Integer a => OpSafeFromInteger a where
    unsafe_fromInteger :: Integer -> a
    safe_fromInteger :: Integer -> Maybe a
class From Integer a => OpFromInteger a where
    fromInteger :: Integer -> a
instance SafeFrom Integer a => OpSafeFromInteger a where
    unsafe_fromInteger = unsafe_from
    safe_fromInteger = safe_from
instance From Integer a => OpFromInteger a where
    fromInteger = from




{-
newtype PInt = PInt Integer -- positive
    deriving (Eq, Ord, Show)
newtype IntBut0 = IntBut0 Integer -- nonzero
    deriving (Eq, Ord, Show)
    -- <==> WithSign PInt


instance From Integer PInt where
    from i = justif (i > 0) $ PInt i
instance To Integer PInt where
instance Explain Integer PInt where
    explain (PInt i) = i

instance From Integer IntBut0 where
    from i = justif (i /= 0) $ IntBut0 i
instance To Integer IntBut0 where
instance Explain Integer IntBut0 where
    explain (IntBut0 i) = i

_explain_as_int :: Explain Integer a => a -> Integer
_explain_as_int = explain

instance From UInt PInt where
    from (UInt i) = justif (i /= 0) $ PInt i
instance To UInt PInt where
instance Explain UInt PInt where
    explain (PInt i) = UInt i

instance From IntBut0 PInt where
    from (IntBut0 i) = justif (i > 0) $ PInt i
instance To IntBut0 PInt where
instance Explain IntBut0 PInt where
    explain (PInt i) = IntBut0 i


instance From Integer Integer where
instance Make Integer Integer where
    make = id
instance To Integer Integer where
instance Explain Integer Integer where
    explain = id


_toInteger :: Explain Integer a => a -> Integer
_toInteger = explain





------


data Div n d = Div n d -- xxx do not export ctor?? xxx
_div2n (Div n _) = n
_div2d (Div _ d) = d
-- data RationalNum = RationalNum Integer PInt -- P.Rational
newtype URationalNum = URationalNum (Div UInt PInt) -- do not export ctor
newtype PRationalNum = PRationalNum (Div PInt PInt) -- do not export ctor
-- newtype RationalNumBut0 = RationalNumBut0 (Div IntBut0 PInt) -- WithSign PRationalNum
_div :: (Explain Integer n, Explain Integer d) => n -> d -> P.Rational
_rational2pair :: P.Rational -> (Integer, Integer)
_convert :: (From Integer n, From Integer d)
    => (Integer, Integer) -> (n, d)
_div n d = _toInteger n Ratio.% _toInteger d
-- _toRational = P.toRational . _toInteger
-- _div n d = _toRational n P./ _toRational d
_rational2pair r = (Ratio.numerator r, Ratio.denominator r)
_convert (n, d) = (unsafe_from n, unsafe_from d)
_pair2div (n, d) = Div n d
_toDiv ::   ( Explain Integer n, Explain Integer d
            , From Integer n, From Integer d)
    => n -> d -> Div n d
_toDiv n d = _pair2div . _convert . _rational2pair $ _div n d

unURationalNum (URationalNum a) = a
unPRationalNum (PRationalNum a) = a
mkURationalNum :: UInt -> PInt -> URationalNum
mkURationalNum n d = URationalNum $ _toDiv n d
mkPRationalNum :: PInt -> PInt -> PRationalNum
mkPRationalNum n d = PRationalNum $ _toDiv n d
type EpsBut0 = PRationalNum
type Eps = URationalNum
-- type RationalDivisor = RationalNumBut0


-}
