{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskell #-}


module Explain_abc_Integral
    ( module Explain
    , module Explain_abc_Boolean
    {-
    , module Explain_abc_Integral
    , module Explain__Bool
    , module Explain__Integer
    , module Explain__UInt
    , module Explain__PInt
    , module Explain__DInt
    -}
    , Integral (..)
    , Value_Zero (..)
    , Value_One (..)
    , Value_NegOne (..)
    , Value_ZeroOrOne (..)
    , Value_ZeroOrOneOrNegOne (..)
    , IntegralD (..)
    , IntegralU (..)
    , IntegralP (..)
    , Natural (..)
    , NaturalEq0 (..)
    , NaturalEq1 (..)
    , NaturalLe1 (..)
    , IntegralEqNeg1 (..)
    , Integral_AbsLe1 (..)
    , DynIntegral (..)
    , OpDynIntegral (..)
    , DynIntegralU (..)
    , OpDynIntegralU (..)
    , DynIntegralD (..)
    , OpDynIntegralD (..)
    , DynIntegralP (..)
    , OpDynIntegralP (..)
    , DynNatural (..)
    , OpDynNatural (..)
    , typeAssertIntegral
    , typeAssertIntegralU
    , typeAssertIntegralD
    , typeAssertIntegralP
    )

where
import PrintQ
import Language.Haskell.TH
import Language.Haskell.Syntax
import Control.Monad
import SeedUtils__TH
{-# LANGUAGE TemplateHaskell #-}


import Explain
import Explain_abc_Boolean
{-
import Explain__Bool
import Explain__Integer
import Explain__UInt
import Explain__PInt
import Explain__DInt
-}

import UInt
import PInt
import DInt

import OpDynTheOnlyValue
import NumConcept
import Language.Haskell.TH

import NumOp
import OpTest
import SeedUtils (justif)
import qualified Prelude as P -- hiding (or, and, not)
import Prelude hiding (or, and, not, Integral)






---------------------- abc Integral
{-
class OpIsZero a where
class OpIsUnit a where
    -- e.g. +1 -1 for Integral; any but 0 for Rational
    safe_inv :: a -> Maybe a
class OpIsOne a where
class OpIsTwo a where
class OpIsNegOne a where
class OpIsNegTwo a where

class Explain Ordering (SignExResult a)
    => OpSign a where
    type SignExResult a :: *
    type SignExResult a = Ordering
    sign :: a -> Ordering
    sign_ex :: a -> SignExResult a
    -- sign a = toInteger a `compare` 0
-}
class (Explain Integer a, IntegralConcept a, Ord a)
    --, OpSign a)
    => Integral a where
    -- must be "a" integer
    -- but may not store "any" integer
    -- Why IntegralConcept?
    --      1) because we need Integral to define others...
    --          no Rational a => Integral a
    --      2) we can explain Bool by Integer,
    --          but we not treat Bool as Integral
    --          since Bool is not IntegralConcept
    -- no (Ord a)?? since we can explain it by Integer, it must Ord
    --      e.g. in factored form : II primes -- though slow
instance (Explain Integer a, IntegralConcept a, Ord a)
    => Integral a where

data Value_Zero
data Value_One
data Value_NegOne
newtype Value_ZeroOrOne = Value_ZeroOrOne Bool
    deriving (Eq, Ord, Show, Read)
    -- Either Value_Zero Value_One
newtype Value_ZeroOrOneOrNegOne = Value_ZeroOrOneOrNegOne Ordering
    deriving (Eq, Ord, Show, Read)



def__instances [d|
    class (Integral a, Explain DInt a) => IntegralD a where
    class (Integral a, Explain UInt a) => IntegralU a where
        -- nonnegative integer
    class (IntegralU a) => Natural a where
    class (IntegralU a, IntegralD a, Explain PInt a) => IntegralP a where
    class (Natural a, Explain Value_Zero a) => NaturalEq0 a where
    class (IntegralP a, Explain Value_One a) => NaturalEq1 a where
    class (Natural a, Explain Value_ZeroOrOne a) => NaturalLe1 a where
    class (IntegralD a, Explain Value_NegOne a) => IntegralEqNeg1 a where
    class (Integral a, Explain Value_ZeroOrOneOrNegOne a)
        => Integral_AbsLe1 a where
    |]







instOpDynTheOnlyValues $ map conT
    [''Value_One, ''Value_Zero, ''Value_NegOne]
_a = the_only_value :: Value_One
b1 = 1 == (from _a :: Integer)
------------------ Value_Zero
inst_num_concepts ''UIntConcept (conT ''Value_Zero)
instExplain (conT ''Integer) (conT ''Value_Zero)  $ [e| const 0 |]
instExplain (conT ''Value_ZeroOrOne) (conT ''Value_Zero)
    $ [e| const (Value_ZeroOrOne False) |]
instance OpSafeFrom Integer Value_Zero where
    safe_from i = justif (0 == i) the_only_value
instExplain (conT ''UInt) (conT ''Value_Zero)  $ [e| const 0 |]
instance OpSafeFrom UInt Value_Zero where
    safe_from i = justif (0 == i) the_only_value
------------------ Value_One
inst_num_concepts ''PIntConcept (conT ''Value_One)
instExplain (conT ''Integer) (conT ''Value_One)  $ [e| const 1 |]
instExplain (conT ''Value_ZeroOrOne) (conT ''Value_One)
    $ [e| const (Value_ZeroOrOne True) |]
instance OpSafeFrom Integer Value_One where
    safe_from i = justif (1 == i) the_only_value
instExplain (conT ''PInt) (conT ''Value_One)  $ [e| const 1 |]
instance OpSafeFrom PInt Value_One where
    safe_from i = justif (1 == i) the_only_value
instExplain (conT ''UInt) (conT ''Value_One)  $ [e| const 1 |]
instance OpSafeFrom UInt Value_One where
    safe_from i = justif (1 == i) the_only_value
instExplain (conT ''DInt) (conT ''Value_One)  $ [e| const 1 |]
instance OpSafeFrom DInt Value_One where
    safe_from i = justif (1 == i) the_only_value
------------------ Value_NegOne
inst_num_concepts ''DIntConcept (conT ''Value_NegOne)
instExplain (conT ''Integer) (conT ''Value_NegOne)  $ [e| const (-1) |]
instance OpSafeFrom Integer Value_NegOne where
    safe_from i = justif (-1 == i) the_only_value
instExplain (conT ''DInt) (conT ''Value_NegOne)  $ [e| const (-1) |]
instance OpSafeFrom DInt Value_NegOne where
    safe_from i = justif (-1 == i) the_only_value
------------------ Value_ZeroOrOne
from_value01 :: Num a => Value_ZeroOrOne -> a
from_value01 (Value_ZeroOrOne False) = 0
from_value01 (Value_ZeroOrOne True) = 1
safe_to_value01 :: (Eq a, Num a) => a -> Maybe Value_ZeroOrOne
safe_to_value01 i   | 0 == i = Just $ Value_ZeroOrOne False
                    | 1 == i = Just $ Value_ZeroOrOne True
                    | otherwise = Nothing

inst_num_concepts ''UIntConcept (conT ''Value_ZeroOrOne)
instExplain (conT ''Integer) (conT ''Value_ZeroOrOne)  $ [e| from_value01 |]
instance OpSafeFrom Integer Value_ZeroOrOne where
    safe_from = safe_to_value01
instExplain (conT ''UInt) (conT ''Value_ZeroOrOne)  $ [e| from_value01 |]
instance OpSafeFrom UInt Value_ZeroOrOne where
    safe_from = safe_to_value01
------------------ Value_ZeroOrOneOrNegOne
from_value_101 :: Num a => Value_ZeroOrOneOrNegOne -> a
from_value_101 (Value_ZeroOrOneOrNegOne EQ) = 0
from_value_101 (Value_ZeroOrOneOrNegOne GT) = 1
from_value_101 (Value_ZeroOrOneOrNegOne LT) = -1
safe_to_value_101 :: (Eq a, Num a) => a -> Maybe Value_ZeroOrOneOrNegOne
safe_to_value_101 i | 0 == i = Just $ Value_ZeroOrOneOrNegOne EQ
                    | 1 == i = Just $ Value_ZeroOrOneOrNegOne GT
                    | -1 == i = Just $ Value_ZeroOrOneOrNegOne LT
                    | otherwise = Nothing

inst_num_concepts ''IntegralConcept (conT ''Value_ZeroOrOneOrNegOne)
instExplain (conT ''Integer) (conT ''Value_ZeroOrOneOrNegOne)  $ [e| from_value_101 |]
instance OpSafeFrom Integer Value_ZeroOrOneOrNegOne where
    safe_from = safe_to_value_101







---------------------- DynIntegral
{-
class   ( View Integer a, Integral a, OpDynIntegral a)
    => DynIntegral a
instance( View Integer a, Integral a, OpDynIntegral a)
    => DynIntegral a
class   ( OpAdd a, OpSub a, OpMul a, OpNeg a
        , OpSafeDivModEx a a a a
        , OpDivModEx a a a DInt
        , OpSafeDivEx a a a
        , OpDivEx Rational a DInt
        --, OpSafeFloorLogEx a PInt a -- log 1 x??
        , OpPowEx a a UInt -- 0 ** 0 == 1??
        --, OpGcdEx a
        , OpGcd a
        )
    => OpDynIntegral a where
instance( OpAdd a, OpSub a, OpMul a, OpNeg a
        , OpSafeDivModEx a a a a
        , OpDivModEx a a a DInt
        , OpSafeDivEx a a a
        , OpDivEx Rational a DInt
        --, OpSafeFloorLogEx a PInt a -- log 1 x??
        , OpPowEx a a UInt -- 0 ** 0 == 1??
        --, OpGcdEx a
        , OpGcd a
        )
    => OpDynIntegral a where

-}
class D a
instance D Bool
def__instances [d|
    class D a => C a where
        f :: a -> Int
        f a = 0
    _0 = f False
    |]


def__instances [d|
    class   ( View Integer a, Integral a, OpDynIntegral a)
        => DynIntegral a
    class   ( OpAdd a, OpSub a, OpMul a, OpAbs a, OpNeg a
            , OpSafeDivModEx a a a a
            , OpDivModEx a a a DInt
            , OpSafeDivEx a a a
            , OpDivEx Rational a DInt
            --, OpSafeFloorLogEx a PInt a -- log 1 x??
            , OpPowEx a a UInt -- 0 ** 0 == 1??
            --, OpGcdEx a
            , OpGcd a
            )
        => OpDynIntegral a where
    class   (View UInt a, IntegralU a, OpDynIntegralU a)
        => DynIntegralU a where
    class   ( OpAdd a, OpSafeSub a, OpMul a, OpAbs a
            , OpSafeDivModEx a a a a
            , OpDivModEx a a a PInt
            , OpSafeDivEx a a a
            , OpDivEx Rational a DInt
            --, OpSafeFloorLogEx a PInt a
            , OpPowEx a a a -- 0 ** 0 == 1??
            , OpGcd a
            )
        => OpDynIntegralU a where

    class OpDynIntegralU a => OpDynNatural a where
    class DynIntegralU a => DynNatural a



    class   ( View DInt a, IntegralD a, OpDynIntegralD a)
        => DynIntegralD a
    class   ( OpSafeAdd a, OpSafeSub a, OpMul a, OpAbs a, OpNeg a
            , OpSafeDivModEx a a a a
            , OpDivModEx Integer Integer a a
            , OpSafeDivEx a a a
            , OpDivEx Rational a a
            --, OpSafeFloorLogEx a PInt a
            , OpPowEx a a UInt -- 0 ** 0 == 1??
            )
        => OpDynIntegralD a where
    class   ( View PInt a, IntegralP a, OpDynIntegralP a)
        => DynIntegralP a
    class   ( OpAdd a, OpSafeSub a, OpMul a, OpAbs a
            , OpSafeDivModEx a a a a
            , OpDivModEx Integer Integer a a
            , OpSafeDivEx a a a
            , OpDivEx Rational a a
            --, OpSafeFloorLogEx a PInt a
            --, OpSafeFloorLogEx a a a
            , OpPowEx a a UInt -- 0 ** 0 == 1??
            , OpPowEx a a a
            , OpGcd a
            )
        => OpDynIntegralP a where
    |]


typeAssertIntegral :: DynIntegral a => a -> a
typeAssertIntegral = id
typeAssertIntegralU :: DynIntegralU a => a -> a
typeAssertIntegralU = id
typeAssertIntegralD :: DynIntegralD a => a -> a
typeAssertIntegralD = id
typeAssertIntegralP :: DynIntegralP a => a -> a
typeAssertIntegralP = id

inst_num_concepts ''IntegralConcept (conT ''Integer)
b2 = typeAssertIntegral (1::Integer)


instance OpSafeDivEx Rational UInt DInt where
instance OpDivEx Rational UInt DInt where
    div_ex n d = toRational n P./ toRational d
inst_num_concepts ''UIntConcept (conT ''UInt)
b3 = typeAssertIntegralU (1::UInt)


instance OpSafePowEx DInt DInt UInt where
instance OpPowEx DInt DInt UInt where
    pow_ex b n = P.fromInteger (P.toInteger b P.^ P.toInteger n)
inst_num_concepts ''DIntConcept (conT ''DInt)
b4 = typeAssertIntegralD (1::DInt)



instance OpSafePowEx PInt PInt UInt where
instance OpPowEx PInt PInt UInt where
    pow_ex b n = P.fromInteger (P.toInteger b P.^ P.toInteger n)
inst_num_concepts ''PIntConcept (conT ''PInt)
b5 = typeAssertIntegralP (1::PInt)
------------------- 
--}
--}
--}



