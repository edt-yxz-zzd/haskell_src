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
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}


module Explain
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
import SeedUtils__TH (decsQ_add, decQ2decsQ)
import Language.Haskell.TH
{-# LANGUAGE TemplateHaskell #-}
import SeedUtils (justif, unjust) -- , fromInteger)
import qualified Data.Ratio as Ratio
import qualified Prelude as P
import Prelude hiding (Integral, fromInteger, toInteger, Rational, Real)

-- like Boxed
-- but  1) Boxed assume O(1)
--          and assume box . unbox === unbox . box === id
--      2) no "| u -> spec where"
-- safe_from -- 1..* -> 0/1
-- from -- 1..* -> 1
-- explain -- 1 -> 1
-- make -- 1..* -> 1
--    v.s. from
--    explain . make . explain = explain
--    make . explain . make = make
class To spec u => Explain spec u where
    explain :: u -> spec
    default explain :: OpFrom u spec => u -> spec
    explain = from -- alias
class From spec u => Make spec u where
    -- offer an opportunity to add auxiliary data
    -- or modify data, e.g. to simplify and standardize data
    -- so id /= explan . make /= make . explain
    -- not O(1)
    make :: spec -> u
    default make :: OpFrom spec u => spec -> u
    make = from -- alias

class OpSafeFrom from to => OpFrom from to where
    from :: from -> to
class OpSafeFrom from to where
    safe_from :: from -> Maybe to
    unsafe_from :: from -> to

    default safe_from :: OpFrom from to => from -> Maybe to
    safe_from = Just . from
    unsafe_from = unjust . safe_from

class OpSafeFrom from to => SafeTo to from where
class OpFrom from to => To to from where
class OpSafeFrom from to => SafeFrom from to where
class OpFrom from to => From from to where
instance OpSafeFrom from to => SafeTo to from where
instance OpFrom from to => To to from where
instance OpSafeFrom from to => SafeFrom from to where
instance OpFrom from to => From from to where



class (Explain spec u, Make spec u) => View spec u where
instance (Explain spec u, Make spec u) => View spec u where


class LimitedFrom from to where
    limited_from :: from -> to -- e.g. int -> uint => -1 -> 0









---------------------- toInt
-- instExplain [t|Int|] [t|Integer|]
-- instance ToInteger a => OpSafeFrom a Int where
instance OpSafeFrom Integer Int where
    safe_from i = if i <= P.toInteger (maxBound :: Int) &&
                     i >= P.toInteger (minBound :: Int)
                  then Just $ P.fromIntegral i
                  else Nothing
    unsafe_from i = maybe e id $ safe_from i where
        e = error "unsafe_from :: Integer -> Int"
instance OpSafeFrom Int Integer where
instance OpFrom Int Integer where
    from = P.toInteger






------------------ a a
instance OpSafeFrom a a where
instance OpFrom a a where
    from = id
instance Explain a a
instance Make a a






----------- tpl
instExplain :: TypeQ -> TypeQ -> ExpQ -> DecsQ
-- flip instOpFrom!!
instExplain t f eq = decsQ_add
    [d| instance Explain $t $f where |]
    $ instOpFrom f t eq
instOpFrom :: TypeQ -> TypeQ -> ExpQ -> DecsQ
instOpFrom from_tq to_tq expQ = [d|
    instance OpSafeFrom $from_tq $to_tq where
    instance OpFrom $from_tq $to_tq where
        from = undefined
    |] >>= \[a, InstanceD ctx ts [dec]] -> do
        expr <- expQ
        return [a, InstanceD ctx ts [f dec expr]]
  where
    f dec expr = case dec of
      ValD (VarP nameFrom) (NormalB (VarE _undefined)) [] ->
            ValD (VarP nameFrom) (NormalB expr) []
      x -> error $ show x







--- FromToXXX
def__Ops_FromOrToXXX_ThisOrList :: String -> TypeQ -> DecsQ
def__Ops_FromOrToXXX_ThisOrList strXXX xxxQ =
    decsQ_add
        (def__Ops_FromOrToXXX strXXX xxxQ)
        (def__Ops_FromOrToXXX_List strXXX xxxQ)
def__Ops_FromOrToXXX_List :: String -> TypeQ -> DecsQ
def__Ops_FromOrToXXX_List strXXX xxxQ =
    def__Ops_FromOrToXXX (strXXX ++ "List") [t| [$xxxQ] |]
def__Ops_FromOrToXXX :: String -> TypeQ -> DecsQ
def__Ops_FromOrToXXX strXXX xxxQ = do
    a <- newName "a"
    fmap concat $ sequence $
        [ def__OpSafeToXXX strXXX xxxQ a
        , def__OpToXXX strXXX xxxQ a
        , def__OpSafeFromXXX strXXX a xxxQ
        , def__OpFromXXX strXXX a xxxQ
        ]


---  OpSafeToXXX
sig__safe_toXXX :: String -> TypeQ -> TypeQ -> DecQ
sig__safe_toXXX strXXX to_q from_q = sigD name sigQ where
    name = mkName $ "safe_to" ++ strXXX
    sigQ = [t| $from_q -> Maybe $to_q|]
sig__unsafe_toXXX :: String -> TypeQ -> TypeQ -> DecQ
sig__unsafe_toXXX strXXX to_q from_q = sigD name sigQ where
    name = mkName $ "unsafe_to" ++ strXXX
    sigQ = [t| $from_q -> $to_q|]

def__OpSafeToXXX :: String -> TypeQ -> Name -> DecsQ
def__OpSafeToXXX strXXX to_q from_name = sequence
    [ cls__OpSafeToXXX strXXX to_q from_name
    , inst__OpSafeToXXX strXXX to_q from_name
    ]
clsD__OpSafeToXXX :: String -> TypeQ -> Name -> DecsQ
clsD__OpSafeToXXX strXXX to_q from_name =
    decQ2decsQ $ cls__OpSafeToXXX strXXX to_q from_name
cls__OpSafeToXXX :: String -> TypeQ -> Name -> DecQ
cls__OpSafeToXXX strXXX to_q from_name = classD cxtQ name binds [] decQs where
    name = mkName $ "OpSafeTo" ++ strXXX
    from_q = varT from_name
    cxtQ = cxt [classP ''SafeTo [to_q, from_q]]
    decQs = map (\f-> f strXXX to_q from_q) [sig__safe_toXXX, sig__unsafe_toXXX]
    binds = [PlainTV from_name]

inst__OpSafeToXXX :: String -> TypeQ -> Name -> DecQ
inst__OpSafeToXXX strXXX to_q from_name = instanceD cxtQ typeQ decQs where
    name = mkName $ "OpSafeTo" ++ strXXX
    cls_q = conT name
    from_q = varT from_name
    cxtQ = cxt [classP ''SafeTo [to_q, from_q]]
    typeQ = [t| $cls_q $from_q |]
    name__unsafe_toXXX = mkName $ "unsafe_to" ++ strXXX
    name__safe_toXXX = mkName $ "safe_to" ++ strXXX
    decQs = [ f name__safe_toXXX 'safe_from
            , f name__unsafe_toXXX 'unsafe_from]
    f name_target name_src =
        valD (varP name_target) (normalB (varE name_src)) []
{-
[ValD (VarP unsafe_toXXX_37) (NormalB (VarE Explain.unsafe_from)) []
,ValD (VarP safe_toXXX_38) (NormalB (VarE Explain.safe_from)) []]
-}

---  OpToXXX
sig__toXXX :: String -> TypeQ -> TypeQ -> DecQ
sig__toXXX strXXX to_q from_q = sigD name sigQ where
    name = mkName $ "to" ++ strXXX
    sigQ = [t| $from_q -> $to_q|]

def__OpToXXX :: String -> TypeQ -> Name -> DecsQ
def__OpToXXX strXXX to_q from_name = sequence
    [ cls__OpToXXX strXXX to_q from_name
    , inst__OpToXXX strXXX to_q from_name
    ]
cls__OpToXXX :: String -> TypeQ -> Name -> DecQ
cls__OpToXXX strXXX to_q from_name = classD cxtQ name binds [] decQs where
    name = mkName $ "OpTo" ++ strXXX
    from_q = varT from_name
    cxtQ = cxt [classP ''To [to_q, from_q]]
    decQs = map (\f-> f strXXX to_q from_q) [sig__toXXX]
    binds = [PlainTV from_name]

inst__OpToXXX :: String -> TypeQ -> Name -> DecQ
inst__OpToXXX strXXX to_q from_name = instanceD cxtQ typeQ decQs where
    name = mkName $ "OpTo" ++ strXXX
    cls_q = conT name
    from_q = varT from_name
    cxtQ = cxt [classP ''To [to_q, from_q]]
    typeQ = [t| $cls_q $from_q |]
    name__toXXX = mkName $ "to" ++ strXXX
    decQs = [f name__toXXX 'from]
    f name_target name_src =
        valD (varP name_target) (normalB (varE name_src)) []















---  OpSafeFromXXX
sig__safe_fromXXX :: String -> TypeQ -> TypeQ -> DecQ
sig__safe_fromXXX strXXX to_q from_q = sigD name sigQ where
    name = mkName $ "safe_from" ++ strXXX
    sigQ = [t| $from_q -> Maybe $to_q|]
sig__unsafe_fromXXX :: String -> TypeQ -> TypeQ -> DecQ
sig__unsafe_fromXXX strXXX to_q from_q = sigD name sigQ where
    name = mkName $ "unsafe_from" ++ strXXX
    sigQ = [t| $from_q -> $to_q|]

def__OpSafeFromXXX :: String -> Name -> TypeQ -> DecsQ
def__OpSafeFromXXX strXXX to_name from_q = sequence
    [ cls__OpSafeFromXXX strXXX to_name from_q
    , inst__OpSafeFromXXX strXXX to_name from_q
    ]
cls__OpSafeFromXXX :: String -> Name -> TypeQ -> DecQ
cls__OpSafeFromXXX strXXX to_name from_q = classD cxtQ name binds [] decQs where
    name = mkName $ "OpSafeFrom" ++ strXXX
    to_q = varT to_name
    cxtQ = cxt [classP ''SafeFrom [from_q, to_q]]
    decQs = map (\f-> f strXXX to_q from_q) [sig__safe_fromXXX, sig__unsafe_fromXXX]
    binds = [PlainTV to_name]

inst__OpSafeFromXXX :: String -> Name -> TypeQ -> DecQ
inst__OpSafeFromXXX strXXX to_name from_q = instanceD cxtQ typeQ decQs where
    name = mkName $ "OpSafeFrom" ++ strXXX
    cls_q = conT name
    to_q = varT to_name
    cxtQ = cxt [classP ''SafeFrom [from_q, to_q]]
    typeQ = [t| $cls_q $to_q |]
    name__unsafe_fromXXX = mkName $ "unsafe_from" ++ strXXX
    name__safe_fromXXX = mkName $ "safe_from" ++ strXXX
    decQs = [ f name__safe_fromXXX 'safe_from
            , f name__unsafe_fromXXX 'unsafe_from]
    f name_target name_src =
        valD (varP name_target) (normalB (varE name_src)) []





---  OpFromXXX
sig__fromXXX :: String -> TypeQ -> TypeQ -> DecQ
sig__fromXXX strXXX to_q from_q = sigD name sigQ where
    name = mkName $ "from" ++ strXXX
    sigQ = [t| $from_q -> $to_q|]

def__OpFromXXX :: String -> Name -> TypeQ -> DecsQ
def__OpFromXXX strXXX to_name from_q = sequence
    [ cls__OpFromXXX strXXX to_name from_q
    , inst__OpFromXXX strXXX to_name from_q
    ]
cls__OpFromXXX :: String -> Name -> TypeQ -> DecQ
cls__OpFromXXX strXXX to_name from_q = classD cxtQ name binds [] decQs where
    name = mkName $ "OpFrom" ++ strXXX
    to_q = varT to_name
    cxtQ = cxt [classP ''From [from_q, to_q]]
    decQs = map (\f-> f strXXX to_q from_q) [sig__fromXXX]
    binds = [PlainTV to_name]

inst__OpFromXXX :: String -> Name -> TypeQ -> DecQ
inst__OpFromXXX strXXX to_name from_q = instanceD cxtQ typeQ decQs where
    name = mkName $ "OpFrom" ++ strXXX
    cls_q = conT name
    to_q = varT to_name
    cxtQ = cxt [classP ''From [from_q, to_q]]
    typeQ = [t| $cls_q $to_q |]
    name__fromXXX = mkName $ "from" ++ strXXX
    decQs = [f name__fromXXX 'from]
    f name_target name_src =
        valD (varP name_target) (normalB (varE name_src)) []





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
