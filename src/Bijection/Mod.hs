{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}



module Bijection.Mod
where



import Seed.Boxed
import Data.Proxy
import Bijection.TransformObj
import Bijection.BijectionC

import IntDefs.IntDefs
import Seed.ListOps (list_len_ge, genericLength) -- , split_eq, join1)
import GHC.TypeLits
import Seed.ProxyOps (last1P)
--import Test.Enum (allWords, range)


data Modulo
data OldModulo

data Len -- finite
data LenGe :: Nat -> * -- finite
data ModuloGe :: Nat -> *
data OldModuloGe :: Nat -> *
data ModuloEq :: Nat -> *
--data ModuloGe_LenGe :: Nat -> Nat -> *
type ModuloGe_LenGe m len = ModuloGe m :&&: LenGe len
type ModuloEq_LenGe m len = ModuloEq m :&&: LenGe len
data Mod a = Mod UInt a
    deriving (Eq, Ord, Read, Show)

class Constraint Len a => GetLen a where
    getLen :: a -> UInt
class Constraint Modulo a => GetModulo a where
    getModulo :: a -> UInt
class GetOldModulo a where
    getOldModulo :: a -> UInt
instance Constraint Modulo (Mod a) => GetModulo (Mod a) where
    getModulo (Mod m _) = m
instance (Constraint Len (Mod a), GetLen a) => GetLen (Mod a) where
    getLen (Mod _ a) = getLen a
instance Constraint Len [a] where
    verifyP _ ls = length ls >= 0
instance GetLen a => Constraint Len (Mod a) where
    verifyP p (Mod _ ls) = verifyP p ls




instance (KnownNat n, GetModulo a) => Constraint (ModuloGe n) a where
    verifyP pMn a = b0 && b1 where
        b0 = toInteger (getModulo a) >= natVal (last1P pMn)
        b1 = verifyP (Proxy :: Proxy Modulo) a
instance (KnownNat n, GetModulo a) => Constraint (ModuloEq n) a where
    verifyP pMn a = b0 && b1 where
        b0 = toInteger (getModulo a) == natVal (last1P pMn)
        b1 = verifyP (Proxy :: Proxy Modulo) a

instance Constraint Modulo (Mod UInt) where
    verifyP _ (Mod m u) = u < m || m == 0
instance Constraint Modulo (Mod a) => Constraint Modulo (Mod [a]) where
    verifyP p (Mod m us) = all (verifyP p) $ map (\u -> (Mod m u)) us



instance KnownNat len => Constraint (LenGe len) (Mod [a]) where
    verifyP pLl (Mod _ ls) = list_len_ge (natVal $ last1P pLl) ls



instance Constraint Modulo (Mod a)
    => Configure (Label Modulo UInt) (Mod a) where
    extract_config = box . getModulo
instance Configure (Label Len UInt) (Mod [a]) where
    extract_config (Mod _ us) = box $ genericLength us


--instance Configure (Label (Modulo, Len) (UInt, UInt)) (Mod [UInt]) where
--    extract_config (m, us) = box (m, genericLength us)


data UIntsModGeLenGe :: Nat -> Nat -> *
data UIntsModEqLenGe :: Nat -> Nat -> *
instance (KnownNat m, KnownNat len)
    => TransformArgTpl (UIntsModGeLenGe m len) where
    type ArgType (UIntsModGeLenGe m len) = Mod [UInt]
    type ArgConstraint (UIntsModGeLenGe m len) = ModuloGe_LenGe m len
    type ArgConfigure (UIntsModGeLenGe m len)
        = Label (Modulo :&&: Len) (UInt :&&: UInt)
instance (KnownNat m, KnownNat len)
    => TransformArgTpl (UIntsModEqLenGe m len) where
    type ArgType (UIntsModEqLenGe m len) = Mod [UInt]
    type ArgConstraint (UIntsModEqLenGe m len) = ModuloEq_LenGe m len
    type ArgConfigure (UIntsModEqLenGe m len)
        = Label (Modulo :&&: Len) (UInt :&&: UInt)


{-

type T = (UInt, [UInt])
pr = do
    print $ verifyP (Proxy :: Proxy (ModuloGe 2)) ((2, []) :: T)
    print . not $ verifyP (Proxy :: Proxy (ModuloGe 2)) ((1, []) :: T)
    print . not $ verifyP (Proxy :: Proxy (ModuloGe 2)) ((2, [2]) :: T)







--}
--}
--}
--}
--}
--}



