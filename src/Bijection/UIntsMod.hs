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



module Bijection.UIntsMod
where



import Seed.Boxed
import Seed.ProxyOps
import Data.Proxy
import Bijection.TransformObj
import Bijection.BijectionC

import IntDefs.IntDefs
import Seed.ListOps (split_eq, join1)
--import Seed.ListOps (list_len_ge, genericLength, split_eq, join1)
import GHC.TypeLits
import Test.Enum (allWords, range)

import Bijection.Mod
import IntTransform.Integer2Bytes
    ( uint2chunks__be, chunks2uint__be
    , uint2chunks__le, chunks2uint__le
    )
    {-
    ( digits2uint__le, digits2uint__be
    , uint2digits__le, uint2digits__be)
    -}

{-
input for SplitMax
    ([UInt] mod m >= 2) <-> (UInt, m :: m >= 2)
    ([[UInt]] mod m :: (m >= 2, len >= 1)) <-> ([UInt] :: len >= 1, m :: m >= 2)
-}

--data UIntPair2UIntsMod
data Endian = LittleEndian | BigEndian
    deriving (Eq, Ord, Show, Read)
--newtype OffsetedUIntsMod (e :: Endian) = OffsetedUIntsMod [UInt]
endian2is_little :: Endian -> Bool
endian2is_little = (LittleEndian ==)



data UIntsMod2UIntWithOldMod (e :: Endian) (ge :: Nat)
    = UIntsMod2UIntWithOldMod
type FromTpl m =
    Relabel_TransformF OldModulo (FirstL_TransformF (UIntsModGeLenGe m 0))
data WithOldModuloGe :: Nat -> *
type From = Mod [UInt]
type To = (UInt, UInt)
--data WithOldModuleGeConstraint :: Nat -> *
--data OldModuleGeConstraint :: Nat -> *
instance (KnownNat m)
    => TransformArgTpl (WithOldModuloGe m) where
    type ArgType (WithOldModuloGe m) = (UInt, UInt) -- (M, u)
    type ArgConstraint (WithOldModuloGe m) = OldModuloGe m
    type ArgConfigure (WithOldModuloGe m) = Label OldModulo UInt

instance GetOldModulo (UInt, a) where
    getOldModulo = fst
instance (GetOldModulo a, KnownNat m) => Constraint (OldModuloGe m) a where
    verifyP pWn a = toInteger (getOldModulo a) >= natVal (last1P pWn)

instance GetOldModulo a => Configure (Label OldModulo UInt) a where
    extract_config = box . getOldModulo

type LE m = UIntsMod2UIntWithOldMod LittleEndian m
instance KnownNat m => TransformObj (Forward (LE m)) where
    type ArgTplFrom (Forward (LE m)) = FromTpl m
    type ArgTplTo (Forward (LE m)) = WithOldModuloGe m
    transform _ (Mod om us) = (om, chunks2uint__le om us)
instance KnownNat m => TransformObj (Backward (LE m)) where
    type ArgTplFrom (Backward (LE m)) = WithOldModuloGe m
    type ArgTplTo (Backward (LE m)) = FromTpl m
    transform _ (om, u) = Mod om $ uint2chunks__le om u

instance KnownNat m => BijectionC (LE m)



type BE m = UIntsMod2UIntWithOldMod BigEndian m
instance KnownNat m => TransformObj (Forward (BE m)) where
    type ArgTplFrom (Forward (BE m)) = FromTpl m
    type ArgTplTo (Forward (BE m)) = WithOldModuloGe m
    transform _ (Mod om us) = (om, chunks2uint__be om us)
instance KnownNat m => TransformObj (Backward (BE m)) where
    type ArgTplFrom (Backward (BE m)) = WithOldModuloGe m
    type ArgTplTo (Backward (BE m)) = FromTpl m
    transform _ (om, u) = Mod om $ uint2chunks__be om u

instance KnownNat m => BijectionC (BE m)


ls = map (forwardWithFullVerifyEq_ (UIntsMod2UIntWithOldMod :: LE 2)
        . Mod 3) . take 300 $ allWords [0..2]
ls' = map (backwardWithFullVerifyEq_ (UIntsMod2UIntWithOldMod :: LE 3)
        . (\u -> (3,u))) $ [0..2000]
pr = do
    print ls
    print ls'



