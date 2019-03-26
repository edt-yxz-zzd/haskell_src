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



module Bijection.SplitMax
    ( UIntsMod2SplitMax(..)
    )
where



import Seed.Boxed
import Data.Proxy
import Bijection.TransformObj
import Bijection.BijectionC

import IntDefs.IntDefs
import Seed.ListOps (split_eq, join1)
--import Seed.ListOps (list_len_ge, genericLength, split_eq, join1)
import GHC.TypeLits
import Test.Enum (allWords, range)

import Bijection.Mod


{-

split max
    [UInt] mod M :: M >= 4
    <-> ([[UInt]] mod 2 :: len >= 1 | [[UInt]] mod max :: (3 <= max < M, len >= 2)) :: M >= 4
-}

type SplitMax_FromTpl =
    Relabel_TransformF OldModulo (FirstL_TransformF (UIntsModGeLenGe 4 0))
type SplitMax_ToTpl = FirstL_TransformF SplitMaxTpl
type SplitMaxDataConstraint = (ModuloEq_LenGe 2 1 :||: ModuloGe_LenGe 3 2)
data SplitMax = SplitMax UInt (Mod [[UInt]])
    deriving (Eq, Ord, Read, Show)
data SplitMaxTpl
data SplitMaxConstraint
instance Constraint SplitMaxConstraint SplitMax where
    verifyP _ (SplitMax om d@(Mod m us)) = b0 && b1 where
        b0 = om >= 4 && m < om
        b1 = verifyP (Proxy :: Proxy SplitMaxDataConstraint) d
instance Configure (Label OldModulo UInt) SplitMax where
    extract_config (SplitMax om _) = box om
instance Configure (Label Modulo UInt) SplitMax where
    extract_config (SplitMax _ mod) = extract_config mod
instance Configure (Label Len UInt) SplitMax where
    extract_config (SplitMax _ mod) = extract_config mod
instance TransformArgTpl SplitMaxTpl where
    type ArgType SplitMaxTpl = SplitMax
    type ArgConstraint SplitMaxTpl = SplitMaxConstraint
    type ArgConfigure SplitMaxTpl
        = Label (OldModulo :&&: Modulo :&&: Len) (UInt :&&: UInt :&&: UInt)





data UIntsMod2SplitMax = UIntsMod2SplitMax
    {-  [UInt] mod M :: M >= 4
    <-> ( [[UInt]] mod 2 :: len >= 1
        | [[UInt]] mod max :: (3 <= max < M, len >= 2)) :: M >= 4
    -}
instance TransformObj (Forward UIntsMod2SplitMax) where
    type ArgTplFrom (Forward UIntsMod2SplitMax) = SplitMax_FromTpl
    type ArgTplTo (Forward UIntsMod2SplitMax) = SplitMax_ToTpl
    transform _ (Mod om us) = SplitMax om (Mod max_ uss) where
        max_ = foldr max 2 us
        uss = split_eq max_ us
instance TransformObj (Backward UIntsMod2SplitMax) where
    type ArgTplFrom (Backward UIntsMod2SplitMax) = SplitMax_ToTpl
    type ArgTplTo (Backward UIntsMod2SplitMax) = SplitMax_FromTpl
    transform _ (SplitMax om (Mod max_ uss)) = Mod om us where
        us = join1 max_ uss

instance BijectionC UIntsMod2SplitMax


ls = map (forwardWithFullVerifyEq_ UIntsMod2SplitMax . Mod 4)
    . take 300 $ allWords [0..3]
pr = do
    print ls



