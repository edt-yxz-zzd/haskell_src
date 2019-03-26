{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}



module IntDefs.DNat
where

import Data.Kind
import Data.Typeable
import Numeric.Natural
import GHC.Types (Nat)
import Data.Proxy
import GHC.TypeLits (type (+), type (-), type (<=?))
import Explain.ExplainBase
{-
import GHC.Tuple
import Seed.Boxed
import TH.PrintQ
-}


data DNat = Zero | Succ DNat
type family Add__DNat (a :: DNat) (b :: DNat) = (r :: DNat)
type instance Add__DNat Zero b = b
type instance Add__DNat (Succ d) b = Add__DNat d (Succ b)

type family ToDNatEx (prefix :: DNat) (postfix :: [Bool]) = (r :: DNat) where
    ToDNatEx prefix '[] = prefix
    ToDNatEx prefix (False ': ts) = ToDNatEx (Add__DNat prefix prefix) ts
    ToDNatEx prefix (True ': ts) = ToDNatEx (Succ (Add__DNat prefix prefix)) ts


type family Prev (a :: DNat) :: DNat where
    Prev (Succ d) = d
    --Prev Zero = Zero
type family ToDNat (postfix :: [Bool]) = (r :: DNat)
type instance ToDNat bs = Prev (ToDNatEx (Succ Zero) bs)

type family DNat2Nat (d :: DNat) = (n :: Nat) where
    DNat2Nat Zero = 0
    DNat2Nat (Succ d) = 1 + DNat2Nat d
type family Nat2DNat (d :: Nat) = (n :: DNat) where
    Nat2DNat 0 = Zero
    Nat2DNat u = Succ (Nat2DNat (u-1))

data family ListN (u :: DNat) a
data instance ListN Zero a = List0
newtype instance ListN (Succ u) a = ListN (a, ListN u a)

instance Explain [a] (ListN Zero a) where
    explain _ = []
instance Explain [a] (ListN u a) => Explain [a] (ListN (Succ u) a) where
    explain (ListN (h, ts)) = h : explain ts

newtype ListNx (u :: DNat) a = ListNx (ListN u a, [a])
instance Explain [a] (ListN u a) => Explain [a] (ListNx u a) where
    explain (ListNx (hs, ts)) = explain hs ++ ts
{-
type family ListN2LinkedTuple a where
    ListN2LinkedTuple (ListN
-}

_Nat0 = Zero
_Nat1 = Succ _Nat0
_Nat2 = Succ _Nat1
_Nat3 = Succ _Nat2


_p :: Proxy (DNat2Nat Zero) -- '_Nat3)
_p = Proxy


pr = do
    print $ toList 




