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



module Seed.List
    ( List (..)
    , Tuple (..)
    , Empty (..)
    , ListArgs (..)
    , module Seed.List
    )
where

import Data.Kind
import Data.Typeable
import Numeric.Natural
import GHC.Types (Nat)
import Data.Proxy
import GHC.TypeLits (type (+), type (-), type (<=?))
import GHC.Tuple
import Explain.ExplainBase
import Seed.Boxed
import TH.PrintQ
--import Data.Typeable (typeOf)
--import Seed.Tuple (RPair)

data Empty a = Empty
data ListArgs (u :: Nat) a
type family GetListArgs_elem_ ls where
    GetListArgs_elem_ (Empty a) = a
    GetListArgs_elem_ (a, b) = a
type family GetListArgs_ (u :: Nat) a ls where
    GetListArgs_ u a (Empty a) = ListArgs u a
    GetListArgs_ u a (a, ts) = GetListArgs_ (1+u) a ts
type family GetListArgs ls where
    GetListArgs ls = GetListArgs_ 0 (GetListArgs_elem_ ls) ls
type family List (u :: Nat) a = r where -- | r -> u a where
    List 0 a = Empty a
    List u a = (a, List (u-1) a)
type family Tuple (ts :: [*]) = (r :: *) | r -> ts where
    Tuple '[] = ()
    Tuple (h ': ts) = (h, Tuple ts)


newtype AsTuple ls = AsTuple (Tuple ls)
instance Boxed (AsTuple ls) where
    type BoxedFrom (AsTuple ls) = Tuple ls
    box = AsTuple
    unbox (AsTuple a) = a

type E a = (Tuple '[a])
-- instance Explain () (Tuple '[])
instance Explain (Unit a) (AsTuple '[a]) where
    explain (AsTuple (a, ())) = Unit a
instance Explain (AsTuple '[a]) (Unit a) where
    explain (Unit a) = AsTuple (a, ())
instance Make (Unit a) (AsTuple '[a]) where
instance Make (AsTuple '[a]) (Unit a) where


decs =
  [d|
    instance Explain (a, b) (AsTuple '[a,b]) where
        explain (AsTuple (a, (b, ()))) = (a, b)
    instance Explain (AsTuple '[a,b]) (a, b) where
        explain (a, b) = (AsTuple (a, (b, ())))
    instance Make (a, b) (AsTuple '[a,b]) where
    instance Make (AsTuple '[a,b]) (a, b) where
    |]


pr = do
    pprintLnQ decs













--}


{-
data DNat = Zero | Succ DNat
type family Add__DNat (a :: DNat) (b :: DNat) = (r :: DNat)
type instance Add__DNat Zero b = b
type instance Add__DNat (Succ d) b = Add__DNat d (Succ b)

type family ToDNatEx (prefix :: DNat) (postfix :: [Bool]) = (r :: DNat)
type instance ToDNatEx prefix '[] = prefix
type instance ToDNatEx prefix (False ': ts) = ToDNatEx (Add__DNat prefix prefix) ts
type instance ToDNatEx prefix (True ': ts) = ToDNatEx (Succ (Add__DNat prefix prefix)) ts


type family Prev (a :: DNat) :: DNat
type instance Prev (Succ d) = d
type instance Prev Zero = Zero
type family ToDNat (postfix :: [Bool]) = (r :: DNat)
type instance ToDNat bs = Prev (ToDNatEx (Succ Zero) bs)
-}


{-
type family FMap (f :: DNat -> DNat) g
type instance FMap f (Proxy d) = Proxy (f d)
type family Nat2DNat (a :: Nat) = r  | r -> a
type family Nat2DNat_ (b :: Bool) (a :: Nat) = r | r -> a
type instance Nat2DNat_ False 0 = Proxy Zero
--type instance Nat2DNat_ True u = (u ~ (1+(u-1))) => (FMap 'Succ (Nat2DNat (u-1)))
type instance Nat2DNat_ True u = (FMap 'Succ (Nat2DNat_ (1 <=? (u-1)) (u-1)))
--type instance Nat2DNat u = Nat2DNat_ (1 <=? u) u
-}




{-
{-
lg :: Proxy base -> Proxy (base ^ pow) -> Proxy pow
lg _ _ = Proxy
-}

---
infixr 5 :/ , //

(//) :: a -> a -> List 2 a
a // b = a :/ b :/ Empty


to_pair :: (1 <= u) => List u a -> (a, List (u-1) a)
to_pair (h :/ ts) = (h, ts)

type family RPair (u :: Nat) a where
    RPair 0 a = ()
    RPair u a = (a, RPair (u-1) a)


{-

data Succ a where
    Zero :: Succ ()
    Succ :: Succ a -> Succ (Succ a)
{-
type family Succ (u :: Nat) = r | r -> u
type instance ((1+u)-1 ~ u) => Succ u = Proxy (1+u)
-}

type family RPair (u :: Nat) a
type family RPair_ (b :: Bool) (u :: Nat) a
type instance RPair_ False 0 a = ()
type instance RPair_ True u a = (a, RPair (u-1) a)
type instance RPair u a = RPair_ (1 <=? u) u a
{-
data RPair_ u a t where
--data RPair_ u a t | u a -> t where
    RPair_0 :: RPair_ 0 a ()
    RPair_1x :: a -> RPair_ u a t -> RPair_ (1+u) a (a, t)
-}

to_rpair_ :: List (1+u) a -> RPair_ True (1+u) a
--to_rpair_ :: List (1+u) a -> RPair (1+u) a
to_rpair_ (h :/ ts) = (h, to_rpair ts)
to_rpair :: List u a -> RPair u a
to_rpair Empty = ()
--to_rpair (h :/ ts) = (h, to_rpair ts)
to_rpair ls@(h :/ ts) = to_rpair_ ls

toList :: List u a -> [a]
toList Empty = []
toList (h :/ ts) = h : toList ts

class UnsafeFromList a ls | ls -> a where
    unsafe_fromList :: [a] -> ls
instance UnsafeFromList a (List 0 a) where
    unsafe_fromList = fromList_0
instance (1 <= u, (1+(u-1))~u) => UnsafeFromList a (List u a) where
    unsafe_fromList = fromList_1x
fromList_0 :: [a] -> List 0 a
fromList_1x :: (1 <= u, ((1+u)-1) ~ u) => [a] -> List u a
fromList_0 [] = Empty
fromList_0 _ = error "too many"
fromList_1x [] = error "too few"
fromList_1x (h:ts) = h :/ unsafe_fromList ts
-}




{--
data List (u :: Natural) a where
    --Empty :: List (0::Natural) a
    --Empty :: List '0 a
    --Empty :: List '(0) a
    --Empty :: List (read "0" ::Natural) a
    Empty :: List (fromIntegral 0 ::Natural) a
    (:/) :: a -> List u a -> List (fromIntegral (u+1)) a
--}
---
data List (u :: Nat) a where
    Empty :: List 0 a
    (:/) :: (((1+u)-1) ~ u, 1 <= (1+u)) => a -> List u a -> List (1 + u) a
    -- deriving (Eq, Ord, Read, Show)
--}



--}
--}
--}


