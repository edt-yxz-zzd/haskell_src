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
{-# LANGUAGE DeriveDataTypeable #-}



module ListDefs.List
    ( List (..)
    --, Tuple (..)
    , Empty (..)
    , ListArgs (..)
    , module ListDefs.List
    )
where

--import Data.Kind
import Data.Typeable
import Data.Data
import Numeric.Natural
import GHC.Types (Nat)
import Data.Proxy
import GHC.TypeLits (type (+), type (-), type (<=?), type (<=))
import GHC.Tuple
import Explain.ExplainBase
import Seed.Boxed
import TH.PrintQ
import TH.SeedUtils
import TH.Transform
import Control.Monad
--import Data.Typeable (typeOf)
--import Seed.Tuple (RPair)

data Empty a = Empty
    deriving (Eq, Ord, Read, Show, Typeable, Data)
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


{-
newtype AsArray u a = AsArray (List u a)
--instance Explain [a] (List u a) where
instance Explain [a] (AsArray 0 a) where
    explain (AsArray Empty) = []
instance {-# OVERLAPS #-} (1 <= u) => Explain [a] (AsArray u a) where
    explain (AsArray (h, ts)) = h : explain (AsArray ts)
-}









--}


