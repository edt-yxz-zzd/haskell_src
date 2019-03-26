{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

-}

module Ordering where
import Prelude hiding (Ordering(..))
import qualified Prelude as P
import Boxed
import SeedUtils (lift2)
import Language.Haskell.TH
{-# LANGUAGE TemplateHaskell #-}
import Explain
import Explain__FromOrToXXX
{-
instance SetOrd a where
    (|<=|) = (||<=||)
    (|<|) = (||<||)
-}
-- -}

{-
instance (Eq a, EqIsSetEq a) => SetEq a where
    (|==|) = (==)
instance OrdIsTotalSetOrd a => SetOrd a where
    (|<|) = (<)
    (|<=|) = (<=)
    set_compare a = ord2pord . compare a
-}

newtype SetWrapper a = SetWrapper a
    deriving (Show, Read)
instance New SetWrapper where
    wrap = SetWrapper
    unwrap (SetWrapper a) = a
instance SetEq a => Eq (SetWrapper a) where
    (==) = lift2 unwrap (|==|)
instance SetEq a => EqIsSetEq (SetWrapper a)
    -- ==>> SetEq (SetWrapper a)
instance SetEq a => SetEq (SetWrapper a)
instance SetOrd a => SetOrd (SetWrapper a) where
    -- SetOrd a ==>> SetEq a
    -- ==>> Eq (SetWrapper a)
    -- ==>> EqIsSetEq (SetWrapper a)
    -- ==>> SetEq (SetWrapper a)
    -- +SetOrd ==>> PartialOrd (SetWrapper a)
    (|<|) = lift2 unwrap (|<|)
    (|<=|) = lift2 unwrap (|<=|)
    type SetCmpExResult (SetWrapper a) = SetCmpExResult a
    set_compare_ex = lift2 unwrap set_compare_ex
    set_compare = lift2 unwrap set_compare
instance OrdIsTotalSetOrd a => Ord (SetWrapper a) where
    (<) = lift2 unwrap (|<|)
    (<=) = lift2 unwrap (|<=|)
instance OrdIsTotalSetOrd a => OrdIsTotalSetOrd (SetWrapper a) where
























------------- instance Bool
-- False = {}, True = {{}}
instance EqIsSetEq Bool
instance SetEq Bool
instance SetOrd Bool where
    type SetCmpExResult Bool = P.Ordering
    (|<=|) = (<=)
    (|<|) = (<)
    set_compare_ex = compare
    set_compare a = ord2pord . compare a
instance OrdIsTotalSetOrd Bool
instance TotalSetOrd Bool

