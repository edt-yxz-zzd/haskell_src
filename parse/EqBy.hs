{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
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

import Boxed
import Data.List ((\\))

infix 4 ==|
class New by => EqBy by a where
    (==|) :: by a -> by a -> Bool
    -- object address "is"
    -- structure "same": [a] ==
    -- value "equal":
    --    [a] as set: lhs \\ rhs == [] && rhs \\ lhs == []
    --    projection: (a,b) as snd: snd lhs == snd rhs
    default (==|) :: OpEqKey (by a) => by a -> by a -> Bool
    a ==| b = eq_key a == eq_key b


class Eq (EqKey a) => OpEqKey a where
    type EqKey a :: *
    eq_key :: a -> EqKey a
    default eq_key :: (Eq a, a ~ EqKey a) => a -> EqKey a
    eq_key = id
class (EqBy by a, OpEqKey (by a)) => EqByKey by a
instance (EqBy by a, OpEqKey (by a)) => EqByKey by a

newtype ID a = ID { unID :: a }
instance New ID where
    wrap = ID
    unwrap = unID
newtype AsSet a = AsSet { unAsSet :: a }
instance New AsSet where
    wrap = AsSet
    unwrap = unAsSet
newtype Snd a = Snd { unSnd :: a }
instance New Snd where
    wrap = Snd
    unwrap = unSnd


liftN2' f a b = f (unwrap a) (unwrap b)
instance Eq a => EqBy ID a where
    (==|) = liftN2' (==)
instance Eq a => EqBy AsSet [a] where
    (==|) = liftN2' (===) where
        lhs === rhs = lhs \\ rhs == [] && [] == rhs \\ lhs
instance Eq b => EqBy Snd (a, b) where
    (==|) = liftN2' (===) where
        lhs === rhs = snd lhs == snd rhs



