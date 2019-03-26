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


class A a b
{-
    The RHS of an associated type declaration mentions type variable `a'
      All such variables must be bound on the LHS

class A (R b) b => B b where
    type R b :: *
instance A a b => B b where
    type R b = a
-}
{-
    Not in scope: type variable `a'
class (A a b, a~R b) => B b | b -> a where
    type R b :: *
-}
type family R b :: *
class A (R b) b => B b where
instance A (R b) b => B b where
--}

class C c where
    type S c :: *
class (C c, d~S c) => D d c | c -> d
instance (C c, d~S c) => D d c



