{-# LANGUAGE  MultiParamTypeClasses
             , FunctionalDependencies
             , FlexibleInstances
             , FlexibleContexts
             , UndecidableInstances
             #-}

newtype ID a = ID { unID :: a }
class PR p v | p -> v where
    pget :: p v -> v
instance PR ID a where
    pget = unID

p1 = pget $ ID 1
p2 = pget $ ID True

{-
    ID -> ?? Int | Bool | forall a. a ??
    UndecidableInstances ==>> forall a ??
-}
