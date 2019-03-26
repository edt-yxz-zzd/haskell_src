
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , Rank2Types
            , KindSignatures
            , ScopedTypeVariables #-}
            -- 

type T a b = (a, b)
newtype N a b = N (a, b)
class New b where
    wrap :: a -> b a

{-
-- "type" fail!
instance New (T a) where
    wrap b = (undefined, b)
-}
-- "newtype" success
instance New (N a) where
    wrap b = N (undefined, b)


class Boxed via o bo | bo -> via o, via o -> bo where
    box :: o -> bo

newtype A s m a = A (m a)
-- note that (A s m a) ~ (m a), not in New class, but only Boxed


{- conflict with instance Boxed (A s m a) (m a) (A s m a)
instance New b => Boxed (b o) o (b o) where
    box = wrap
instance Boxed (A s m a) (m a) (A s m a) where
    box = A
-}



{-
data ViaNew (b :: * -> *)
data ViaA (aa :: * -> (* -> *) -> * -> *) s (m :: * -> *) a
instance New b => Boxed (ViaNew b) o (b o) where
    box = wrap
instance Boxed (ViaA A s m a) (m a) (A s m a) where
    box = A
-}












