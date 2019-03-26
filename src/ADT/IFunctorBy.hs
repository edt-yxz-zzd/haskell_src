

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}


{-
    why "by"?
    to implement 
        class ArrowCC arr where
            -- Arrow (arr r) => arr r i o
        Arrow arr => Arrow (ArrowT arr)
        ArrowO arr
            -- Arrow (arr ex_i ex_o)
-}
module ADT.IFunctorBy
where


import Seed.Boxed
import Seed.BoxedBy
import Seed.ProxyOps
class (IFunctorBy by (GetFunctorBy by a)
    , UnboxFunctorBy by (GetFunctorBy by a) (GetFunctorValueBy by a) ~ a
    ) => IBoxFunctorBy by a where
    type GetFunctorBy by a :: * -> *
    type GetFunctorValueBy by a

class Functor f => IFunctorBy by f where
    --type UnboxFunctorBy by f v = r | by r -> f v
    -- GetFunctorBy by (UnboxFunctorBy by f v) === f
    type UnboxFunctorBy by f v
    unboxFunctorBy
        :: IFunctorByEx by f v fv => proxy by -> f v -> fv
    boxFunctorBy
        :: IFunctorByEx by f v fv => proxy by -> fv -> f v
    fmapBy
        :: (IFunctorByEx by f v fv, IFunctorByEx by f u fu)
        => proxy by -> (v->u) -> fv -> fu
    fmapBy p f = unboxFunctorBy p . fmap f . boxFunctorBy p


class (IFunctorBy by f, IBoxFunctorBy by fv
    , fv ~ UnboxFunctorBy by f v
    , f ~ GetFunctorBy by fv
    , v ~ GetFunctorValueBy by fv
    ) => IFunctorByEx by f v fv | by f v -> fv, by fv -> f v

instance (IFunctorBy by f, IBoxFunctorBy by fv
    , fv ~ UnboxFunctorBy by f v
    , f ~ GetFunctorBy by fv
    , v ~ GetFunctorValueBy by fv
    ) => IFunctorByEx by f v fv

data FunctorBy by
proxy_castFunctorBy :: proxy by -> Proxy (FunctorBy by)
proxy_castFunctorBy = appP (Proxy :: Proxy FunctorBy)
instance IFunctorByEx by f v fv
    => BoxedBy (FunctorBy by) fv (f v) where
    boxBy = boxFunctorBy . last1P
    unboxBy = unboxFunctorBy . last1P







data ByFst = ByFst
newtype FlipPair b a = FlipPair (a,b)
instance Boxed (FlipPair b a) where
    type BoxedFrom (FlipPair b a) = (a,b)
    box = FlipPair
    unbox (FlipPair p) = p
instance Functor (FlipPair b) where
    fmap f (FlipPair (a,b)) = FlipPair (f a, b)
instance IFunctorBy ByFst (FlipPair b) where
    type UnboxFunctorBy ByFst (FlipPair b) a = (a,b)
    unboxFunctorBy _ = unbox
    boxFunctorBy _ = box
instance IBoxFunctorBy ByFst (a, b) where
    type GetFunctorBy ByFst (a, b) = FlipPair b
    type GetFunctorValueBy ByFst (a, b) = a

fmapByFst
        :: (IFunctorByEx by f v fv, IFunctorByEx by f u fu, by ~ ByFst)
        => (v->u) -> (fv -> fu)
fmapByFst = fmapBy (toProxy ByFst)
a = fmapByFst (=='a') ('c', "s") == (False, "s")
pr = do
    print a



