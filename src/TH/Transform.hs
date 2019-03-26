{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

--  See http://www.haskell.org/haskellwiki/Research_papers/Generics#Scrap_your_boilerplate.21
module TH.Transform
    ( module TH.Transform
    , gmapT
    , typeOf, cast
    )
where
import Data.Typeable
import Data.Data
import Language.Haskell.TH
import Language.Haskell.Syntax
import Data.Proxy

one_layer_traversal, everywhere_bottomup, everywhere_topdown
    :: GenericT -> GenericT
one_layer_traversal = gmapT
-- f == direct_transform
everywhere_bottomup f = f . one_layer_traversal (everywhere_bottomup f)
everywhere_topdown f = one_layer_traversal (everywhere_topdown f) . f


gdirect_transform :: Data a => (a->a) -> GenericT
--gdirect_transform = flip direct_transform_override id
gdirect_transform a2a = direct_transform_override a2a id
direct_transform_override :: Data a => (a->a) -> GenericT
    -> (forall b. Data b => b -> b)
direct_transform_override a2a default_b2b b = case cast b of
    Just a -> case cast $ a2a a of
        Just b' -> b'
        _ -> error "logic error"
    _ -> default_b2b b


type Transform a = a -> a
type GenericT = forall a. Data a => Transform a
pre_modify :: Data a => (a->a) -> GenericT
post_modify :: Data a => (a->a) -> GenericT
pre_modify a2a = everywhere_topdown (gdirect_transform a2a)
post_modify a2a = everywhere_bottomup (gdirect_transform a2a)

modify_if :: (a -> Bool) -> Transform a -> Transform a
--modify_if :: Data a => (a->Bool) -> GenericT
modify_if pred f a = if pred a then f a else a




{-
class VTable cxt a where
    -- type VTableType cxt a
    -- vtable :: proxy (cxt a) -> Maybe (VTableType cxt a)
    -- vtable :: Maybe (cxt a)
    vtable :: cxt a


{-
instance {-# OVERLAPPABLE #-} VTable cxt a where
    --type VTableType cxt a = ()
    vtable = Nothing

get_vtable :: VTable cxt a => proxy cxt -> Maybe (cxt a)
get_vtable _ = vtable


class (Data a, VTable cxt a) => DataGMapT cxt a where
    direct_transform :: Proxy cxt -> a -> a

direct_transform' :: DataGMapT cxt a => Proxy (cxt :: * -> *) -> a -> a
direct_transform' = direct_transform
-}

gmapT_ex
    :: (forall b. Data b => Proxy (cxt :: * -> *) -> b -> b)
    -> (forall a. Data a => Proxy cxt -> a -> a)
gmapT_ex f cxt a = gmapT (f cxt) a



data ID a = ID a
get_cxt :: cxt a -> Proxy cxt
get_cxt _ = Proxy


{-
gf :: proxy cxt -> (forall a. cxt a -> a -> a) -> (forall a. Data a => a->a)
gf proxy f = if defined proxy then f vtable else id


{-
class (Data a, Typeable a, VTable (cxt a)) => DataEx cxt a where
    gmapT_ex :: proxy cxt -> (forall x. DataEx cxt x => x -> x) -> a -> a
instance (Data a, Typeable a, VTable (cxt a)) => DataEx cxt a where
    gmapT_ex proxy f = gmapT f' where
        f' :: Data x => x -> x
        f' = tDataEx2tData proxy f
tDataEx2tData
    :: proxy cxt -> (forall x. DataEx cxt x => x -> x)
    -> (forall y. Data y => y -> y)
-- tDataEx2tData _ = id
tDataEx2tData _ f y = case cast y of
    Just x -> case cast $ f x of
        Just y' -> y'
        _ -> error "logic error"
    _ -> y


{-
gf :: Typeable a => (forall b. (DataEx cxt b) => b -> b) -> (a->a)
gf f a = case cast a of
    Just b -> case cast $ f b of
        Just a' -> a'
        _ -> error "logic error"
    _ -> a
{-
gdirect_transform
    :: () -- Typeable cxt
    => (forall b. (VTable cxt b, Data b) => b -> b)
    -> (forall a. Data a => a -> a)
gdirect_transform f a = case cast a of
    Just b -> case cast $ f b of
        Just a' -> a'
        _ -> error "logic error"
    _ -> a
one_layer_traversal_cxt, direct_transform
    :: Typeable cxt
    => Data a => (forall b. Data (cxt b) => b -> b) -> a -> a
one_layer_traversal_cxt = undefined
direct_transform = undefined
direct_transform = gdirect_transform
one_layer_traversal_cxt = one_layer_traversal . gdirect_transform


--}
--}
--}
--}
--}
--}
--}

