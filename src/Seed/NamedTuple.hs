

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

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}


{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Seed.NamedTuple
    ( module Seed.NamedTuple
    , module Seed.Kind
    , module Data.Kind
    , module Data.Proxy
    )
where

import Data.Proxy
import Data.Kind
import GHC.TypeLits hiding (type (*))
import Seed.Boxed
import Seed.Kind

class (NoDuplicateKey attrs, HasKey key attrs
    , isfirst ~ IsFirstKey key attrs)
    => AccessAttrBase (isfirst :: Bool) (attrs :: Dict k *) (key :: k)
       -- | attrs key -> isfirst
        where
    getattrP_
        :: proxy' attrs -> proxy key
        -> Dict2LinkedVals attrs
        -> LookupE key attrs
    setattrP_, setattrP_'
        :: proxy' attrs -> proxy key
        -> LookupE key attrs
        -> Dict2LinkedVals attrs
        -> Dict2LinkedVals attrs
    getattrP_ p = getattrP . _getattrP__proxy_cast_P_ p
    setattrP_ p = setattrP . _getattrP__proxy_cast_P_ p
    setattrP_' p = setattrP' . _getattrP__proxy_cast_P_ p
    getattrP
        :: proxy (proxy' attrs, proxy'' key)
        -> Dict2LinkedVals attrs
        -> LookupE key attrs
    setattrP, setattrP'
        :: proxy (proxy' attrs, proxy'' key)
        -> LookupE key attrs
        -> Dict2LinkedVals attrs
        -> Dict2LinkedVals attrs
    setattrP p a = fmapattrP p $ const a
    setattrP' p a = fmapattrP' p $ const a
    fmapattrP, fmapattrP'
        :: proxy (proxy' attrs, proxy'' key)
        -> (LookupE key attrs -> LookupE key attrs)
        -> Dict2LinkedVals attrs
        -> Dict2LinkedVals attrs

    removeattrP, removeattrP'
        :: proxy (proxy' attrs, proxy'' key)
        -> Dict2LinkedVals attrs
        -> (LookupE key attrs, Dict2LinkedVals (RemoveFirstItem key attrs))
    addattrP, addattrP'
        :: proxy (proxy' attrs, proxy'' key)
        -> LookupE key attrs
        -> Dict2LinkedVals (RemoveFirstItem key attrs)
        -> Dict2LinkedVals attrs


_getattrP__proxy_cast_P_
    :: proxy attrs -> proxy' key -> Proxy (Proxy attrs, Proxy key)
_getattrP__proxy_cast_P_ _ _ = Proxy
_getattrP__proxy_cast_P
    :: proxy (proxy' (h ': ts), proxy'' key) -> Proxy (Proxy ts, proxy'' key)
_getattrP__proxy_cast_P _ = Proxy
instance (attrs ~ (key ':> x ': ts)
    , NoDuplicateKey attrs, HasKey key attrs)
    => AccessAttrBase 'True (key ':> x ': ts) key where
    getattrP _ = fst
    fmapattrP _ f (h, ts) = (f h, ts)
    fmapattrP' _ f (h, ts) = seq h' r where
        h' = f h
        r = (h', ts)
    removeattrP _ = id
    removeattrP' _ = id
    addattrP _ = (,)
    addattrP' _ h ts = seq r r where
        r = (h,ts)
instance (attrs ~ (y ':> x ': ts)
    , NoDuplicateKey attrs, HasKey key attrs
    , 'False ~ IsFirstKey key attrs
    , LookupE key ts ~ LookupE key (y ':> x ': ts)
    , AccessAttrBase (IsFirst key (Dict2Keys ts)) ts key
    , Dict2LinkedVals (RemoveFirstItem key attrs)
        ~ (x, Dict2LinkedVals (RemoveFirstItem key ts))
    , RemoveFirstItem key attrs ~ (y ':> x ': RemoveFirstItem key ts)
    )
    => AccessAttrBase 'False (y ':> x ': ts) key where
    getattrP p ls = getattrP (_getattrP__proxy_cast_P p) (snd ls)
    fmapattrP p f (h, ts) = (h, fmapattrP (_getattrP__proxy_cast_P p) f ts)
    fmapattrP' p f (h, ts) = seq ts' r where
        r = (h, ts')
        ts' = fmapattrP' (_getattrP__proxy_cast_P p) f ts
    removeattrP p (h, ts) = (v, (h, ts')) where
        (v, ts') = removeattrP (_getattrP__proxy_cast_P p) ts
    removeattrP' p (h, ts) = seq ts' $ seq ls' r where
        r = (v, ls')
        ls' = (h, ts')
        (v, ts') = removeattrP' (_getattrP__proxy_cast_P p) ts
    addattrP p v (h, ts) = (h, addattrP p' v ts) where
        p' = _getattrP__proxy_cast_P p
    addattrP' p v (h, ts) = seq ts' r where
        r = (h, ts')
        ts' = addattrP' p' v ts
        p' = _getattrP__proxy_cast_P p

class AccessAttrBase (IsFirstKey key attrs) attrs key
    => AccessAttr attrs key where
instance AccessAttrBase (IsFirstKey key attrs) attrs key
    => AccessAttr attrs key where

newtype NamedTuple (attrs :: Dict k *)
    = NamedTuple {unNamedTuple :: Dict2LinkedVals attrs}
instance Boxed (NamedTuple attrs) where
    type BoxedFrom (NamedTuple attrs) = Dict2LinkedVals attrs
    box = NamedTuple
    unbox = unNamedTuple

getattr
    :: AccessAttr attrs key
    => proxy key -> NamedTuple attrs -> LookupE key attrs
setattr, setattr'
    :: AccessAttr attrs key
    => proxy key -> LookupE key attrs
    -> NamedTuple attrs -> NamedTuple attrs
fmapattr, fmapattr'
    :: AccessAttr attrs key => proxy key
    -> (LookupE key attrs -> LookupE key attrs)
    -> (NamedTuple attrs -> NamedTuple attrs)
removeattr, removeattr'
    :: AccessAttr attrs key
    => proxy key -> NamedTuple attrs
    -> (LookupE key attrs, NamedTuple (RemoveFirstItem key attrs))
addattr, addattr'
    :: AccessAttr attrs key
    => proxy key -> LookupE key attrs
    -> NamedTuple (RemoveFirstItem key attrs) -> NamedTuple attrs
getattr p t = getattrP (_getattr__proxy_cast_key p t) (unbox t)
setattr p = fmapattr p . const
setattr' p = fmapattr' p . const
fmapattr p f t = box $ fmapattrP (_getattr__proxy_cast_key p t) f (unbox t)
fmapattr' p f t = mapGBox (fmapattrP' (_getattr__proxy_cast_key p t) f) t
removeattr p t = fmap box $
    removeattrP (_getattr__proxy_cast_key p t) (unbox t)
removeattr' p t = fmap box $
    removeattrP' (_getattr__proxy_cast_key p t) (unbox t)
addattr p v t = r where
    r = box $ addattrP (_getattr__proxy_cast_key p r) v (unbox t)
addattr' p v t = r where
    r = box $ addattrP' (_getattr__proxy_cast_key p r) v (unbox t)
_getattr__proxy_cast_key
    :: proxy key -> NamedTuple attrs -> Proxy (Proxy attrs, Proxy key)
_getattr__proxy_cast_key _ _ = Proxy




