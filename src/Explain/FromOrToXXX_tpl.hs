{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Explain.FromOrToXXX_tpl
    ( def__classFromOrToXXX
    , defs__easy, _ConTName2conty_str_pair
    )
where
import Explain.ExplainBase
import TH.PrintQ
import TH.SeedUtils
import TH.Transform
import Seed.ListOps (endswith)
import Seed.MonadOps (concatTFM)
import Numeric.Natural (Natural)

{-
class OpFrom a Bool => OpToBool a where
    toBool :: a -> Bool
    toBool = from
instance OpFrom a Bool => OpToBool a where

class OpFrom [a] b => OpFromList a b where
    fromList :: [a] -> b
    fromList = from
instance OpFrom [a] b => OpFromList a b where
-}

data XXX a b

decs = [d|
    class OpFrom (XXX a b) _YYYTo => OpFromXXX a b _YYYTo where
        fromXXX :: XXX a b -> _YYYTo
        fromXXX = from
    --instance OpFrom (XXX a b) _YYYTo => OpFromXXX a b _YYYTo where
    class OpFrom _ZZZFrom (XXX a b) => OpToXXX a b _ZZZFrom where
        toXXX :: _ZZZFrom -> XXX a b
        toXXX = from
    --instance OpFrom _ZZZFrom (XXX a b) => OpToXXX a b _ZZZFrom where
    |]
decs' = [d|
    class OpFrom (XXX a b) _YYYTo => OpFromXXX a b _YYYTo where
        fromXXX :: XXX a b -> _YYYTo
    class OpFrom _ZZZFrom (XXX a b) => OpToXXX a b _ZZZFrom where
        toXXX :: _ZZZFrom -> XXX a b
    |]



decs'' = r where
    -- input: _XXX_tvbs _XXX_ty _XXX_str -- new XXX
    ns = [mkName "a"]
    _XXX_tvbs = map PlainTV ns
    _XXX_ty = apps AppT (ConT ''Maybe) $ map VarT ns
    _XXX_str = "Maybe"
    r = def__classFromOrToXXX _XXX_str _XXX_ty _XXX_tvbs

prepare_args_ :: String -> Natural -> Type -> (Type, [TyVarBndr])
prepare_args_ tvs_prefix tvs_total _XXX_con_ty = (_XXX_ty, _XXX_tvbs) where
    ns = makeNames tvs_prefix tvs_total
    _XXX_tvbs = map PlainTV ns
    _XXX_ty = apps AppT _XXX_con_ty $ map VarT ns
def__easy :: (Natural, Type, String) -> DecsQ
def__easy (tvs_total, _XXX_con_ty, _XXX_str) = decs where
    (_XXX_ty, _XXX_tvbs) = prepare_args_ "a" tvs_total _XXX_con_ty
    decs = def__classFromOrToXXX _XXX_str _XXX_ty _XXX_tvbs
defs__easy :: [(Natural, Type, String)] -> DecsQ
defs__easy = concatTFM . map def__easy

_ConTName2conty_str_pair :: Name -> (Type, String)
_ConTName2conty_str_pair n = (ConT n, nameBase n)



def__classFromOrToXXX _XXX_str _XXX_ty _XXX_tvbs = r4 where
    r0 = decs
    --r2 = fmap (replace_ClassD_Name replace2 . post_modify f1) r0
    r2 = fmap (post_modify f1) r0
    r4 = fmap (post_modify replace4 . replace_Name replace3) r2


    ---
    _old_XXX_str = "XXX"
    _old_XXX_tvs_num = 2
    --_XXX_ts = catMaybes $ map getName _XXX_tvbs
    f1 = modify_if pred1 modify1
    pred1 = findConTName__depth 2 _old_XXX_str
    modify1 = const $ _XXX_ty
    --replace2 = replace_if (\n -> "OpFromXXX" == nameBase n) modify2
    --modify2 = const $ mkName $ "OpFrom" ++ _XXX_str
    replace3 = replace_if (\n -> endswith _old_XXX_str $ nameBase n) modify3
    modify3 name = mkName $ prefix ++ _XXX_str where
        name_str = nameBase name
        len = length name_str
        prefix = take (len - length _old_XXX_str) name_str
    replace4 (ClassD _0 _1 _tvs _3 _4) = (ClassD _0 _1 tvs' _3 _4) where
        tvs' = _XXX_tvbs ++ drop _old_XXX_tvs_num _tvs
    replace4 dec = dec

pr = do
    --pprintLnQ decs
    pprintLnQ decs''
    pprintAsSrcLnQ decs''


