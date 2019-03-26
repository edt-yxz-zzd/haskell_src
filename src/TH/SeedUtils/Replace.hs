{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


module TH.SeedUtils.Replace
where

import TH.Transform (gmapT, gdirect_transform, post_modify, GenericT)
import TH.SeedUtils.SeedUtils


replace_if :: (a -> Bool) -> (a->a) -> (a->a)
replace_if pred f a = if pred a then f a else a

replace_Name :: (Name -> Name) -> GenericT
replace_Name = post_modify
replace_Dec :: (Dec -> Dec) -> GenericT
replace_ClassD :: (Dec -> Dec) -> GenericT
replace_Dec = post_modify
replace_ClassD = replace_Dec . replace_if is_ClassD
is_ClassD :: Dec -> Bool
is_ClassD (ClassD _ _ _ _ _) = True
is_ClassD _ = False


replace_ClassD_Name :: (Name -> Name) -> GenericT
replace_ClassD_Name f = replace_ClassD f' where
    f' = gmapT (gdirect_transform f)

{-
replace_ClassD_Name :: (Name -> Bool) -> (Name -> Name) -> GenericT
replace_ClassD_Name pred f = post_modify f1 where
    f1 = modify_if pred1 modify1
    pred1 (ClassD _Cxt _Name _TyVarBndrs _FunDeps _Decs) = pred _Name
    pred1 _ = False
    modify1 = gmapT (gdirect_transform f)
-}

