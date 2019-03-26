{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

module TH.SeedUtils.SeedUtils
    ( apps -- module TH.SeedUtils
    , def__instances
    , decQ2decsQ, decsQ_add
    , is_nameBase, name2exp, isConName, caseName
    , module TH.PrintQ
    , module TH
    , module Syntax
    , module Control.Monad
    )
where
import TH.PrintQ
import Language.Haskell.TH as TH
import Language.Haskell.Syntax as Syntax
import Control.Monad
-- import SeedUtils__TH
{-# LANGUAGE TemplateHaskell #-}
import Data.List (foldl')
import Data.Maybe
import Data.Map as M (Map)
import Control.Monad.Reader
import Seed.Utils (caseBool)
import Data.Char (isUpper)









tyvarbndr2name (PlainTV n) = n
tyvarbndr2name (KindedTV n _) = n
apps f head ls = foldl' f head ls


may_classD2instanceD :: Dec -> Maybe Dec
may_classD2instanceD (ClassD cxt name binds _ _) = Just $
    InstanceD Nothing cxt type_ [] where
    type_ = apps AppT (ConT name) $ map (VarT . tyvarbndr2name) binds
may_classD2instanceD _ = Nothing
def__instances :: DecsQ -> DecsQ
def__instances = fmap f where
    f decs = catMaybes (map may_classD2instanceD decs) ++ decs

decQ2decsQ :: DecQ -> DecsQ
decQ2decsQ = (>>= return . return)
decQs2decsQ :: [DecQ] -> DecsQ
decQs2decsQ = sequence
decsQ_add :: DecsQ -> DecsQ -> DecsQ
decsQ_add = liftM2 (++)

{-
str2var_ :: String -> Type
str2var_ = VarT . mkName
str2ctor_ :: String -> Type
str2ctor_ = ConT . mkName
str2ctor :: String -> TypeQ
str2ctor = return . ConT . mkName
name2ctor :: Name -> TypeQ
name2ctor = return . ConT

(~+) :: String -> String -> Name
a ~+ b = mkName $ a ++ b
(^^+) :: String -> String -> Type
a ^^+ b = ConT $ a ~+ b
(^^*) :: String -> String -> Type
a ^^* b = VarT $ a ~+ b
--}


{-
hsDecl2thDec :: HsDecl -> Dec
hsName2thName :: HsName -> Name
hsType2thType :: HsType -> Type
-}



---------------


is_nameBase :: String -> Name -> Bool
is_nameBase s n = s == nameBase n
isConName :: Name -> Bool
isConName n = (not $ null s) && isUpper (head s) where
    s = nameBase n
caseName :: (Name -> a) -> (Name -> a) -> (Name -> a)
caseName = caseBool isConName
name2exp :: Name -> Exp
name2exp = caseName ConE VarE


