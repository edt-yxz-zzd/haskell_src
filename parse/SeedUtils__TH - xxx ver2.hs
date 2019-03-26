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

module SeedUtils__TH
    ( module SeedUtils__TH
    , module PrintQ
    )
where
import PrintQ
import Language.Haskell.TH
import Language.Haskell.Syntax
import Control.XMonad
-- import SeedUtils__TH
{-# LANGUAGE TemplateHaskell #-}
import Data.Maybe
import Data.Map as M (Map)


class Monad m => XMonad m where
instance Monad m => XMonad m where


class Visit t a where
    visit :: MonadReader (t m) m => a -> m a

type M a m = a -> m a
type MM a m = MonadReader (Table m) m => a -> m a
type MT t m = MonadReader (Table m) m => t m
type CM m a = MonadReader (Table m) m => a



type MDec m = M Dec m
data Case_Dec m = Case_Dec
    { case_FunD :: MDec m
    , case_ClassD :: MDec m
    , case_InstanceD :: MDec m
    }
    --deriving (Eq, Ord, Show, Read)

default_cases__Dec :: XMonad m => Case_Dec m
default_cases__Dec = Case_Dec
    { case_FunD = default_case_FunD
    , case_ClassD = default_case_ClassD
    , case_InstanceD = default_case_InstanceD
    }
default_case_ClassD :: XMonad m => MDec m
default_case_ClassD (ClassD cxt name tvs deps decs) = do
    cxt' <- visit cxt
    name' <- visit name
    tvs' <- visit tvs
    deps' <- visit deps
    decs' <- visit decs
    return $ ClassD cxt' name' tvs' deps' decs'
default_case_InstanceD :: XMonad m => MDec m
default_case_InstanceD (InstanceD cxt type_ decs) = do
    cxt' <- visit cxt
    type_' <- visit type_
    decs' <- visit decs
    return $ InstanceD cxt' type_' decs'
default_case_FunD :: XMonad m => MDec m
default_case_FunD (FunD name clauses) = do
    name' <- visit name
    -- clauses' <- mapM visit clauses
    clauses' <- visit clauses
    return $ FunD name' clauses'
data Case_Name m = Case_Name { case_Name :: M Name m }
default_cases__Name :: XMonad m => Case_Name m
default_cases__Name = Case_Name default_case_Name
default_case_Name :: XMonad m => M Name m
default_case_Name = return
data Case_Clause m = Case_Clause { case_Clause :: M Clause m }
default_cases__Clause :: XMonad m => Case_Clause m
default_cases__Clause = Case_Clause default_case_Clause
default_case_Clause :: XMonad m => M Clause m
default_case_Clause = undefined
data Case_Pred m = Case_Pred
    { case_ClassP :: M Pred m
    , case_EqualP :: M Pred m
    }
default_cases__Pred :: XMonad m => Case_Pred m
default_cases__Pred = Case_Pred
    { case_ClassP = default_case_ClassP
    , case_EqualP = default_case_EqualP
    }
default_case_ClassP :: XMonad m => M Pred m
default_case_ClassP (ClassP name types) = do
    name' <- visit name
    types' <- visit types
    return $ ClassP name' types'
default_case_EqualP :: XMonad m => M Pred m
default_case_EqualP (EqualP t1 t2) = do
    t1' <- visit t1
    t2' <- visit t2
    return $ EqualP t1' t2'
data Case_Type m = Case_Type
    { case_ConT :: M Type m
    , case_AppT :: M Type m
    }
default_cases__Type :: XMonad m => Case_Type m
default_cases__Type = Case_Type
    { case_ConT = default_case_ConT
    , case_AppT = default_case_AppT
    }
default_case_ConT :: XMonad m => M Type m
default_case_ConT (ConT name) = do
    name' <- visit name
    return $ ConT name'
default_case_AppT :: XMonad m => M Type m
default_case_AppT (AppT t1 t2) = do
    t1' <- visit t1
    t2' <- visit t2
    return $ AppT t1' t2'

data Cases_TH m = Cases_TH
    { caseDec :: Case_Dec m
    , caseName :: Case_Name m
    , caseClause :: Case_Clause m
    , casePred :: Case_Pred m
    , caseType :: Case_Type m

    , caseXXXs :: Case_XXXs m
    , caseClauses :: Case_Clauses m
    , caseFunDeps :: Case_FunDeps m
    , caseDecs :: Case_Decs m
    , caseTypes :: Case_Types m
    , caseTyVarBndrs :: Case_TyVarBndrs m
    , caseCxt :: Case_Cxt m
    }

default_cases__TH :: XMonad m => Cases_TH m
default_cases__TH = Cases_TH
    { caseDec = default_cases__Dec
    , caseName = default_cases__Name
    , caseClause = default_cases__Clause
    , casePred = default_cases__Pred
    , caseType = default_cases__Type

    , caseXXXs = default_cases__XXXs
    , caseClauses = default_cases__Clauses
    , caseFunDeps = default_cases__FunDeps
    , caseDecs = default_cases__Decs
    , caseTypes = default_cases__Types
    , caseTyVarBndrs = default_cases__TyVarBndrs
    , caseCxt = default_cases__Cxt
    -- , caseDec = default_cases__Dec
    }

data XXX
type XXXs = [XXX]
data Case_XXX m = Case_XXX { case_XXX :: M XXX m }
type Clauses = [Clause]
type FunDeps = [FunDep]
type Decs = [Dec]
type Types = [Type]
type TyVarBndrs = [TyVarBndr]
{-
data Case_XXXs m = Case_XXXs { case_XXXs :: M XXXs m }
data Case_Clauses m = Case_Clauses { case_Clauses :: M Clauses m }
data Case_FunDeps m = Case_FunDeps { case_FunDeps :: M FunDeps m }
data Case_TyVarBndrs m = Case_TyVarBndrs { case_TyVarBndrs :: M TyVarBndrs m }
data Case_Decs m = Case_Decs { case_Decs :: M Decs m }
-}
data Case_Clauses m = Case_Clauses { case_Clauses :: M Clauses m }
default_cases__Clauses :: XMonad m => Case_Clauses m
default_cases__Clauses = Case_Clauses default_case_Clauses
default_case_Clauses :: XMonad m => M Clauses m
default_case_Clauses = mapM visit
data Case_FunDeps m = Case_FunDeps { case_FunDeps :: M FunDeps m }
default_cases__FunDeps :: XMonad m => Case_FunDeps m
default_cases__FunDeps = Case_FunDeps default_case_FunDeps
default_case_FunDeps :: XMonad m => M FunDeps m
default_case_FunDeps = mapM visit
data Case_TyVarBndrs m = Case_TyVarBndrs { case_TyVarBndrs :: M TyVarBndrs m }
default_cases__TyVarBndrs :: XMonad m => Case_TyVarBndrs m
default_cases__TyVarBndrs = Case_TyVarBndrs default_case_TyVarBndrs
default_case_TyVarBndrs :: XMonad m => M TyVarBndrs m
default_case_TyVarBndrs = mapM visit
data Case_Decs m = Case_Decs { case_Decs :: M Decs m }
default_cases__Decs :: XMonad m => Case_Decs m
default_cases__Decs = Case_Decs default_case_Decs
default_case_Decs :: XMonad m => M Decs m
default_case_Decs = mapM visit
data Case_Cxt m = Case_Cxt { case_Cxt :: M Cxt m }
default_cases__Cxt :: XMonad m => Case_Cxt m
default_cases__Cxt = Case_Cxt default_case_Cxt
default_case_Cxt :: XMonad m => M Cxt m
default_case_Cxt = mapM visit
data Case_Types m = Case_Types { case_Types :: M Types m }
default_cases__Types :: XMonad m => Case_Types m
default_cases__Types = Case_Types default_case_Types
default_case_Types :: XMonad m => M Types m
default_case_Types = mapM visit
data Case_XXXs m = Case_XXXs { case_XXXs :: M XXXs m }
default_cases__XXXs :: XMonad m => Case_XXXs m
default_cases__XXXs = Case_XXXs default_case_XXXs
default_case_XXXs :: XMonad m => M XXXs m
default_case_XXXs = mapM visit




instance Visit Cases_TH Dec where
    visit dec = case dec of
        ClassD _ _ _ _ _ -> case_ClassD (caseDec ?visit_table) dec
        InstanceD _ _ _ -> case_InstanceD (caseDec ?visit_table) dec
instance Visit Cases_TH Name where
    visit name = case_Name (caseName ?visit_table) name
instance Visit Cases_TH Pred where
    visit pred = case pred of
        ClassP _ _ -> case_ClassP (casePred ?visit_table) pred
        EqualP _ _ -> case_EqualP (casePred ?visit_table) pred
instance Visit Cases_TH Type where
    visit t = case t of
        ConT _ -> case_ConT (caseType ?visit_table) t
        AppT _ _ -> case_AppT (caseType ?visit_table) t
instance Visit Cases_TH [Clause] where
    visit a = case_Clauses (caseClauses ?visit_table) a
instance Visit Cases_TH [TyVarBndr] where
    visit a = case_TyVarBndrs (caseTyVarBndrs ?visit_table) a
instance Visit Cases_TH [FunDep] where
    visit a = case_FunDeps (caseFunDeps ?visit_table) a
instance Visit Cases_TH [Dec] where
    visit a = case_Decs (caseDecs ?visit_table) a
instance Visit Cases_TH [Type] where
    visit a = case_Types (caseTypes ?visit_table) a
instance Visit Cases_TH Cxt where
    visit cxt = case_Cxt (caseCxt ?visit_table) cxt
instance Visit Cases_TH [XXX] where
    visit a = case_XXXs (caseXXXs ?visit_table) a



instance Visit Cases_TH XXX where
    visit = undefined
instance Visit Cases_TH Clause where
    visit = undefined
instance Visit Cases_TH TyVarBndr where
    visit = undefined
instance Visit Cases_TH FunDep where
    visit = undefined

replaceClsBaseName__table :: (Name -> Name) -> Cases_TH m -> Cases_TH m
replaceClsBaseName__table f table = table' where
    _casePred = casePred table
    _caseDec = caseDec table
    __case_ClassP = case_ClassP _casePred
    __case_ClassD = case_ClassD _caseDec
    __case_InstanceD = case_InstanceD _caseDec
    table' = table
        { casePred = _casePred { case_ClassP = __case_ClassP' }
        , caseDec = _caseDec
            { case_ClassD = __case_ClassD'
            , case_InstanceD = __case_InstanceD'}
        }
    __case_ClassP' a = do
        ClassP name types <- __case_ClassP a
        return $ ClassP (f name) types
    __case_ClassD' a = do
        ClassD cxt name tvs deps decs <- __case_ClassD a
        return $ ClassD cxt (f name) tvs deps decs
    __case_InstanceD' a = do
        InstanceD cxt type_ decs <- __case_InstanceD a
        return $ InstanceD cxt (update_fst_name type_) decs
    update_fst_name type_ = case type_ of
        ConT name -> f name
        AppT t1 t2 -> AppT (update_fst_name t1) t2
        x -> x -- error???
replaceClsBaseName__Dec :: (Name -> Name) -> (Dec -> Dec)
replaceClsBaseName__Dec f dec = r where
    table = replaceClsBaseName__table f default_cases__TH
    r = let ?visit_table = table in case visit dec of
        Left err -> error err
        Right dec' -> dec'





















tyvarbndr2name (PlainTV n) = n
tyvarbndr2name (KindedTV n _) = n
apps f head ls = foldl f head ls


may_classD2instanceD :: Dec -> Maybe Dec
may_classD2instanceD (ClassD cxt name binds _ _) = Just $
    InstanceD cxt type_ [] where
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


