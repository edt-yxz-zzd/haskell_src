{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

module TH_Utils
    ( module TH_Utils_base2
    , module TH_Utils
    )
where
import TH_Utils_base2
import SeedUtils__TH
import SeedUtils (lift2, for)
import PrintQ
import Language.Haskell.TH as TH
import Language.Haskell.Syntax
import Language.Haskell.TH.Syntax -- (StrictType, VarStrictType)
import Control.Monad
-- import SeedUtils__TH
{-# LANGUAGE TemplateHaskell #-}
import Data.Maybe
import qualified Data.Map as M 
import Data.Map (Map)
import Control.Monad.Reader
import Data.List as L -- (nub)
import Data.Functor.Identity (Identity)

type Decs = [Dec]

[d|
    _test1 = typeAssertMonadReader (undefined :: RM TableTH (->) a)
    _test2 = typeAssertMonadReader (undefined :: RT TableTH ReaderT IO a)
    _testV1 = typeAssertVisit (undefined :: RM TableTH (->) Dec)
    _testV2 = typeAssertVisit (undefined :: RT TableTH ReaderT IO Dec)

    tableTH_Arr :: TableTH (RM TableTH (->))
    tableTH_Arr = default_tableTH
    tableTH_ReaderT :: (Monad m) => TableTH (ReaderT_TH m)
    tableTH_ReaderT = default_tableTH
    |]


visitBy_Arr_TH :: (VisitByTable (TableTH m) m a, m~Arr_TH)
    => TableTH m -> a -> a
visitBy_Arr_TH table a = unRM (visit a) table
replace_ClassName :: (Name -> Name) -> Decs -> Decs
replace_ClassName f decs =
    let table = tableTH__replace_ClassName f
    in  visitBy_Arr_TH table decs
tableTH__replace_ClassName
    :: (Visit_TH m, MonadReader (TableTH m) m)
    => (Name -> Name) -> TableTH m
tableTH__replace_ClassName f = let table = default_tableTH in
  case table of
    TableTH {tableDec, tablePred} -> -- table -- InstanceDec
        table {tableDec = fDec tableDec, tablePred = fPred tablePred}
    where
    fPred tablePred@TablePred {caseClassP} =
        tablePred {caseClassP = f_caseClassP caseClassP}
    f_caseClassP old (ClassP name types) = old (ClassP (f name) types)

    fDec tableDec@TableDec {caseInstanceD, caseClassD} =
        tableDec{caseClassD = f_caseClassD caseClassD
                ,caseInstanceD = f_caseInstanceD caseInstanceD
                }
    f_caseClassD old (ClassD cxt name tvs deps decs) =
        old (ClassD cxt (f name) tvs deps decs)
    f_caseInstanceD old (InstanceD cxt typ decs) =
        old (InstanceD cxt typ' decs) where
        (name, types) = unfold_inst_type typ
        typ' = call AppT (ConT $ f name) types




main = do
    let f name = if nameBase name == "C" then mkName "D" else name
    pprintLnQ $ [d|
        class C a
        instance C Int
        class X a
        instance X Int
        |] >>= return . replace_ClassName f




--}
--}
--}
--}
--}
