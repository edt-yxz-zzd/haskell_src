{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

import SeedUtils__TH
import SeedUtils (lift2, for)
import PrintQ
import Language.Haskell.TH as TH
import Language.Haskell.Syntax as Syntax
import Control.Monad
-- import SeedUtils__TH
{-# LANGUAGE TemplateHaskell #-}
import Data.Maybe
import qualified Data.Map as M 
import Data.Map (Map)
import Control.Monad.Reader

import TH_Utils_base hiding (main)

-- xx
infoMain_TH_Q >>= def_TableMain_all

$(
    let m = mkName "m"
        typeM = VarT m
        typeTable_tvs = AppT (ConT ''TableTH) typeM
    in  infoMain_TH_Q >>= def_VisitInstance_all typeTable_tvs typeM)
[t|RM TableTH (->)|] >>= def_VisitInstances__prime_types_TH

main = do
    pprintLnQ xx
    printLnQ [d|
        instance Visit m Int where
            visit = return
        |]
    print '[]
    print '(:)
_test1 = typeAssertMonadReader (undefined :: RM TableTH (->) a)
_test2 = typeAssertMonadReader (undefined :: RT TableTH ReaderT IO a)
_testV1 = typeAssertVisit (undefined :: RM TableTH (->) Dec)
_testV2 = typeAssertVisit (undefined :: RT TableTH ReaderT IO Dec)

tableTH_Arr :: TableTH (RM TableTH (->))
tableTH_Arr = default_tableTH

