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

module TH_Utils_base2
    ( module TH_Utils_base
    , module TH_Utils_base2
    )
where
import TH_Utils_base
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


-- def TableTH
infoMain_TH_Q >>= def_TableMain_all
[d|
    type R2M_TH = RM TableTH
    type Arr_TH = R2M_TH (->)
    type ReaderT_TH = RT TableTH ReaderT
    type Reader_TH = ReaderT_TH Identity
    |]
$(do
    let var = VarT . mkName
        con = ConT . mkName
        r2m = var "r2m"
        rT = var "rT"
        m = var "m"
        _RM = ConT ''RM
        _RT = ConT ''RT
        -- _TableTH = con "TableTH"
        _RM_TableTH_r2m = call AppT _RM [_TableTH, r2m]
        _RT_TableTH_rT_m = call AppT _RT [_TableTH, rT, m]
        _TableTH = ConT ''TableTH
    decs1 <- def_VisitInstance_all__TH _TableTH _RM_TableTH_r2m
    decs2 <- def_VisitInstance_all__TH _TableTH _RT_TableTH_rT_m
    return $ decs1 ++ decs2
 )

{-

GHC stage restriction: instance for `Visit
                                       (RT TableTH ReaderT IO) Dec'
  is used in a top-level splice or annotation,
  and must be imported, not defined locally

[d|
    _test1 = typeAssertMonadReader (undefined :: RM TableTH (->) a)
    _test2 = typeAssertMonadReader (undefined :: RT TableTH ReaderT IO a)
    _testV1 = typeAssertVisit (undefined :: RM TableTH (->) Dec)
    _testV2 = typeAssertVisit (undefined :: RT TableTH ReaderT IO Dec)

    tableTH_Arr :: TableTH (RM TableTH (->))
    tableTH_Arr = default_tableTH
    |]

--}
--}
--}
--}
--}
