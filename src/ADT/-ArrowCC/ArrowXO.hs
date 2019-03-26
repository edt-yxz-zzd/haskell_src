{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}
-}
module ADT.ArrowXO
where


--import Control.Monad.Trans.Cont
import Control.Arrow
import Seed.ArrowOps (Arrow2Functor(..), Arrow2Applicative(..), Arrow2Monad(..))
import Seed.ArrowOps (constA)
import Prelude hiding ((.), id)
import Control.Category
{-
import qualified Control.Arrow as A
import Control.Arrow (Arrow)
--import qualified Prelude as P
import qualified Control.Category as Cat
import Control.Category (Category)
-}
import Seed.Boxed
import Seed.Utils ((...))
import Seed.EitherOps (either_merge, either_flip, bimap, isLeft)
import Seed.PairOps (pair_flip, bimap)
import Data.Semigroup
import Data.Monoid



class CategoryXO arr where
    idXO :: (ar ~ arr st r) => ar a a
    (>>>>) :: (ar ~ arr st r) => ar i x -> ar x o -> ar i o
    default idXO :: (ar ~ arr st r) => ArrowXO arr => ar a a
    idXO = arrXO id

--type ArrowXO2Arrow arr = CategoryXO2Category arr
class (CategoryXO arr, Arrow (ArrowXO2Arrow arr)) => ArrowXO arr where
    -- external_input external_output internal_input internal_output
    -- external_output -> internal_input : hole
    -- internal_input -> internal_output : solid arrow
    -- internal_output -> external_output : hole
    -- external_input == state
    -- external_output == result
    type ArrowXO2Arrow arr :: * -> * -> *
    type ArrowXO2Arrow arr = (->)

    firstXO :: (ar ~ arr st r) => ar i o -> ar (i, x) (o, x)
    arrXO :: (ar ~ arr st r) => ArrowXO2Arrow arr i o -> ar i o
    exitXO :: (ar ~ arr st r) => ArrowXO2Arrow arr st r -> ar x y
    callXO
        :: (ar ~ arr st r)
        => ((ArrowXO2Arrow arr st r -> ar x y) -> ar st r) -> ar st r
    callXO f = f exitXO
    get_stateXO :: (ar ~ arr st r) => ar x st


{-
    arrXO :: (ar ~ arr st r) => (i -> o) -> ar i o
    arrXO = arrXO_ex . arr
    exitXO :: (ar ~ arr st r) => (st->r) -> ar x y
    exitXO = exitXO_ex . arrXO

    switchXO :: (ar ~ arr st r) => ar i o -> arr i o st r
    mapXO
        :: (ar ~ arr st r) => (ArrowXO2Arrow arr i x -> ArrowXO2Arrow arr i o)
        -> (ar i x -> ar i o)
    discardXO :: (ar ~ arr st r) => ar i o -> ar i i
    discardXO = mapXO discardA
    discardXO_ex :: (ar ~ arr st r) => ar i ox -> ar i o -> ar i o
    discardXO_ex arr = (discardXO arr >>>>)


    runXO_ex :: (ar ~ arr st r) => ar o r -> ar i o -> ArrowXO2Arrow arr i r
    runXO_ex o2r i2o = runXO $ i2o >>>> o2r
    runXO :: (ar ~ arr st r) => ar i r -> ArrowXO2Arrow arr i r
    runXO = runXO_ex idXO
    runXO_exR :: (ar ~ arr st r) => arr o r o -> ar i o -> ArrowXO2Arrow arr i o
    runXO_exR r2o i2o = runXO_ex r2o $ switchXO i2o
    rename_resultXO :: (ar ~ arr st r) => arr o i o -> ar i o
    rename_resultXO = arrXO_ex . runXO

    {-# MINIMAL ((>>>>), firstXO, arrXO_ex, exitXO_ex
                , switchXO, mapXO, (runXO_ex | runXO)) #-}
-}






newtype ArrCPS arr st r i o =
    ArrCPS {runArrCPS :: arr o r -> arr i r}

chainCPS
    :: (Category arr, ar ~ ArrCPS arr st r) => ar i x -> ar x o -> ar i o
chainCPS (ArrCPS x2r_to_i2r) (ArrCPS o2r_to_x2r)
    = ArrCPS $ x2r_to_i2r . o2r_to_x2r
idCPS :: Category arr => ArrCPS arr st r a a
--idCPS = ArrCPS (>>>)
idCPS = ArrCPS id
exitCPS :: (Arrow arr, ar ~ ArrCPS arr st r) => r -> ar x y
exitCPS r = ArrCPS y2r_to_x2r where
    y2r_to_x2r y2r = constA r

firstCPS
    :: (ArrowApply arr, ar ~ ArrCPS arr st r) => ar i o -> ar (i, x) (o, x)
firstCPS (ArrCPS o2r_to_i2r) = ArrCPS ox2r_to_ix2r where
    ox2r_to_ix2r ox2r = ix2r where
        ix2r = ix2ox >>> ox2r
        ix2ox = 
--
--}
--}
--}
--}
--}
