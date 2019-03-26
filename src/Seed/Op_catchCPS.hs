{-# LANGUAGE TypeFamilies #-}

module Seed.Op_catchCPS
    (catchCPS, EE, CPS)
where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Numeric.Natural
{-
import Seed.EitherOps (swap_eEe_E_r)
import Data.List.NonEmpty
import Seed.ListOps
import Seed.PairOps
import Seed.MaybeOps
import Seed.OpSwap
-}

---------------------- CPS
--swap_eEe_E_r = undefined

type CPS r arr i o = arr o r -> arr i r
type EE e r = Either (Natural, e) r
        -- Natural: is_an_O2R_exception? 0 - means an I2O_exception
catchCPS
    :: (ArrowChoice arr, eEo ~ Either e o)
    -- => (forall x. arr x eer -> arr x (EE e r))
    -- catch
    => (arr eEo eer -> arr eEo (EE e r))
    -- catch
    -> (arr i eer -> arr i (EE e r))
    -- throw
    -> (arr (EE e r) eer)
    -> CPS eer arr i o -> CPS eer arr i (Either e o)
    -- eer - the final result
    -- EE e r
    --      the (n>0, "e") is used in catchCPS,
    --      should not use it outside catchCPS
    --      the (0, "e") is used as normal exception channel
    -- catch :: arr i eer -> arr i (EE e r)
    -- throw :: arr (EE e r) eer
    -- throw = (throw_first_channel ||| throw_second_channel) ||| id
    -- (catch i2eer) >>> throw === i2eer
    -- catch (i2neEr >>> throw) === i2neEr
catchCPS catch_eEo2eer catch_i2eer throw o2eer_to_i2eer
    = eEo2eer_to_i2eer where
        eEo2eer_to_i2eer eEo2eer' = i2eer' where
            -- lift error; inc n
            eEo2neEr = catch_eEo2eer eEo2eer'
                -- o2r err | r
            inc_n (n,e) = (succ n, e)
            dec_n (n,e) = (pred n, e)
            eEo2eer = eEo2neEr >>> left (arr inc_n) >>> throw
            -- now, eEo2eer is lifted/uncaught version of eEo2eer'
            o2eer = Right ^>> eEo2eer  -- lifted
            e2eer__I2O = Left ^>> eEo2eer' -- unlifted
            ne2eer__O2R = Left . dec_n ^>> throw -- after dec n
            r2eer = Right ^>> throw
            -- ?? e2eer__O2R = (Left . Right) ^>> throw

            --
            i2eer = o2eer_to_i2eer o2eer -- i2o unlifted; o2r lifted
            i2neEr = catch_i2eer i2eer
            -- 1) r
            -- 2) (0, e) -- i2o err
            -- 3) (n>0, e) -- o2r err

            ne2either (0, e) = Right e
            ne2either ne = Left ne
            i2eer' = i2neEr >>> left (arr ne2either)
                >>> ((ne2eer__O2R ||| e2eer__I2O) ||| r2eer)



