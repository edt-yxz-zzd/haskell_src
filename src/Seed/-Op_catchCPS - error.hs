{-# LANGUAGE TypeFamilies #-}

module Seed.Op_catchCPS
    (catchCPS, EE, CPS)
where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Seed.EitherOps (swap_eEe_E_r)
{-
import Seed.ListOps
import Seed.PairOps
import Seed.MaybeOps
import Numeric.Natural
import Seed.OpSwap
-}

---------------------- CPS; an error implement!!!!!!!
--swap_eEe_E_r = undefined

type CPS r arr i o = arr o r -> arr i r
type EE e r = Either (Either e e) r
--type EE e r = Either e (Either e r)
catchCPS
    :: (ArrowChoice arr, eEo ~ Either e o)
    -- => (forall x. arr x eer -> arr x (EE e r))
    => (arr eEo eer -> arr eEo (EE e r))
    -> (arr i eer -> arr i (EE e r))
    -> (arr (EE e r) eer)
    -> CPS eer arr i o -> CPS eer arr i (Either e o)
    -- EE e r
    --      the first "e" is used in catchCPS,
    --      should not use it outside catchCPS
    --      the second "e" is used as normal exception channel
    -- catch :: arr i eer -> arr i (EE e r)
    -- throw :: arr (EE e r) eer
    -- throw = (throw_first_channel ||| throw_second_channel) ||| id
    -- (catch i2eer) >>> throw === i2eer
    -- catch (i2eEeEr >>> throw) === i2eEeEr
catchCPS catch_eEo2eer catch_i2eer throw o2eer_to_i2eer
    = eEo2eer_to_i2eer where
        eEo2eer_to_i2eer eEo2eer' = i2eer' where
            -- swap error; assume original arr has no error
            eEo2eEeEr = catch_eEo2eer eEo2eer'
                -- org o2r err | cps o2r err | r
            swap_err = swap_eEe_E_r
            --swap_err = swap_e_E_eEr
            eEo2eer = eEo2eEeEr >>> arr swap_err >>> throw
            -- now, eEo2eer is swapped version of eEo2eer'
            o2eer = Right ^>> eEo2eer  -- e2eer__I2O is another channel
            e2eer__I2O = Left ^>> eEo2eer' -- cps catched; unswapped version

            --
            i2eer = o2eer_to_i2eer o2eer
            i2eEeEr = catch_i2eer i2eer
            -- 1) org i2o err | cps i2o err | r
            -- 2) cps o2r err | org o2r err | r
            -- assume original arr has no error
            -- ==>> cps o2r err | cps i2o err | r
            -- indeed, "cps i2o err" will skip "org o2r err"
            --          "org i2o err" should be lifted to "cps o2r err"
            --  so, even original arr throw error, it is fine
            -- ERROR: e2eer__O2R = (Left . Left) ^>> throw
            --  since eer is the final result
            --  we should shift the first exception to second channel
            --  "cps o2r err" should not be caught before itself
            --      so, put this err in first channel
            --      but, when treated as a part of whole "i2eer"
            --      it should be caught, put it in second channel
            e2eer__O2R = (Left . Right) ^>> throw
            --e2eer__O2R = (Right . Left) ^>> throw
                -- unhandled cps err will be throwed to arr
            r2eer = Right ^>> throw
            --r2eer = (Right . Right) ^>> throw
            i2eer' = i2eEeEr >>> ((e2eer__O2R ||| e2eer__I2O) ||| r2eer)
            --i2eer' = i2eEeEr >>> (e2eer__O2R ||| (e2eer__I2O ||| r2eer))



