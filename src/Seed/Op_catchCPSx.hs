{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}
module Seed.Op_catchCPSx
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

---------------------- CPS; an error implement!!!!!!!
--swap_eEe_E_r = undefined

type CPS env r arr i o = arr (env,o) r -> arr (env,i) r
type EE e r = Either (Natural, e) r
        -- Natural: is_an_O2R_exception? 0 - means an I2O_exception
catchCPS
    :: forall env arr r e i o eEo neEr eer
    . (ArrowChoice arr, eEo ~ Either e o, neEr ~ EE e r)
    -- catch
    => (forall i vi. (vi ~ (env, i)) => arr vi eer -> arr vi neEr)
    -- throw
    -> (arr neEr eer)
    -> CPS env eer arr i o -> CPS env eer arr i eEo
    -- eer - the final result
    -- EE e r
    --      the (n>0, "e") is used in catchCPS,
    --      should not use it outside catchCPS
    --      the (0, "e") is used as normal exception channel
    -- catch :: arr vi eer -> arr vi (EE e r)
    -- throw :: arr (EE e r) eer
    -- throw = (throw_first_channel ||| throw_second_channel) ||| id
    -- (catch vi2eer) >>> throw === vi2eer
    -- catch (vi2neEr >>> throw) === vi2neEr
catchCPS catch throw vo2eer_to_vi2eer
    = v_eEo2eer_to_vi2eer where
        v_eEo2eer_to_vi2eer v_eEo2eer' = vi2eer' where
            -- lift error; inc n
            v_eEo2neEr = catch v_eEo2eer'
                -- o2r err | r
            inc_n (n,e) = (succ n, e)
            dec_n (n,e) = (pred n, e)
            v_eEo2eer = v_eEo2neEr >>> left (arr inc_n) >>> throw
            -- now, v_eEo2eer is lifted/uncaught version of v_eEo2eer'
            vo2eer = fmap Right ^>> v_eEo2eer  -- lifted
            ve2eer__I2O = fmap Left ^>> v_eEo2eer' -- unlifted
            ne2eer__O2R = Left . dec_n ^>> throw -- after dec n
            r2eer = Right ^>> throw

            --
            vi2eer = vo2eer_to_vi2eer vo2eer -- i2o unlifted; o2r lifted
            vi2neEr = catch vi2eer
            -- 1) r
            -- 2) (0, e) -- i2o err
            -- 3) (n>0, e) -- o2r err

            vi2neEr_v = vi2neEr &&& arr fst
            neEr_v__to__neEveEr (Right r, _) = Right r
            neEr_v__to__neEveEr (Left (0, e), v) = Left (Right (v,e))
            neEr_v__to__neEveEr (Left ne, _) = Left (Left ne)

            vi2neEveEr = (vi2neEr &&& arr fst) >>> arr neEr_v__to__neEveEr
            vi2eer' = vi2neEveEr >>> ((ne2eer__O2R ||| ve2eer__I2O) ||| r2eer)



