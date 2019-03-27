
{-# LANGUAGE TypeFamilies #-}

module Configure4LFT4StreamingAlgorithm_EngelExpansion
    (Configure4LFT4StreamingAlgorithm_EngelExpansion(..)
    )
where

import ZipExceptLast (zip_except_last)
import EngelExpansion
import Interval
import LinearFractionalTransformation
import Configure4LFT4StreamingAlgorithm
    (OutputConfigure4LFT4StreamingAlgorithm(..)
    ,InputConfigure4LFT4StreamingAlgorithm(..)
    ,WholeInputData4LFT4StreamingAlgorithm(..)
    )


data Configure4LFT4StreamingAlgorithm_EngelExpansion
    = Configure4LFT4StreamingAlgorithm_EngelExpansion Bool
    -- after init?
    deriving (Show, Read, Eq, Ord)


instance OutputConfigure4LFT4StreamingAlgorithm Configure4LFT4StreamingAlgorithm_EngelExpansion where
    type OutputType4OutputConfigure4LFT4StreamingAlgorithm
        Configure4LFT4StreamingAlgorithm_EngelExpansion = Integer
    maybe_make_output
        (Configure4LFT4StreamingAlgorithm_EngelExpansion after_init)
        rational
        = if after_init
            then if rational == 0 then Nothing else
                Just $ ceiling (1/rational)
            else Just $ floor rational

    output2inv_LFT_ex oconfigure digit
        = if after_init
            then (later_mx, oconfigure)
            else (init_mx, oconfigure')
        -- matrix[1,-floor;0,1]
        -- matrix[digit, -1;0,1]
        where
            (Configure4LFT4StreamingAlgorithm_EngelExpansion after_init
                ) = oconfigure
            init_mx = LinearFractionalTransformation
                    (LinearTransformation 1 (-digit))
                    (LinearTransformation 0 1)
            later_mx = LinearFractionalTransformation
                    (LinearTransformation digit (-1))
                    (LinearTransformation 0 1)
            oconfigure' = Configure4LFT4StreamingAlgorithm_EngelExpansion True


instance InputConfigure4LFT4StreamingAlgorithm Configure4LFT4StreamingAlgorithm_EngelExpansion where
    type InputType4InputConfigure4LFT4StreamingAlgorithm Configure4LFT4StreamingAlgorithm_EngelExpansion = Integer

    input2LFT_ex iconfigure digit = if after_init
        then (later_mx, iconfigure)
        else (init_mx, iconfigure')
        where
            -- init = matrix[1, floor; 0, 1]
            -- next digit = matrix[1,1;0,digit]
            -- 0 <= x <= 1
            (Configure4LFT4StreamingAlgorithm_EngelExpansion after_init
                ) = iconfigure
            init_mx = LinearFractionalTransformation
                    (LinearTransformation 1 digit)
                    (LinearTransformation 0 1)
            later_mx = LinearFractionalTransformation
                    (LinearTransformation 1 1)
                    (LinearTransformation 0 digit)
            iconfigure' = Configure4LFT4StreamingAlgorithm_EngelExpansion True


instance WholeInputData4LFT4StreamingAlgorithm EngelExpansion where
    type InputConfigureType4WholeInputData4LFT4StreamingAlgorithm EngelExpansion = Configure4LFT4StreamingAlgorithm_EngelExpansion
    initial_input_configure_LFT
        (EngelExpansion floor_part float_part)
        = iconfigure where
        iconfigure = Configure4LFT4StreamingAlgorithm_EngelExpansion False

    inputs_LFT engel_expansion
        -- engel_expansion@(EngelExpansion floor_part float_part)
        = inputs where
        (floor_part, digits) = unEngelExpansion engel_expansion
        -- bug: inputs = [(d, Inside 0 1) | d <- floor_part:digits]
        all_digits = floor_part:digits
        inputs = zip_except_last all_digits (repeat $ Inside 0 1) (Inside 0 0)

    initial_state_ex_LFT
        (EngelExpansion floor_part float_part)
        = (mempty, (Inside floor_part_r $ floor_part_r+1))
        where
            floor_part_r = toRational floor_part


