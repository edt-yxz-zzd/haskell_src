
{-# LANGUAGE TypeFamilies #-}

module Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction
    (Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction(..)
    )
where

import SimpleContinuedFraction
import ZipExceptLast (zip_except_last)
import Interval
import LinearFractionalTransformation
import Configure4LFT4StreamingAlgorithm
    (OutputConfigure4LFT4StreamingAlgorithm(..)
    ,InputConfigure4LFT4StreamingAlgorithm(..)
    ,WholeInputData4LFT4StreamingAlgorithm(..)
    )


data Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction
    = Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction Bool
    -- after init?
    deriving (Show, Read, Eq, Ord)

instance OutputConfigure4LFT4StreamingAlgorithm Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction where
    type OutputType4OutputConfigure4LFT4StreamingAlgorithm
        Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction = Integer
    maybe_make_output
        (Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction after_init)
        rational
        = if after_init
            then if rational == 0 then Nothing else
                Just $ floor (1/rational)
            else Just $ floor rational

    output2inv_LFT_ex oconfigure digit
        = if after_init
            then (later_mx, oconfigure)
            else (init_mx, oconfigure')
        -- matrix[1,-floor;0,1]
        -- matrix[-digit, 1;1,0]
        --  y=1/(digit+x)
        --  x=1/y - digit = (-digit*y+1)/y
        --  0 <= x <= 1
        where
            (Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction after_init
                ) = oconfigure
            init_mx = LinearFractionalTransformation
                    (LinearTransformation 1 (-digit))
                    (LinearTransformation 0 1)
            later_mx = LinearFractionalTransformation
                    (LinearTransformation (-digit) 1)
                    (LinearTransformation 1 0)
            oconfigure' = Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction True


instance InputConfigure4LFT4StreamingAlgorithm Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction where
    type InputType4InputConfigure4LFT4StreamingAlgorithm Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction = Integer

    input2LFT_ex iconfigure digit = if after_init
        then (later_mx, iconfigure)
        else (init_mx, iconfigure')
        where
            -- init = matrix[1, floor; 0, 1]
            -- next digit = matrix[0,1;1,digit]
            -- 0 <= x <= 1
            (Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction after_init
                ) = iconfigure
            init_mx = LinearFractionalTransformation
                    (LinearTransformation 1 digit)
                    (LinearTransformation 0 1)
            later_mx = LinearFractionalTransformation
                    (LinearTransformation 0 1)
                    (LinearTransformation 1 digit)
            iconfigure' = Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction True


instance WholeInputData4LFT4StreamingAlgorithm SimpleContinuedFraction where
    type InputConfigureType4WholeInputData4LFT4StreamingAlgorithm SimpleContinuedFraction = Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction
    initial_input_configure_LFT
        (SimpleContinuedFraction floor_part float_part)
        = iconfigure where
        iconfigure = Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction False

    inputs_LFT continued_fraction
        = inputs where
        (floor_part, digits) = unSimpleContinuedFraction continued_fraction
        -- bug: inputs = [(d, Inside 0 1) | d <- floor_part:digits]
        all_digits = floor_part:digits
        inputs = zip_except_last all_digits (repeat $ Inside 0 1) (Inside 0 0)

    initial_state_ex_LFT
        (SimpleContinuedFraction floor_part float_part)
        = (mempty, (Inside floor_part_r $ floor_part_r+1))
        where
            floor_part_r = toRational floor_part


