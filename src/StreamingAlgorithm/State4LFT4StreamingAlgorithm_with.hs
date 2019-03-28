
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}



{-
LinearFractionalTransformation as core state:
    radix float number:
        .radix_base
        .floor
        .digits
        ---------
        x = floor + 1/radix_base*(digits[0] + ...)
        output:
            #bug: xMIN=0, xMAX=radix_base
            if floor (call state_mx xMIN) == floor (call state_mx xMAX):
                digit = floor ...
                y = f digit x = digit + 1/radix_base*x
                f digit = matrix[1, digit*radix_base; 0, radix_base]
                x = radix_base*y - digit*radix_base
                (f digit)^-1 = matrix[radix_base, -digit*radix_base; 0, 1]
                state_mx' = (digit + 1/radix_base*)^-1 * state_mx
                        = (f digit)^-1 * state_mx
                        = matrix[radix_base, -digit*radix_base; 0, 1] * state_mx
        input1:
            init x = floor + x
            init = matrix[1, floor; 0, 1]
            next digit x = 1/radix_base*(digit + x)
            next digit = matrix[1, digit; 0, radix_base]
            0 <= x <= 1
        input2:
            init x = floor + 1/radix_base*x
            init = matrix[1, floor*radix_base; 0, radix_base]
            next digit x = digit + 1/radix_base*x
            next digit = matrix[1, digit*radix_base; 0, radix_base]
            0 <= x <= radix_base
    continued fraction:
        .floor
        .digits
        ---------
        x = floor + 1/(digits[0] + 1/(digits[1] + ...))
        output:
            #bug: xMIN=1, xMAX=+oo
            if floor (call state_mx xMIN) == floor (call state_mx xMAX):
                digit = floor ...
                y = f digit x = digit + 1/x # x==+oo??
                f digit = matrix[digit,1;1,0]
                x = 1/(y-digit)
                    # y==digit ==>> x==+oo
                    #   state_mx==matrix[digit*a, digit*b; a, b]
                    #   state_mx'==matrix[a,b; 0,0]
                (f digit)^-1 = matrix[0,1;1,-digit]
                state_mx' = (f digit)^-1 * state_mx
                            = matrix[0,1;1-digit] * state_mx
        input:
            init x = floor + x
            init = matrix[1, floor; 0, 1]
            next digit x = 1/(digit + x)
            next digit = matrix[0, 1; 1, digit]
            0 <= x <= 1

    EngelExpansion
        .floor
        .digits
        ---------
        x = floor + 1/digits[0]*(1 + 1/digits[1]*(...))
        output:
            init floor:
                if floor (call state_mx xMIN) == floor (call state_mx xMAX):
                    output floor ...
                    y = f floor x = floor + x
                    x = y - floor
                    (f floor)^-1 = matrix[1,-floor;0,1]
                    state_mx' = matrix[1,-floor;0,1]*state_mx
            later ceil inv:
                if ceil (1/call state_mx xMIN) == ceil (1/call state_mx xMAX):
                    digit = ceil...
                    y = f digit x = 1/digit*(1+x)
                    x = digit*y - 1
                    (f digit)^-1 = matrix[digit, -1;0,1]
                    state_mx' = matrix[digit, -1;0,1]*state_mx
        input:
            init x = floor + x
            init = matrix[1,floor;0,1]
            # digit>=2
            next digit x = 1/digit*(1+x)
            next digit = matrix[1,1;0,digit]
            0 <= x <= 1/(digit-1)
            or 0 <= x <= 1


-}

module State4LFT4StreamingAlgorithm_with
    (State4LFT4StreamingAlgorithm_with() -- ()
    ,make_streaming_args_LFT
    ,streaming_LFT
    )
where

import Configure4LFT4StreamingAlgorithm
    (OutputConfigure4LFT4StreamingAlgorithm(..)
    ,InputConfigure4LFT4StreamingAlgorithm(..)
    ,WholeInputData4LFT4StreamingAlgorithm(..)
    )
import LinearFractionalTransformation
    (LinearFractionalTransformation(..)
    ,are_all_zeros
    ,interval_transformation_LFT
    )
import Interval
import IntervalEx
import StreamingAlgorithm
import Standardizable
import PartialCallable (partial_transform)
import Data.Semigroup ((<>))



data State4LFT4StreamingAlgorithm_with oconfigure iconfigure
    = Private_State4LFT4StreamingAlgorithm_with
        oconfigure
        (LinearFractionalTransformation Integer)
        iconfigure
        (Interval Rational)
    -- oconfigure state_LFT iconfigure interval_input
    deriving (Show, Read)

instance Standardizable (State4LFT4StreamingAlgorithm_with oconfigure iconfigure) where
    standardize (Private_State4LFT4StreamingAlgorithm_with
        oconfigure state_LFT iconfigure interval_input
        ) = Private_State4LFT4StreamingAlgorithm_with
                oconfigure
                (standardize state_LFT)
                iconfigure
                interval_input

instance
    (OutputConfigure4LFT4StreamingAlgorithm oconfigure
    ,InputConfigure4LFT4StreamingAlgorithm iconfigure
    ,Eq (OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure)
        -- FlexibleContexts
    ) => StreamingAlgorithm (State4LFT4StreamingAlgorithm_with oconfigure iconfigure) where

    type OutputType4StreamingAlgorithm
        (State4LFT4StreamingAlgorithm_with oconfigure iconfigure)
        = OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure
    type InputType4StreamingAlgorithm
        (State4LFT4StreamingAlgorithm_with oconfigure iconfigure)
        = (InputType4InputConfigure4LFT4StreamingAlgorithm iconfigure
            ,Interval Rational)
    type MultiwayInputType4StreamingAlgorithm
        (State4LFT4StreamingAlgorithm_with oconfigure iconfigure)
        = [(InputType4InputConfigure4LFT4StreamingAlgorithm iconfigure
            ,Interval Rational)]
        -- [InputType4StreamingAlgorithm (State4LFT4StreamingAlgorithm_with oconfigure iconfigure)]
    type InputWaySelectorType4StreamingAlgorithm
        (State4LFT4StreamingAlgorithm_with oconfigure iconfigure)
        = ()


    maybe_poll_output std_state
        = maybe (Left ()) (Right . g) may_output
        where
            (Private_State4LFT4StreamingAlgorithm_with
                oconfigure state_LFT iconfigure interval_input
                ) = unStandard std_state

            may_output = if are_all_zeros state_LFT
                            then Nothing
                            else may_output''
            interval_output = interval_transformation_LFT state_LFT
                                    $ IntervalEx interval_input
            may_output'' = case interval_output of
                    IntervalEx interval -> case interval of
                        Inside c d -> do
                            output <- maybe_make_output oconfigure c
                            output' <- maybe_make_output oconfigure d
                            -- bug: if c == d then return output else Nothing
                            if output == output' then return output else Nothing
                        Outside a b -> Nothing
                        LessEq a -> Nothing
                        GreaterEq a -> Nothing
                    Inside_oo_oo ->  Nothing
                    Outside_oo_oo ->  Nothing

            f = partial_transform state_LFT
            may_output' = case interval_input of
                    Inside a b -> do
                        c <- f a
                        d <- f b
                        output <- maybe_make_output oconfigure c
                        output' <- maybe_make_output oconfigure d
                        -- bug: if c == d then return output else Nothing
                        if output == output' then return output else Nothing
                    _ -> Nothing

            g output = (output, mkStandard nonstd_state') where
                (inv_LFT, oconfigure') = output2inv_LFT_ex oconfigure output
                state_LFT' = inv_LFT <> state_LFT
                nonstd_state' = Private_State4LFT4StreamingAlgorithm_with
                                    oconfigure'
                                    state_LFT'
                                    iconfigure
                                    interval_input
    get_inputs _m_state _selector inputss = inputs
        where inputs=inputss
    set_inputs _m_state _selector inputs inputss = inputss'
        where inputss'=inputs
    update_after_consume std_state _selector input = std_state'
        where
            (Private_State4LFT4StreamingAlgorithm_with
                oconfigure state_LFT iconfigure interval_input
                ) = unStandard std_state
            std_state' = mkStandard nonstd_state'
            nonstd_state' = Private_State4LFT4StreamingAlgorithm_with
                                oconfigure
                                state_LFT'
                                iconfigure'
                                interval_input'
            (input_digit, input_interval_LFT) = input
            (mx, iconfigure') = input2LFT_ex iconfigure input_digit
            state_LFT' = state_LFT <> mx
            interval_input' = input_interval_LFT



make_streaming_args_LFT
    :: (OutputConfigure4LFT4StreamingAlgorithm oconfigure
        ,WholeInputData4LFT4StreamingAlgorithm whole_input
        )
    => oconfigure -> whole_input
    -> ((Standard (State4LFT4StreamingAlgorithm_with
            oconfigure
            (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input)
        ))
       , [(InputType4InputConfigure4LFT4StreamingAlgorithm
                (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input)
            , Interval Rational
         )]
       )

streaming_LFT
    :: (OutputConfigure4LFT4StreamingAlgorithm oconfigure
        ,WholeInputData4LFT4StreamingAlgorithm whole_input
        ,Eq (OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure)
        )
    => oconfigure -> whole_input
    -> [OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure]


make_streaming_args_LFT oconfigure whole_input
    = (std_state0, inputs) where
    iconfigure = initial_input_configure_LFT whole_input
    (nonstd_state0_LFT, interval_input) = initial_state_ex_LFT whole_input
    inputs = inputs_LFT whole_input
    ------
    nonstd_state0 = Private_State4LFT4StreamingAlgorithm_with
                    oconfigure nonstd_state0_LFT iconfigure interval_input
    std_state0 = mkStandard nonstd_state0

streaming_LFT oconfigure whole_input
    = outputs where
    (std_state0, inputs) = make_streaming_args_LFT oconfigure whole_input
    outputs = streaming std_state0 inputs



