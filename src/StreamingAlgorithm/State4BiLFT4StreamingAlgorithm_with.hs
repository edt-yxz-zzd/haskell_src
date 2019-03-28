
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module State4BiLFT4StreamingAlgorithm_with
    (State4BiLFT4StreamingAlgorithm_with() -- ()
    ,make_binary_operator_args_BiLFT
    ,binary_operator_BiLFT
    ,mul_BiLFT, div_BiLFT, add_BiLFT, sub_BiLFT
    ,make_streaming_args_BiLFT
    ,streaming_BiLFT
    )
where

import Configure4LFT4StreamingAlgorithm
    (OutputConfigure4LFT4StreamingAlgorithm(..)
    ,InputConfigure4LFT4StreamingAlgorithm(..)
    ,WholeInputData4BiLFT4StreamingAlgorithm(..)
    ---binary_operator_BiLFT
    ,WholeInputData4LFT4StreamingAlgorithm(..)
    )
import LinearFractionalTransformation
    (BiLinearFractionalTransformation(..)
    ,are_all_zeros
    ,interval_transformation_ex_BiLFT
    ,inv_output_matrix_mul_BiLFT
    ,mul_fst_input_matrix_BiLFT
    ,mul_snd_input_matrix_BiLFT
    ,the_mul_matrix_BiLFT
    ,the_div_matrix_BiLFT
    ,the_add_matrix_BiLFT
    ,the_sub_matrix_BiLFT
    )
import Interval
import IntervalEx
import StreamingAlgorithm
import Standardizable
import PartialCallable (partial_transform)
import Data.Semigroup ((<>))

data State4BiLFT4StreamingAlgorithm_with oconfigure iconfigure_lhs iconfigure_rhs
    = Private_State4BiLFT4StreamingAlgorithm_with
        oconfigure
        (BiLinearFractionalTransformation Integer)
        iconfigure_lhs (Interval Rational)
        iconfigure_rhs (Interval Rational)
    -- oconfigure state_BiLFT iconfigure_lhs interval_lhs iconfigure_rhs interval_rhs
    deriving (Show)

instance Standardizable (State4BiLFT4StreamingAlgorithm_with oconfigure iconfigure_lhs iconfigure_rhs) where
    standardize (Private_State4BiLFT4StreamingAlgorithm_with
        oconfigure state_BiLFT iconfigure_lhs interval_lhs iconfigure_rhs interval_rhs
        ) = Private_State4BiLFT4StreamingAlgorithm_with
                oconfigure
                (standardize state_BiLFT)
                iconfigure_lhs interval_lhs
                iconfigure_rhs interval_rhs

instance
    (OutputConfigure4LFT4StreamingAlgorithm oconfigure
    ,InputConfigure4LFT4StreamingAlgorithm iconfigure_lhs
    ,InputConfigure4LFT4StreamingAlgorithm iconfigure_rhs
    ,Eq (OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure)
        -- FlexibleContexts
    ,InputType4InputConfigure4LFT4StreamingAlgorithm iconfigure_lhs
          ~ InputType4InputConfigure4LFT4StreamingAlgorithm iconfigure_rhs
    ) => StreamingAlgorithm (State4BiLFT4StreamingAlgorithm_with oconfigure iconfigure_lhs iconfigure_rhs) where

    type OutputType4StreamingAlgorithm
        (State4BiLFT4StreamingAlgorithm_with oconfigure iconfigure_lhs iconfigure_rhs)
        = OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure
    type InputType4StreamingAlgorithm
        (State4BiLFT4StreamingAlgorithm_with oconfigure iconfigure_lhs iconfigure_rhs)
        = (InputType4InputConfigure4LFT4StreamingAlgorithm iconfigure_lhs, Interval Rational)
    type MultiwayInputType4StreamingAlgorithm
        (State4BiLFT4StreamingAlgorithm_with oconfigure iconfigure_lhs iconfigure_rhs)
        = ([(InputType4InputConfigure4LFT4StreamingAlgorithm iconfigure_lhs, Interval Rational)]
          ,[(InputType4InputConfigure4LFT4StreamingAlgorithm iconfigure_rhs, Interval Rational)]
          )
    type InputWaySelectorType4StreamingAlgorithm
        (State4BiLFT4StreamingAlgorithm_with oconfigure iconfigure_lhs iconfigure_rhs)
        = Bool


    maybe_poll_output std_state
        = maybe (Left selector) (Right . g) may_output
        where
            (Private_State4BiLFT4StreamingAlgorithm_with
                oconfigure state_BiLFT
                iconfigure_lhs interval_lhs
                iconfigure_rhs interval_rhs
                ) = unStandard std_state

            may_output = if are_all_zeros state_BiLFT
                            then Nothing
                            else may_output''
            (interval_output, selector
                        ) = interval_transformation_ex_BiLFT
                                    state_BiLFT
                                    (IntervalEx interval_lhs)
                                    (IntervalEx interval_rhs)
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


            g output = (output, mkStandard nonstd_state') where
                (inv_LFT, oconfigure') = output2inv_LFT_ex oconfigure output
                state_BiLFT' = inv_output_matrix_mul_BiLFT
                                    inv_LFT state_BiLFT
                nonstd_state' = Private_State4BiLFT4StreamingAlgorithm_with
                                    oconfigure'
                                    state_BiLFT'
                                    iconfigure_lhs interval_lhs
                                    iconfigure_rhs interval_rhs
    get_inputs _m_state selector inputss = inputs
        where inputs = (if selector then snd else fst) inputss
    set_inputs _m_state selector inputs inputss = inputss'
        where
            inputss' = if selector
                        then (fst inputss, inputs)
                        else (inputs, snd inputss)
    update_after_consume std_state selector input = std_state'
        where
            (Private_State4BiLFT4StreamingAlgorithm_with
                oconfigure state_BiLFT
                iconfigure_lhs interval_lhs
                iconfigure_rhs interval_rhs
                ) = unStandard std_state
            std_state' = mkStandard nonstd_state'
            nonstd_state' = if selector
                then Private_State4BiLFT4StreamingAlgorithm_with
                        oconfigure state_BiLFT_rhs'
                        iconfigure_lhs interval_lhs
                        iconfigure_rhs' interval_rhs'
                else Private_State4BiLFT4StreamingAlgorithm_with
                        oconfigure state_BiLFT_lhs'
                        iconfigure_lhs' interval_lhs'
                        iconfigure_rhs interval_rhs
            (input_digit, input_interval_LFT) = input

            ----------------
            (mx_lhs, iconfigure_lhs') = input2LFT_ex iconfigure_lhs input_digit
            state_BiLFT_lhs' = mul_fst_input_matrix_BiLFT state_BiLFT mx_lhs ()
            interval_lhs' = input_interval_LFT
            ----------------
            (mx_rhs, iconfigure_rhs') = input2LFT_ex iconfigure_rhs input_digit
            state_BiLFT_rhs' = mul_snd_input_matrix_BiLFT state_BiLFT () mx_rhs
            interval_rhs' = input_interval_LFT

make_binary_operator_args_BiLFT
    :: (OutputConfigure4LFT4StreamingAlgorithm oconfigure
        ,WholeInputData4LFT4StreamingAlgorithm whole_input_lhs
        ,WholeInputData4LFT4StreamingAlgorithm whole_input_rhs
        ,InputType4InputConfigure4LFT4StreamingAlgorithm
            (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input_lhs)
        ~ InputType4InputConfigure4LFT4StreamingAlgorithm
            (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input_rhs)
        )
    => oconfigure
    -> BiLinearFractionalTransformation Integer
    -> whole_input_lhs
    -> whole_input_rhs
    -> ((Standard (State4BiLFT4StreamingAlgorithm_with
            oconfigure
            (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input_lhs)
            (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input_rhs)
        ))
       , ([(InputType4InputConfigure4LFT4StreamingAlgorithm
                (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input_lhs)
            , Interval Rational
         )]
        , [(InputType4InputConfigure4LFT4StreamingAlgorithm
                (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input_rhs)
            , Interval Rational
         )]
         )
       )
make_binary_operator_args_BiLFT oconfigure mx_BiLFT whole_input_lhs whole_input_rhs
    = (std_state0, inputss) where
    iconfigure_lhs = initial_input_configure_LFT whole_input_lhs
    iconfigure_rhs = initial_input_configure_LFT whole_input_rhs

    (nonstd_state0_LFT_lhs, interval_lhs
        ) = initial_state_ex_LFT whole_input_lhs
    (nonstd_state0_LFT_rhs, interval_rhs
        ) = initial_state_ex_LFT whole_input_rhs
    _0_nonstd_state0_BiLFT = mx_BiLFT
    _1_nonstd_state0_BiLFT = mul_fst_input_matrix_BiLFT _0_nonstd_state0_BiLFT nonstd_state0_LFT_lhs ()
    _2_nonstd_state0_BiLFT = mul_snd_input_matrix_BiLFT _1_nonstd_state0_BiLFT () nonstd_state0_LFT_rhs
    nonstd_state0_BiLFT = _2_nonstd_state0_BiLFT


    inputs_LFT_lhs = inputs_LFT whole_input_lhs
    inputs_LFT_rhs = inputs_LFT whole_input_rhs
    inputss = (inputs_LFT_lhs, inputs_LFT_rhs)
    ------
    nonstd_state0 = Private_State4BiLFT4StreamingAlgorithm_with
                    oconfigure nonstd_state0_BiLFT
                    iconfigure_lhs interval_lhs
                    iconfigure_rhs interval_rhs
    std_state0 = mkStandard nonstd_state0





binary_operator_BiLFT
    :: (OutputConfigure4LFT4StreamingAlgorithm oconfigure
        ,Eq (OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure)
        ,WholeInputData4LFT4StreamingAlgorithm whole_input_lhs
        ,WholeInputData4LFT4StreamingAlgorithm whole_input_rhs
        ,InputType4InputConfigure4LFT4StreamingAlgorithm
            (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input_lhs)
        ~ InputType4InputConfigure4LFT4StreamingAlgorithm
            (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input_rhs)
        )
    => oconfigure
    -> BiLinearFractionalTransformation Integer
    -> whole_input_lhs
    -> whole_input_rhs
    -> [OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure]
binary_operator_BiLFT oconfigure mx_BiLFT whole_input_lhs whole_input_rhs
    = outputs where
    (std_state0, inputss) = make_binary_operator_args_BiLFT
                    oconfigure mx_BiLFT whole_input_lhs whole_input_rhs
    outputs = streaming std_state0 inputss

mul_BiLFT, div_BiLFT, add_BiLFT, sub_BiLFT
    :: (OutputConfigure4LFT4StreamingAlgorithm oconfigure
        ,Eq (OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure)
        ,WholeInputData4LFT4StreamingAlgorithm whole_input_lhs
        ,WholeInputData4LFT4StreamingAlgorithm whole_input_rhs
        ,InputType4InputConfigure4LFT4StreamingAlgorithm
            (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input_lhs)
        ~ InputType4InputConfigure4LFT4StreamingAlgorithm
            (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input_rhs)
        )
    => oconfigure
    -> whole_input_lhs
    -> whole_input_rhs
    -> [OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure]
mul_BiLFT = flip binary_operator_BiLFT the_mul_matrix_BiLFT
div_BiLFT = flip binary_operator_BiLFT the_div_matrix_BiLFT
add_BiLFT = flip binary_operator_BiLFT the_add_matrix_BiLFT
sub_BiLFT = flip binary_operator_BiLFT the_sub_matrix_BiLFT







make_streaming_args_BiLFT
    :: (OutputConfigure4LFT4StreamingAlgorithm oconfigure
        ,WholeInputData4BiLFT4StreamingAlgorithm whole_input
        ,InputType4InputConfigure4LFT4StreamingAlgorithm
            (FstInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input)
        ~ InputType4InputConfigure4LFT4StreamingAlgorithm
            (SndInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input)
        )
    => oconfigure -> whole_input
    -> ((Standard (State4BiLFT4StreamingAlgorithm_with
            oconfigure
            (FstInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input)
            (SndInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input)
        ))
       , ([(InputType4InputConfigure4LFT4StreamingAlgorithm
                (FstInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input)
            , Interval Rational
         )]
        , [(InputType4InputConfigure4LFT4StreamingAlgorithm
                (SndInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input)
            , Interval Rational
         )]
         )
       )


streaming_BiLFT
    :: (OutputConfigure4LFT4StreamingAlgorithm oconfigure
        ,WholeInputData4BiLFT4StreamingAlgorithm whole_input
        ,Eq (OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure)
        ,InputType4InputConfigure4LFT4StreamingAlgorithm
            (FstInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input)
        ~ InputType4InputConfigure4LFT4StreamingAlgorithm
            (SndInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input)
        )
    => oconfigure -> whole_input
    -> [OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure]


make_streaming_args_BiLFT oconfigure whole_input
    = (std_state0, inputss) where
    (iconfigure_lhs, iconfigure_rhs
        ) = initial_input_configures_BiLFT whole_input
    (nonstd_state0_BiLFT, (interval_lhs, interval_rhs)
        ) = initial_state_ex_BiLFT whole_input
    inputss = inputss_BiLFT whole_input
    ------
    nonstd_state0 = Private_State4BiLFT4StreamingAlgorithm_with
                    oconfigure nonstd_state0_BiLFT
                    iconfigure_lhs interval_lhs
                    iconfigure_rhs interval_rhs
    std_state0 = mkStandard nonstd_state0

streaming_BiLFT oconfigure whole_input
    = outputs where
    (std_state0, inputss) = make_streaming_args_BiLFT oconfigure whole_input
    outputs = streaming std_state0 inputss




