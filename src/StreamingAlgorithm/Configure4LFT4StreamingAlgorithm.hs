
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


-- see: State4LFT4StreamingAlgorithm_with
module Configure4LFT4StreamingAlgorithm
    (OutputConfigure4LFT4StreamingAlgorithm(..)
    ,InputConfigure4LFT4StreamingAlgorithm(..)
    ,WholeInputData4LFT4StreamingAlgorithm(..)
    ,WholeInputData4BiLFT4StreamingAlgorithm(..)
    )
where

import LinearFractionalTransformation
import Interval

class OutputConfigure4LFT4StreamingAlgorithm oconfigure where
    type OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure :: *
    maybe_make_output
        :: oconfigure -> Rational
        -> Maybe (OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure)
    output2inv_LFT_ex
        :: oconfigure
        -> OutputType4OutputConfigure4LFT4StreamingAlgorithm oconfigure
        -> (LinearFractionalTransformation Integer, oconfigure)


class InputConfigure4LFT4StreamingAlgorithm iconfigure where
    type InputType4InputConfigure4LFT4StreamingAlgorithm iconfigure :: *
    input2LFT_ex
        :: iconfigure
        -> InputType4InputConfigure4LFT4StreamingAlgorithm iconfigure
        -> (LinearFractionalTransformation Integer, iconfigure)

class (InputConfigure4LFT4StreamingAlgorithm
        (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input)
    ) => WholeInputData4LFT4StreamingAlgorithm whole_input where
    type InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input :: *
    initial_input_configure_LFT
        :: whole_input
        -> InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input
    inputs_LFT
        :: whole_input
        -> [(InputType4InputConfigure4LFT4StreamingAlgorithm
                (InputConfigureType4WholeInputData4LFT4StreamingAlgorithm whole_input)
            , Interval Rational
            )]
    initial_state_ex_LFT
        -- :: (OutputConfigure4LFT4StreamingAlgorithm oconfigure)
        -- => oconfigure
        :: whole_input
        -> (LinearFractionalTransformation Integer, Interval Rational)

class
    (InputConfigure4LFT4StreamingAlgorithm
        (FstInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input)
    ,InputConfigure4LFT4StreamingAlgorithm
        (SndInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input)
    ) => WholeInputData4BiLFT4StreamingAlgorithm whole_input where
    type FstInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input :: *
    type SndInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input :: *
    initial_input_configures_BiLFT
        :: whole_input
        -> (FstInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input
            ,SndInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input
            )
    inputss_BiLFT
        :: whole_input
        -> ([(InputType4InputConfigure4LFT4StreamingAlgorithm
                (FstInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input)
            , Interval Rational
            )]
            ,[(InputType4InputConfigure4LFT4StreamingAlgorithm
                (SndInputConfigureType4WholeInputData4BiLFT4StreamingAlgorithm whole_input)
            , Interval Rational
            )]
            )
    initial_state_ex_BiLFT
        :: whole_input
        -> (BiLinearFractionalTransformation Integer
            , (Interval Rational, Interval Rational))

