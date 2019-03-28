
{-# LANGUAGE TypeFamilies #-}
module LinearFractionalTransformationIntervalPairs
    (IConfigure4LinearFractionalTransformationIntervalPairs(..)
    ,LinearFractionalTransformationIntervalPairs(..)
    )
where

import Interval
import LinearFractionalTransformation
import Configure4LFT4StreamingAlgorithm

data IConfigure4LinearFractionalTransformationIntervalPairs
    = IConfigure4LinearFractionalTransformationIntervalPairs
    deriving (Show, Read, Eq, Ord)
data LinearFractionalTransformationIntervalPairs
    = LinearFractionalTransformationIntervalPairs
        (LinearFractionalTransformation Integer, Interval Rational)
        [(LinearFractionalTransformation Integer, Interval Rational)]
    -- (state0_mx, ...) inputs



instance InputConfigure4LFT4StreamingAlgorithm IConfigure4LinearFractionalTransformationIntervalPairs where
    type InputType4InputConfigure4LFT4StreamingAlgorithm
        IConfigure4LinearFractionalTransformationIntervalPairs
        = LinearFractionalTransformation Integer
    input2LFT_ex iconfigure mx = (mx, iconfigure)

instance WholeInputData4LFT4StreamingAlgorithm LinearFractionalTransformationIntervalPairs where
    type InputConfigureType4WholeInputData4LFT4StreamingAlgorithm
            LinearFractionalTransformationIntervalPairs
            = IConfigure4LinearFractionalTransformationIntervalPairs
    initial_input_configure_LFT _ = iconfigure where
        iconfigure = IConfigure4LinearFractionalTransformationIntervalPairs

    inputs_LFT (LinearFractionalTransformationIntervalPairs _ inputs) = inputs

    initial_state_ex_LFT
        (LinearFractionalTransformationIntervalPairs pair _) = pair



