
{-# LANGUAGE TypeFamilies #-}

module Configure4LFT4StreamingAlgorithm_Rational
    (Configure4LFT4StreamingAlgorithm_Rational(..)
    ,Fraction(..)
    ,unFraction
    )
where

import Interval
import LinearFractionalTransformation
import Configure4LFT4StreamingAlgorithm
    (OutputConfigure4LFT4StreamingAlgorithm(..)
    ,InputConfigure4LFT4StreamingAlgorithm(..)
    ,WholeInputData4LFT4StreamingAlgorithm(..)
    )
import Data.Ratio(numerator, denominator)

data Configure4LFT4StreamingAlgorithm_Rational
    = Configure4LFT4StreamingAlgorithm_Rational
    deriving (Show, Read, Eq, Ord)
newtype Fraction = Fraction Rational
unFraction :: Fraction -> Rational
unFraction (Fraction rational) = rational

instance OutputConfigure4LFT4StreamingAlgorithm Configure4LFT4StreamingAlgorithm_Rational where
    type OutputType4OutputConfigure4LFT4StreamingAlgorithm
        Configure4LFT4StreamingAlgorithm_Rational = Rational
    maybe_make_output oconfigure rational = Just rational

    output2inv_LFT_ex oconfigure digit_r = (init_mx, oconfigure)
        -- matrix[1,-digit;0,1]
        --  y=r+0 = digit + x
        --  x=y - digit = (1*y -digit)/1
        --  0 <= x <= 0
        where
            init_mx' = LinearFractionalTransformation
                    (LinearTransformation d (-n))
                    (LinearTransformation 0 d)
            init_mx = zero_LinearFractionalTransformation
            n = numerator digit_r
            d = denominator digit_r


instance InputConfigure4LFT4StreamingAlgorithm Configure4LFT4StreamingAlgorithm_Rational where
    type InputType4InputConfigure4LFT4StreamingAlgorithm Configure4LFT4StreamingAlgorithm_Rational = Rational

    input2LFT_ex iconfigure digit_r = (init_mx, iconfigure)
        where
            -- init = matrix[1, digit; 0, 1]
            -- 0 <= x <= 0
            init_mx = LinearFractionalTransformation
                    (LinearTransformation d n)
                    (LinearTransformation 0 d)
            n = numerator digit_r
            d = denominator digit_r


instance WholeInputData4LFT4StreamingAlgorithm Fraction where
    type InputConfigureType4WholeInputData4LFT4StreamingAlgorithm Fraction = Configure4LFT4StreamingAlgorithm_Rational
    initial_input_configure_LFT fraction = iconfigure where
        iconfigure = Configure4LFT4StreamingAlgorithm_Rational

    inputs_LFT fraction = inputs where
        inputs = [(unFraction fraction, Inside 0 0)]

    initial_state_ex_LFT fraction
        = (mempty, Inside rational rational) where
        rational = unFraction fraction


