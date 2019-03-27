
{-# LANGUAGE TypeFamilies #-}

module Configure4LFT4StreamingAlgorithm_RadixBase
    (module RadixBase
    ,RadixBase_init(..)
    ,float_number_convertor
    )
where

import ZipExceptLast(zip_except_last)
import RadixBaseFloatNumber
import RadixBase
import Interval
import LinearFractionalTransformation
import Configure4LFT4StreamingAlgorithm
import State4LFT4StreamingAlgorithm_with (streaming_LFT)
--import Control.Exception (assert)



data RadixBase_init = RadixBase_init RadixBase Bool -- after init?
    deriving (Show, Read, Eq, Ord)



instance OutputConfigure4LFT4StreamingAlgorithm RadixBase where
    type OutputType4OutputConfigure4LFT4StreamingAlgorithm RadixBase = Integer
    maybe_make_output _oconfigure rational = Just $ floor rational
    output2inv_LFT_ex oconfigure digit = (mx, oconfigure')
        -- matrix[radix_base, -digit*radix_base; 0, 1]
        where
            radix_base = unRadixBase oconfigure
            mx = LinearFractionalTransformation
                    (LinearTransformation radix_base (-digit*radix_base))
                    (LinearTransformation 0 1)
            oconfigure' = oconfigure


instance InputConfigure4LFT4StreamingAlgorithm RadixBase_init where
    type InputType4InputConfigure4LFT4StreamingAlgorithm RadixBase_init = Integer
    input2LFT_ex iconfigure digit = if after_init
        then (later_mx, iconfigure)
        else (init_mx, iconfigure')
        where
            -- init = matrix[1, floor; 0, 1]
            -- next digit = matrix[1, digit; 0, radix_base]
            -- 0 <= x <= 1
            RadixBase_init _radix_base after_init = iconfigure
            radix_base = unRadixBase _radix_base
            init_mx = LinearFractionalTransformation
                    (LinearTransformation 1 digit)
                    (LinearTransformation 0 1)
            later_mx = LinearFractionalTransformation
                    (LinearTransformation 1 digit)
                    (LinearTransformation 0 radix_base)
            iconfigure' = RadixBase_init _radix_base True


instance WholeInputData4LFT4StreamingAlgorithm RadixBaseFloatNumber where
    type InputConfigureType4WholeInputData4LFT4StreamingAlgorithm RadixBaseFloatNumber = RadixBase_init
    initial_input_configure_LFT float_number = iconfigure where
        (radix_base, floor_part, digits) = unRadixBaseFloatNumber float_number
        iconfigure = RadixBase_init (unsafe_mkRadixBase radix_base) False

    inputs_LFT float_number = inputs where
        (radix_base, floor_part, digits) = unRadixBaseFloatNumber float_number
        -- bug: inputs = [(d, Inside 0 1) | d <- floor_part:digits]
        all_digits = floor_part:digits
        inputs = zip_except_last all_digits (repeat $ Inside 0 1) (Inside 0 0)

    initial_state_ex_LFT
        (RadixBaseFloatNumber floor_part float_part)
        = (mempty, (Inside floor_part_r $ floor_part_r+1))
        where
            floor_part_r = toRational floor_part





float_number_convertor :: RadixBaseFloatNumber -> Integer -> [Integer]
float_number_convertor float_number radix_baseO
    = streaming_LFT oconfigure float_number where
    oconfigure = unsafe_mkRadixBase radix_baseO

{-
float_number_convertor float_number radix_baseO
    = digitsO where
    (radix_baseI, floor_partI, digitsI) = unRadixBaseFloatNumber float_number
    oconfigure = unsafe_mkRadixBase radix_baseO
    iconfigure = RadixBase_init (unsafe_mkRadixBase radix_baseI) False
    init_mx = mempty
    floor_part_r = toRational floor_partI
    std_state0 = mkStandard $ State4LFT4StreamingAlgorithm_with
                init_mx
                (Inside floor_part_r $ floor_part_r+1)
                oconfigure
                iconfigure

    -- bug: forgot floor_partI
    digitsO = streaming std_state0 [(d, Inside 0 1) | d <- floor_partI:digitsI]
-}

