
module RadixBaseFloatNumber
    (RadixBaseFloatNumber(..)
    ,unRadixBaseFloatNumber
    ,unsafe_mkRadixBaseFloatNumber
    ,maybe_make_finite_RadixBaseFloatNumber
    )
where

import RadixDigitList
    (RadixDigitList
    ,unRadixDigitList
    ,unsafe_mkRadixDigitList
    ,maybe_mk_finite_RadixDigitList
    )

data RadixBaseFloatNumber
    -- floor_part, float_part
    = RadixBaseFloatNumber Integer RadixDigitList
    deriving (Show)

-- radix_base floor_part float_part
unRadixBaseFloatNumber
    :: RadixBaseFloatNumber -> (Integer, Integer, [Integer])
unRadixBaseFloatNumber (RadixBaseFloatNumber floor_part float_part)
    = (radix_base, floor_part, digits)
    where
        (radix_base, digits) = unRadixDigitList float_part

unsafe_mkRadixBaseFloatNumber
    :: Integer -> Integer -> [Integer] -> RadixBaseFloatNumber
unsafe_mkRadixBaseFloatNumber radix_base floor_part
    = RadixBaseFloatNumber floor_part . unsafe_mkRadixDigitList radix_base

maybe_make_finite_RadixBaseFloatNumber
    :: Integer -> Integer -> [Integer] -> Maybe RadixBaseFloatNumber
maybe_make_finite_RadixBaseFloatNumber radix_base floor_part digits = do
    float_part <- maybe_mk_finite_RadixDigitList radix_base digits
    return $ RadixBaseFloatNumber floor_part float_part


