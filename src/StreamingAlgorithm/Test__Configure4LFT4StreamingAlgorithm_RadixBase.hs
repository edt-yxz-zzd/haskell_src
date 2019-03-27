
module Test__Configure4LFT4StreamingAlgorithm_RadixBase
    (main
    )
where

import Configure4LFT4StreamingAlgorithm_RadixBase (float_number_convertor)
import RadixBaseFloatNumber (unsafe_mkRadixBaseFloatNumber)
import Control.Monad (forM_, unless, when, guard, mzero)


inv_e_base3 = unsafe_mkRadixBaseFloatNumber 3   0 [1,0,0,2,2,1,0,1,1,2]
inv_e_base7 = unsafe_mkRadixBaseFloatNumber 7   0 [2,4,0,1,1,6,4,3,5,2]

main :: IO ()
main = do
    main1__radix_base_convert

main1__radix_base_convert :: IO ()
main1__radix_base_convert = do
    print "inv_e_base3 -> inv_e_base7"
    print inv_e_base7
    print $ float_number_convertor inv_e_base3 7

    print "inv_e_base7 -> inv_e_base3"
    print inv_e_base3
    print $ float_number_convertor inv_e_base7 3

    print "inv_e_base3 -> e_base10"
    print $ float_number_convertor inv_e_base3 10
    print "inv_e_base7 -> e_base10"
    print $ float_number_convertor inv_e_base7 10


