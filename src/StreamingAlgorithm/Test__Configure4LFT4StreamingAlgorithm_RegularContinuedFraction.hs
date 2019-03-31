
module Test__Configure4LFT4StreamingAlgorithm_RegularContinuedFraction
    (main
    )
where

import RegularContinuedFraction
import Configure4LFT4StreamingAlgorithm_RegularContinuedFraction
import RadixBaseFloatNumber (unsafe_mkRadixBaseFloatNumber)
import State4LFT4StreamingAlgorithm_with (streaming_LFT)
import RadixBase (unsafe_mkRadixBase)
import Configure4LFT4StreamingAlgorithm_RadixBase ()


inv_e_base3 = unsafe_mkRadixBaseFloatNumber 3   0 [1,0,0,2,2,1,0,1,1,2]
inv_e_base7 = unsafe_mkRadixBaseFloatNumber 7   0 [2,4,0,1,1,6,4,3,5,2]

main :: IO ()
main = do
    let oconfigure = Configure4LFT4StreamingAlgorithm_RegularContinuedFraction False
    print "inv_e_base3 -> RegularContinuedFraction"
    print $ streaming_LFT oconfigure inv_e_base3

    print "inv_e_base7 -> RegularContinuedFraction"
    print $ streaming_LFT oconfigure inv_e_base7



