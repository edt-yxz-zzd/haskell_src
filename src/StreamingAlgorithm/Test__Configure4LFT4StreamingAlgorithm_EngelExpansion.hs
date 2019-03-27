
module Test__Configure4LFT4StreamingAlgorithm_EngelExpansion
    (main
    )
where


import EngelExpansion (unsafe_mkEngelExpansion)
import Configure4LFT4StreamingAlgorithm_EngelExpansion ()
-- test only
import State4LFT4StreamingAlgorithm_with (streaming_LFT)
import RadixBase (unsafe_mkRadixBase)
import Configure4LFT4StreamingAlgorithm_RadixBase ()


-- 1, 1, 1, 8, 8, 17, 19, 300, 1991, 2492 ...
pi_EngelExpansion = unsafe_mkEngelExpansion 3 [8, 8, 17, 19, 300, 1991, 2492]

main :: IO ()
main = do
    print "pi_EngelExpansion -> radix base 10"
    print $ "pi_EngelExpansion = " ++ show pi_EngelExpansion
    let oconfigure = unsafe_mkRadixBase 10
    print $ streaming_LFT oconfigure pi_EngelExpansion


