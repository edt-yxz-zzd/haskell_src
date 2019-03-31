
{-


usage:
    runghc RegularContinuedFraction_of_pi__by_Gosper_series.hs

see:
    "NOTE/continued fraction/[A001203]pi.txt"
        matrix as linear fractional transformation
    https://www.cs.ox.ac.uk/jeremy.gibbons/publications/spigot.pdf
        "Unbounded Spigot Algorithms for the Digits of Pi (2006)(Gibbons).pdf"

    -------------------------------
    # Gosper_series
    pi = 3 + (1*1)/(3*4*5) * (8 + (2*3)/(3*7*8) * (...(5*k-2) + (k*(2*k-1))/(3*(3*k+1)*(3*k+2)) * (...)))
        = (3 + (1*1)/(3*4*5) *) (8 + (2*3)/(3*7*8) *) ... ((5*k-2) + (k*(2*k-1))/(3*(3*k+1)*(3*k+2)) *) ...
        = II ((5*k-2) + (k*(2*k-1))/(3*(3*k+1)*(3*k+2)) *) {k <- 1..}
        = LIMIT call II matrix[(k*(2*k-1), 3*(3*k+1)*(3*k+2)*(5*k-2); 0, 3*(3*k+1)*(3*k+2)] {k <- 1..N} Gosper_series(N+1) {N<-0..}
        #pi === 1*Gosper_series(1)
        #Gosper_series(k) <- [27/5 * k - 12/5, 27/5 * k - (6/5)^3]
        fact: st(27/5 * k - 12/5) <= digit <= st(27/5 * k - (6/5)^3)

-}
module RegularContinuedFraction_of_pi__by_Gosper_series
    (the_pi_inputs
    )
where


import UnsafeFromList
import Interval
import LinearFractionalTransformation

import RegularContinuedFraction
import Configure4LFT4StreamingAlgorithm_RegularContinuedFraction
import State4LFT4StreamingAlgorithm_with (streaming_LFT)
import LinearFractionalTransformationIntervalPairs

import Control.Monad (forM_, mapM_)

type Pair a = (a, a)
_27_5 = toRational 27 / 5
_12_5 = toRational 12 / 5
_6_5_3 = (toRational 6 / 5)^3
_mk_arg_pair__Gosper :: Rational -> Pair Rational
_mk_arg_pair__Gosper k = (_27_5*k-_12_5, _27_5*k-_6_5_3)
        -- 27/5 * k - 12/5
        -- 27/5 * k - (6/5)^3
the_pi_inputs () = LinearFractionalTransformationIntervalPairs
    (head pairs)
    (tail pairs)
    where
        pairs = zip mx_ls__k_ge1 (tail bounds__k_ge1)
        -- pi = LIMIT call II matrix[(k*(2*k-1), 3*(3*k+1)*(3*k+2)*(5*k-2); 0, 3*(3*k+1)*(3*k+2)] {k <- 1..N} Gosper_series(N+1) {N<-0..}
        -- pi === 1*Gosper_series(1)
        -- Gosper_series(k) <- [27/5 * k - 12/5, 27/5 * k - (6/5)^3]
        mx_ls__k_ge1 = [unsafe_fromList [k*(2*k-1), kkk*(5*k-2), 0, kkk]
                        | k <- [1..]
                        , let kkk = 3*(3*k+1)*(3*k+2)
                        ]
        bounds__k_ge1 = [Inside a b
                        | k <- [1..]
                        , let (a,b) = _mk_arg_pair__Gosper (toRational k)
                        ]


the_continued_fraction_digits_of_pi__by_Gosper_series ()
    = streaming_LFT oconfigure whole_input
    where
        oconfigure = Configure4LFT4StreamingAlgorithm_RegularContinuedFraction False
        whole_input = the_pi_inputs ()


main :: IO ()
main = do
    print "pi -> RegularContinuedFraction by Gosper_series"
    mapM_ print . zip [0..] $ the_continued_fraction_digits_of_pi__by_Gosper_series ()
    -- print $ the_continued_fraction_digits_of_pi__by_Gosper_series ()
