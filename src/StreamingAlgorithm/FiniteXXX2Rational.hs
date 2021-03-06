
module FiniteXXX2Rational
    (finite_XXX2Rational
    ,finite_RadixBaseFloatNumber2Rational
    ,finite_EngelExpansion2Rational
    ,finite_RegularContinuedFraction2Rational
    )
where


import Configure4LFT4StreamingAlgorithm_Rational
import State4LFT4StreamingAlgorithm_with (streaming_LFT)
import Configure4LFT4StreamingAlgorithm
    (WholeInputData4LFT4StreamingAlgorithm)

import Configure4LFT4StreamingAlgorithm_RadixBase ()
import Configure4LFT4StreamingAlgorithm_EngelExpansion ()
import Configure4LFT4StreamingAlgorithm_RegularContinuedFraction ()

import RadixBaseFloatNumber
import EngelExpansion
import RegularContinuedFraction

oconfigure = Configure4LFT4StreamingAlgorithm_Rational

finite_XXX2Rational
    :: WholeInputData4LFT4StreamingAlgorithm xxx
    => xxx -> Rational
finite_XXX2Rational xxx = rational where
    [rational] = streaming_LFT oconfigure xxx

finite_RadixBaseFloatNumber2Rational :: RadixBaseFloatNumber -> Rational
finite_RadixBaseFloatNumber2Rational = finite_XXX2Rational

finite_EngelExpansion2Rational :: EngelExpansion -> Rational
finite_EngelExpansion2Rational = finite_XXX2Rational

finite_RegularContinuedFraction2Rational :: RegularContinuedFraction -> Rational
finite_RegularContinuedFraction2Rational = finite_XXX2Rational



