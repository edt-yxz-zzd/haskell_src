
{-
see:
    "NOTE/continued fraction/Arithmetic with Continued Fractions.txt"

https://en.wikipedia.org/wiki/Chudnovsky_algorithm
    #The time complexity of the algorithm is O(n*log(n)^3)
    #"[pi] Pi Unleashed (2001)[good].djvu" :: [page 230] :: (16.89)
    (640320)^(3/2) / (12*pi)
    = 426880 * sqrt(10005) / pi
    = SUM (6*k)!*(545140134*k+13591409) / ((3*k)! * (k!)^3 * (-262537412640768000)^k) {k <- 0..}
    where
        640320^3 = 262537412640768000
        640320 = 2^6 * 3 * 5 * 23 * 29
        545140134 = 163 * 127 * 19 * 11 * 7 * 3^2 * 2
        13591409 = 13 * 1045493
[pi = x/y]:
    x = 426880 * sqrt(10005)
        #continued fraction of x:
        #len(non_periodic_digits)=1
        #len(periodic_digits)=78408
    y = 13591409 + f(1)
    f(k) = a(k)/b(k)*(d(k) + f(k+1))
        = call matrix[a(k)*d(k), a(k); 0, b(k)] f(k+1)
    a(k) = 8*prdouct[6*k-5,6*k-3,6*k-1]
    b(k) = k^3 * (-262537412640768000)
    d(k) = (545140134*k+13591409)

    --------------------from:-----------------------
    y = SUM (6*k)!*(545140134*k+13591409) / ((3*k)! * (k!)^3 * (-262537412640768000)^k) {k <- 0..}
        = (6*0)! / ((3*0)! * (0!)^3 * (-262537412640768000)^0) *((545140134*0+13591409) + f(1))
        = 13591409 + f(1)
    # k >= 1
    f(k) = c(k)*(d(k) + f(k+1))
            = a(k)/b(k)*(d(k) + f(k+1))
            = call matrix[a(k), a(k)*d(k); 0, b(k)] f(k+1)
    d(k) = (545140134*k+13591409)
    c(k) = ((6*k)! / ((3*k)! * (k!)^3 * (-262537412640768000)^k))
          /((6*(k-1))! / ((3*(k-1))! * ((k-1)!)^3 * (-262537412640768000)^(k-1)))
        = prdouct[(6*k-5)..6*k] / prdouct[3*k-2..3*k] / k^3 / (-262537412640768000)
        = 8*prdouct[6*k-5,6*k-3,6*k-1] / k^3 / (-262537412640768000)
        = a(k)/b(k)
    a(k) = 8*prdouct[6*k-5,6*k-3,6*k-1]
    b(k) = k^3 * (-262537412640768000)







----------------------------------
min max?
??? < f(k) < ???
    f(k) = a(k)/b(k)*(d(k) + f(k+1))
    a(k) = 8*prdouct[6*k-5,6*k-3,6*k-1] <= 8*6^3 *k^3
    b(k) = k^3 * (-262537412640768000)
    d(k) = (545140134*k+13591409)

    abs f(k) = abs a(k)/b(k)*(d(k) + f(k+1))
        <= abs 8*6^3 /(-+-+++++++262537412640768000)*((545140134*k+13591409) + f(k+1))
        == K*((A*k+B) + abs f(k+1))
        == (K*A*k+K*B + K* abs f(k+1))
        <= (K*A*k+K*B + K*(K*A*(k+1)+K*B + K*abs f(k+2)))
        <= (K*A*k+K*B + K^2*A*(k+1)+K^2*B + K^2*f(k+2))
        <= SUM K^i*B {i<-1..} + SUM K^i*K*A*(k+i) {i<-0..}
        <= SUM K^i*B {i<-1..} + SUM K^i*K*A*k + K^i*K*A*i {i<-0..}
        <= B*SUM K^i {i<-1..} + k*A* SUM K^i {i<-1..} + K*A*SUM K^i*i {i<-0..}
        == sum1*(B+k*A) + sum2
        == sum1*(B+k*A) + sum1/(1-K)
        == k*(sum1*A) + sum1*B+sum1/(1-K)
        == k*(K/(1-K)*A) + K/(1-K)*B+K/(1-K)/(1-K)
        -- K := -K
        K = 8*6^3 /(262537412640768000)
        A = 545140134
        B = 13591409
    sum1 = SUM K^i {i<-1..} = K/(1-K)
    sum2 = SUM K^i*i {i<-0..}
    = SUM K^i*i {i<-1..}
    = SUM K^(i+1)*(i+1) {i<-0..}
    = SUM K^(i+1)*i+K^(i+1) {i<-0..}
    = K*SUM K^i*i {i<-0..} + SUM K^(i+1) {i<-0..}
    = K*sum2 + sum1
    sum2 = sum1/(1-K) = -6.581919059149153e-15

    abs f(k) <= k*(sum1*A) + (sum1*B+sum2)
        == k*(K/(1-K)*A) + K/(1-K)*B + K/(1-K)/(1-K)
        == 545140134 / 151931373055999 * k + 2064961583067035368591 / 23083142118681138916389888001
        ~~ 3.5880682378817955e-6 *k + 8.945756051971219e-8
-}
module SimpleContinuedFraction_of_pi__by_Chudnovsky_algorithm
    (the_continued_fraction_of_426880_by_sqrt10005
    ,the_pi_inputs
    )
where

import FloorSqrt
    (Sqrt_with_coeffs(..)
    ,continued_fraction_digits_of_sqrt_with_coeffs
    )


import UnsafeFromList
import Interval
import LinearFractionalTransformation

import SimpleContinuedFraction
import Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction
import State4BiLFT4StreamingAlgorithm_with (div_BiLFT)
import LinearFractionalTransformationIntervalPairs

import Control.Monad (forM_, mapM_)


the_continued_fraction_of_426880_by_sqrt10005 ()
    = unsafe_mkSimpleContinuedFraction h ts
    where
        (h:ts) = continued_fraction_digits_of_sqrt_with_coeffs Sqrt_with_coeffs
                    {the_N = 10005
                    ,times_of_sqrtN = 426880
                    ,offset_of_timed_sqrtN = 0
                    ,whole_denominator4offsetted_timed_sqrtN=1
                    }


the_pi_inputs () = LinearFractionalTransformationIntervalPairs
    (state0_mx, head bounds)
    (zip mx_ls $ tail bounds)
    where
        -- y = 13591409 + f(1)
        state0_mx = unsafe_fromList
            [1, 13591409
            ,0, 1
            ]
        -- f(k) = a(k)/b(k)*(d(k) + f(k+1)) = [a, a*d; 0 b]
        -- a(k) = 8*prdouct[6*k-5,6*k-3,6*k-1] <= 8*6^3 *k^3
        -- b(k) = k^3 * (-262537412640768000)
        -- d(k) = (545140134*k+13591409)

        mx_ls = [unsafe_fromList [a,a*d, 0,b] | (a,b,d) <- zip3 ak bk dk]
        ak = [8*(6*k-5)*(6*k-3)*(6*k-1) | k <- [1..]]
        bk = [k^3 * (-262537412640768000) | k <- [1..]]
        dk = [545140134*1+13591409, 545140134*2+13591409..]

        -- 545140134 / 151931373055999 * k + 2064961583067035368591 / 23083142118681138916389888001
        fbounds prev = r : fbounds r where
            r = prev + toRational 545140134 / 151931373055999
        prev_bound = (toRational 2064961583067035368591 / 23083142118681138916389888001)

        bounds = [Inside (-b) b | b <- fbounds prev_bound]


the_continued_fraction_digits_of_pi__by_Chudnovsky_algorithm ()
    = div_BiLFT oconfigure whole_input_lhs whole_input_rhs
    where
        oconfigure = Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction False
        whole_input_lhs = the_continued_fraction_of_426880_by_sqrt10005 ()
        whole_input_rhs = the_pi_inputs ()


main :: IO ()
main = do
    print "pi -> SimpleContinuedFraction by Chudnovsky_algorithm"
    mapM_ print . zip [0..] $ the_continued_fraction_digits_of_pi__by_Chudnovsky_algorithm ()
    -- print $ the_continued_fraction_digits_of_pi__by_Chudnovsky_algorithm ()
