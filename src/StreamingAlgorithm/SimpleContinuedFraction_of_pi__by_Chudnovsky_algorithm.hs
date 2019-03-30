
{-


usage:
    runghc SimpleContinuedFraction_of_pi__by_Chudnovsky_algorithm.hs

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
        = call matrix[a(k), a(k)*d(k); 0, b(k)] f(k+1)
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


ver2:
    f(k) = a(k)/b(k)*(d(k) + f(k+1))
    y = 13591409 + f(1)
        = d(0) + f(1)
        = (d(0) + a(1)/b(1)*) (d(1) + a(2)/b(2)*) ...
        = II (d(k-1)+a(k)/b(k)*) {k<-1..}
        = LIMIT call II matrix[a(k), d(k-1)*b(k); 0, b(k)] {k<-1..N} (d(N) + f(N+1)) {N<-1..}
    d(k) = (545140134*k+13591409)
    abs f(N+1) = 3.5880682378817955e-6 *(N+1) + 8.945756051971219e-8
        <= N+1
    N >= 1
    d(N)+f(N+1) <= (545140134+1)*N+(13591409+1)
    d(N)+f(N+1) >= (545140134-1)*N+(13591409-1)


ver2_2: update interval
    y = 13591409 + f(1)
        = g(1)
    f(k) = a(k)/b(k)*(d(k) + f(k+1))
        = call matrix[a(k), a(k)*d(k); 0, b(k)] f(k+1)
        ver1
    g(k) = d(k-1) + f(k)
        = d(k-1) + a(k)/b(k)*g(k+1)
        = call matrix[a(k), d(k-1)*b(k); 0, b(k)] g(k+1)
        ver2
??? < g(k) < ??? ver2
    y = g(1)
    g(k) = d(k-1) + a(k)/b(k)*g(k+1)
        = call matrix[a(k), d(k-1)*b(k); 0, b(k)] g(k+1)
    a(k) = 8*(6*k-5)*(6*k-3)*(6*k-1)
    b(k) = k^3 * (-262537412640768000)
    d(k) = (545140134*k+13591409)
    B(k) = abs b(k)

    g1 = d0 - a1/B1*(d1 - a2/B2*g3)
        = (d0-a1*d1/B1) + (a1*a2/B1/B2)*g3
    k>=1
    g(k) = (d(k-1)-a(k)*d(k)/B(k)) + (a(k)*a(k+1)/B(k)/B(k+1))*g(k+2)
        = A(k) + C(k)*g(k+2)
        = A(k) + C(k)*(A(k+2) + C(k+2)*g(k+4))
        = A(k) + C(k)*A(k+2) + C(k)*C(k+2)*A(k+4) + ...
        = SUM A(i)*II C(j) {j<-k,k+2..i-2} {i<-k,k+2..}
    # see below: [proof of image range of A(k), C(k), V(k) of g(k)]
    A1 = A(1) <= Ak = A(k) < A(k+1) < A(+oo) = Aoo ~ k*D+E
    C1 = C(1) <= Ck = C(k) < C(k+1) < C(+oo) = Coo
    let V(k) = A(k)-k*D
    V1 = V(1) <= Vk = V(k) < V(k+1) < V(+oo) = Voo

    A(k) = k*D + V(k) < k*D + Voo
    g(k) >= Ak + Ck*Ak + Ck^2*Ak + ... = Ak * SUM Ck^i {i<-0..}
    g(k) < (k*D+Voo) + Coo*((k+2)*D+Voo) + Coo^2*((k+4)*D+Voo) + ...
        = (k*D+Voo) + Coo*(k*D+Voo) + Coo^2*(k*D+Voo) + ...
        + 0 + Coo*2*D + Coo^2*4*D + ...
        = (k*D+Voo) * SUM Coo^i {i<-0..} + 2*D * SUM i*Coo^i {i<-0..}
    [0 < Ck < 1]!
    SUM i*Coo^i {i<-0..} = Coo/(1-Coo)^2
    SUM Ck^i {i<-0..} = 1/(1-Ck)
    SUM Coo^i {i<-0..} = 1/(1-Coo)
    ==>>
    Ak/(1-Ck) <= g(k) < (k*D+Voo)/(1-Coo) + 2*D*Coo/(1-Coo)^2
    where
        D = 5963320012791692474198352/10939058860032000
        Voo = -10094865954426918685151/18991421632000
        Coo = 1/23083142118681442779136000000
        Ak = A(k) = (d(k-1)-a(k)*d(k)/B(k))
        Ck = C(k) = (a(k)*a(k+1)/B(k)/B(k+1))
            = (2*k - 1)*(2*k + 1)*(6*k - 5)*(6*k - 1)*(6*k + 1)*(6*k + 5)/(119663008743244599367041024000000*k**3*(k + 1)**3)
        -------
        a(k) = 8*(6*k-5)*(6*k-3)*(6*k-1)
        B(k) = k^3 * (262537412640768000)
        d(k) = (545140134*k+13591409)



-}
module SimpleContinuedFraction_of_pi__by_Chudnovsky_algorithm
    (the_continued_fraction_of_426880_by_sqrt10005
    ,the_pi_inputs
    ,the_continued_fraction_digits_of_pi__by_Chudnovsky_algorithm
    -----------
    ,the_pi_inputs__ver2_2__from_interval_refine_times
    ,the_pi_inputs__ver2_1
    ,the_pi_inputs__ver1
    ,the_continued_fraction_digits_of_pi__by_Chudnovsky_algorithm__from_interval_refine_times
    ,main
    ,main_i
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
import Data.Ratio

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

the_pi_inputs = the_pi_inputs__ver2_2

-- ver2_2: update interval
the_pi_inputs__ver2_2 ()
    = the_pi_inputs__ver2_2__from_interval_refine_times 2
the_pi_inputs__ver2_2__from_interval_refine_times
    :: Int -> LinearFractionalTransformationIntervalPairs
the_pi_inputs__ver2_2__from_interval_refine_times interval_refine_times
    = LinearFractionalTransformationIntervalPairs
        (head output_pair_1_oo)
        (tail output_pair_1_oo)
    where
        output_pair_1_oo = arg34s_k_oo_to_output_pair_k_oo arg34s_1_oo
        -- arg34s_1_oo = f . f . f $ _arg34s_1_oo
        arg34s_1_oo = f . f . f $ _arg34s_1_oo
          where
            f = arg34s_k_oo_to_better_arg34s_k_oo
            fs = replicate interval_refine_times f
            arg34s_k_oo_to_best_arg34s_k_oo = foldr (.) id fs
        _arg34s_1_oo = map arg123_to_arg34 arg123s_1_oo
        arg123s_1_oo = arg1_k_sub_1_arg12s_k_oo_to_arg123s_k_oo
                            arg1_0 arg12s_1_oo
        arg1_0 = arg1 0
        arg12s_1_oo = arg1s_k_sub_1_oo_to_arg12s_k_oo arg1s_0_oo
        arg1s_0_oo = map arg1 [0..]

        ---------------------------------------------

        arg34s_k_oo_to_output_pair_k_oo arg34s_k_oo@
            (arg34_k@
                    (arg3_k@
                        (mx_k, _lower_bound_k, _upper_bound_k)
                    ,arg4_k
                    )
            :arg34_k_add_1@
                    (arg3_k_add_1@
                        (_mx_k_add_1, lower_bound_k_add_1, upper_bound_k_add_1)
                    ,arg4_k_add_1
                    )
            :_
            )
            = output_pair_k
            : arg34s_k_oo_to_output_pair_k_oo (tail arg34s_k_oo)
          where
            output_pair_k = (mx_k, interval_k_add_1)
            interval_k_add_1 = Inside lower_bound_k_add_1 upper_bound_k_add_1


        -- g(k) = A(k) + C(k)*g(k+2)
        arg34s_k_oo_to_better_arg34s_k_oo
            arg34s_k_oo@
                (arg34_k@
                    (arg3_k@
                        (mx_k, lower_bound_k, upper_bound_k)
                    ,arg4_k@
                        (_Ak, _Ck)
                    )
                :arg34_k_add_1
                :arg34_k_add_2@
                    (arg3_k_add_2@
                        (mx_k_add_2, lower_bound_k_add_2, upper_bound_k_add_2)
                    ,arg4_k_add_2
                    )
                :arg34s_k_add_3_oo
                )
            = better_arg34_k
            : arg34s_k_oo_to_better_arg34s_k_oo (tail arg34s_k_oo)
          where
            better_arg34_k = (better_arg3_k, arg4_k)
            better_arg3_k = (mx_k
                            ,g_k_add_2_to_better_gk lower_bound_k_add_2
                            ,g_k_add_2_to_better_gk upper_bound_k_add_2
                            )
            -- g(k) = A(k) + C(k)*g(k+2)
            g_k_add_2_to_better_gk g_k_add_2 = _Ak + _Ck*g_k_add_2

        arg123_to_arg34 (arg1_k, arg2_k@(_Ak, _Bk, _Ck), arg3_k)
            = arg34_k
          where
            arg34_k = (arg3_k, arg4_k)
            arg4_k = (_Ak, _Ck)


        arg1_k_sub_1_arg12s_k_oo_to_arg123s_k_oo
            arg1_k_sub_1 arg12s_k_oo
            = arg12s_k_sub_1_oo_to_arg123s_k_oo arg12s_k_sub_1_oo
          where
            arg12s_k_sub_1_oo = arg12_k_sub_1 : arg12s_k_oo
            arg12_k_sub_1 = (arg1_k_sub_1, arg2_k_sub_1)
            arg2_k_sub_1 = undefined
        arg12s_k_sub_1_oo_to_arg123s_k_oo
            arg12s_k_sub_1_oo@
                (arg12_k_sub_1@
                    (arg1_k_sub_1@
                        (k_sub_1, a_k_sub_1, b_k_sub_1, d_k_sub_1)
                    ,arg2_k_sub_1
                    )
                :arg12s_k_oo@
                    (arg12_k@
                        (arg1_k@(k, ak, bk, dk)
                        ,arg2_k@(_Ak, _Bk, _Ck)
                        )
                    :arg12s_k_add_1_oo
                    )
                )
            = arg123_k : arg12s_k_sub_1_oo_to_arg123s_k_oo arg12s_k_oo
          where
            arg123_k = (arg1_k, arg2_k, arg3_k)
            arg3_k = (mx_k, lower_bound_k, upper_bound_k)
            mx_k = mx ak bk d_k_sub_1
            lower_bound_k = lower_bound _Ak _Ck
            upper_bound_k = upper_bound k

        arg1s_k_sub_1_oo_to_arg12s_k_oo
            arg1s_k_sub_1_oo@
                (arg1_k_sub_1@
                    (k_sub_1, a_k_sub_1, b_k_sub_1, d_k_sub_1)
                :arg1s_k_oo@
                    (arg1_k@(k, ak, bk, dk)
                    :(k_add_1, a_k_add_1, b_k_add_1, d_k_add_1)
                    :_
                    )
                )
            = arg12_k : arg1s_k_sub_1_oo_to_arg12s_k_oo arg1s_k_oo
          where
            arg12_k = (arg1_k, arg2_k)
            arg2_k = (_Ak, _Bk, _Ck)
            _Ak = _A ak _Bk d_k_sub_1 dk
            _Ck = _C ak a_k_add_1 _Bk _B_k_add_1
            _Bk = -bk
            _B_k_add_1 = -b_k_add_1
            -- a_k_add_1 = a k_add_1
            -- b_k_add_1 = b k_add_1
            -- d_k_sub_1 = d k_sub_1
            -- k_add_1 = k+1
            -- k_sub_1 = k-1
        arg1 k = (k, ak, bk, dk)
          where
            ak = a k
            bk = b k
            dk = d k

        -- y = LIMIT call II matrix[a(k), d(k-1)*b(k); 0, b(k)] {k<-1..N} (d(N) + f(N+1)) {N<-1..}
        mx :: Integer -> Integer -> Integer -> LinearFractionalTransformation Integer
        mx ak bk d_k_sub_1 = unsafe_fromList [ak,d_k_sub_1*bk, 0,bk]

        --
        -- a(k) = 8*prdouct[6*k-5,6*k-3,6*k-1] <= 8*6^3 *k^3
        -- b(k) = k^3 * (-262537412640768000)
        -- d(k) = (545140134*k+13591409)

        a,b,d :: Integer -> Integer
        a k = 8*(6*k-5)*(6*k-3)*(6*k-1)
        b k = k^3 * (-262537412640768000)
        d k = 545140134*k+13591409

        -- ver2_2
        -- ver2_2
        _D = toRational 5963320012791692474198352/10939058860032000
        _Voo = -10094865954426918685151/toRational 18991421632000
        _Coo = toRational 1/23083142118681442779136000000
        _A ak _Bk d_k_sub_1 dk = toRational d_k_sub_1 - ak*dk % _Bk
        _C ak a_k_add_1 _Bk _B_k_add_1 = (ak*a_k_add_1) % (_Bk*_B_k_add_1)
        lower_bound _Ak _Ck = _Ak/(1-_Ck)
        upper_bound k = (toRational k*_D+_Voo)/(1-_Coo) + upper_bound_offset
        upper_bound_offset = 2*_D*_Coo/(1-_Coo)^2
    {-
    Ak/(1-Ck) <= g(k) < (k*D+Voo)/(1-Coo) + 2*D*Coo/(1-Coo)^2
    where
        D = 5963320012791692474198352/10939058860032000
        Voo = -10094865954426918685151/18991421632000
        Coo = 1/23083142118681442779136000000
        Ak = A(k) = (d(k-1)-a(k)*d(k)/B(k))
        Ck = C(k) = (a(k)*a(k+1)/B(k)/B(k+1))
            = (2*k - 1)*(2*k + 1)*(6*k - 5)*(6*k - 1)*(6*k + 1)*(6*k + 5)/(119663008743244599367041024000000*k**3*(k + 1)**3)
        -------
        a(k) = 8*(6*k-5)*(6*k-3)*(6*k-1)
        B(k) = k^3 * (262537412640768000)
        d(k) = (545140134*k+13591409)
    -}


-- ver2_1
the_pi_inputs__ver2_1 () = LinearFractionalTransformationIntervalPairs
    (head pairs)
    (tail pairs)
    where
        pairs = zip mx_ls bounds
        -- y = LIMIT call II matrix[a(k), d(k-1)*b(k); 0, b(k)] {k<-1..N} (d(N) + f(N+1)) {N<-1..}
        mx_ls = [unsafe_fromList [a,d_1*b, 0,b] | (a,b,d_1) <- zip3 ak bk __dk_1]

        --
        -- a(k) = 8*prdouct[6*k-5,6*k-3,6*k-1] <= 8*6^3 *k^3
        -- b(k) = k^3 * (-262537412640768000)
        -- d(k) = (545140134*k+13591409)

        ak = [8*(6*k-5)*(6*k-3)*(6*k-1) | k <- [1..]]
        bk = [k^3 * (-262537412640768000) | k <- [1..]]
        __dk_1 = [545140134*0+13591409, 545140134*1+13591409..]

        -- ver2_1
        -- N >= 1
        -- d(N)+f(N+1) <= (545140134+1)*N+(13591409+1)
        -- d(N)+f(N+1) >= (545140134-1)*N+(13591409-1)
        lower_bounds, upper_bounds :: [Integer]
        lower_bounds = [(545140134-1)*1+(13591409-1), (545140134-1)*2+(13591409-1)..]
        upper_bounds = [(545140134+1)*1+(13591409+1), (545140134+1)*2+(13591409+1)..]
        bounds = [Inside lower_bound upper_bound
                | (lower_bound, upper_bound)
                <- zip  (map toRational lower_bounds)
                        (map toRational upper_bounds)
                ]



the_pi_inputs__ver1 () = LinearFractionalTransformationIntervalPairs
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




the_continued_fraction_digits_of_pi__by_Chudnovsky_algorithm__from_interval_refine_times i
    = div_BiLFT oconfigure whole_input_lhs whole_input_rhs
    where
        oconfigure = Configure4LFT4StreamingAlgorithm_SimpleContinuedFraction False
        whole_input_lhs = the_continued_fraction_of_426880_by_sqrt10005 ()
        whole_input_rhs = the_pi_inputs__ver2_2__from_interval_refine_times i


main_i :: Int -> IO ()
main_i i = do
    print "pi -> SimpleContinuedFraction by Chudnovsky_algorithm"
    mapM_ print . zip [0..] $ the_continued_fraction_digits_of_pi__by_Chudnovsky_algorithm__from_interval_refine_times i

main :: IO ()
main = do
    print "pi -> SimpleContinuedFraction by Chudnovsky_algorithm"
    mapM_ print . zip [0..] $ the_continued_fraction_digits_of_pi__by_Chudnovsky_algorithm ()
    -- print $ the_continued_fraction_digits_of_pi__by_Chudnovsky_algorithm ()
