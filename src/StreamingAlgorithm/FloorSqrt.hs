
{-

--------------------
https://stackoverflow.com/questions/10863132/composing-floor-and-sqrt-in-haskell
    flrt

--------------------
Periodic Continued Fraction
http://mathworld.wolfram.com/PeriodicContinuedFraction.html

n :: Integer
continued_fraction_of sqrt(n) = [a0] | [a0]++(ls++[2*a0])*(+oo)
    where
        ls :: [PInt]
        ls = reverse(ls)

[n is not square]:
    [all (\a-> 1<= a < 2*sqrt(n)) $ continued_fraction_of sqrt(n)]

length_of_period_of_continued_fraction_of sqrt(N) = O(ln N * N)

-}

module FloorSqrt
    (floor_sqrt
    ,ceil_sqrt
    ,floor_sqrt_ex
    ,ceil_sqrt_ex
    ,Sqrt_with_coeffs(..)
    ,floor_sqrt_with_coeffs
    ,maybe_half_standardize__Sqrt_with_coeffs
    ,struct_eq__Sqrt_with_coeffs
    ,maybe_inv_of_sqrt_with_coeffs
    ,negate_of_sqrt_with_coeffs
    ,continued_fraction_digits_of_sqrt_with_coeffs
    ,continued_fraction_digits_of_sqrt_with_coeffs__step
    )
    -- main
where

import qualified Data.Map.Strict as M
--import Control.Exception (assert)

ceil_sqrt :: Integer -> Integer
ceil_sqrt = snd . ceil_sqrt_ex
ceil_sqrt_ex :: Integer -> (Bool, Integer)
ceil_sqrt_ex n = (is_square, ceil_sqrt_n)
    where
        (is_square, floor_sqrt_n) = floor_sqrt_ex n
        ceil_sqrt_n = if is_square then floor_sqrt_n else floor_sqrt_n+1
floor_sqrt :: Integer -> Integer
floor_sqrt = snd . floor_sqrt_ex
floor_sqrt_ex :: Integer -> (Bool, Integer)
floor_sqrt_ex n = if (r2 <= n && n < (r+1)^2)
                then (r2==n, r)
                else error "floor_sqrt impl error"
    where
        r = if (0<=n) then flrt n else error "floor_sqrt (<0)"
        r2 = r^2


flrt :: Integer -> Integer  -- flrt x ≈ √x,  with  flrt x^2 ≤ x < flrt(x+1)^2
flrt x = approx (round . (sqrt::Double->Double) . fromInteger $ x)
   where approx r
            | ctrl <= x, (r+1)^2 > x  = r
            | otherwise               = approx $ r - diff
          where ctrl = r^2
                diff = (ctrl - x) // (2*r)    -- ∂/∂x x² = 2x

         a//b = a`div`b + if (a>0)==(b>0) then 1 else 0   -- always away from 0












data Sqrt_with_coeffs = Sqrt_with_coeffs
    {the_N :: Integer
    ,times_of_sqrtN :: Integer
    ,offset_of_timed_sqrtN :: Integer
    ,whole_denominator4offsetted_timed_sqrtN :: Integer
    }
    deriving (Show, Read)
    {-
        (sqrt(the_N)*times_of_sqrtN + offset_of_timed_sqrtN)/whole_denominator4offsetted_timed_sqrtN

        floor( (sqrt(N)*t+s)/d )
        = floor( (sqrt(N)*t+s)* sign d/abs d )
        = floor( (sqrt(N)*(t*sign d) + s* sign d)/abs d )
        = floor( (sqrt(N*t^2)*(sign (d*t)) + s* sign d)/abs d )
        = floor( floor(sqrt(N*t^2)*(sign (d*t)) + s* sign d)/abs d )
        = floor( (floor(sqrt(N*t^2)*(sign (d*t))) + s* sign d)/abs d )
        = floor( ((floor if sign (d*t) > 0 else negate.ceil)(sqrt(N*t^2)) + s* sign d)/abs d )
        = floor( ((floor_sqrt if sign (d*t) > 0 else negate.ceil_sqrt)((N*t^2)) + s* sign d)/abs d )
    -}
floor_sqrt_with_coeffs :: Sqrt_with_coeffs -> Integer
floor_sqrt_with_coeffs Sqrt_with_coeffs
    {the_N=_N
    ,times_of_sqrtN=time
    ,offset_of_timed_sqrtN=offset
    ,whole_denominator4offsetted_timed_sqrtN=d
    } = if _N == 0 then div offset d else
        if d > 0
        then f time offset d
        else f (-time) (-offset) (-d)
    where
        -- _D > 0
        f time offset _D = div numerator _D
            where
                numerator = (signed_xxx_sqrt (_N*time^2) + offset)
                signed_xxx_sqrt = if time > 0 then floor_sqrt
                                    else negate . ceil_sqrt


maybe_half_standardize__Sqrt_with_coeffs
    :: Sqrt_with_coeffs -> Maybe Sqrt_with_coeffs
maybe_half_standardize__Sqrt_with_coeffs Sqrt_with_coeffs
    {the_N = _N
    ,times_of_sqrtN = time
    ,offset_of_timed_sqrtN = offset
    ,whole_denominator4offsetted_timed_sqrtN = d
    }
    = if d == 0 then Nothing else Just result
    where
        _g = gcd d $ gcd time offset
        g = if d < 0 then -_g else _g
        d' = div d g
        time' = div time g
        offset' = div offset g

        result = Sqrt_with_coeffs
            {the_N=_N
            ,times_of_sqrtN=time'
            ,offset_of_timed_sqrtN=offset'
            ,whole_denominator4offsetted_timed_sqrtN=d'
            }

struct_eq__Sqrt_with_coeffs :: Sqrt_with_coeffs -> Sqrt_with_coeffs -> Bool
struct_eq__Sqrt_with_coeffs
    Sqrt_with_coeffs
        {the_N = n1
        ,times_of_sqrtN = t1
        ,offset_of_timed_sqrtN = o1
        ,whole_denominator4offsetted_timed_sqrtN = d1
        }
    Sqrt_with_coeffs
        {the_N = n2
        ,times_of_sqrtN = t2
        ,offset_of_timed_sqrtN = o2
        ,whole_denominator4offsetted_timed_sqrtN = d2
        }
    = n1==n2 && d1==d2 && t1==t2 && o1==o2


negate_of_sqrt_with_coeffs
    :: Sqrt_with_coeffs -> Sqrt_with_coeffs
negate_of_sqrt_with_coeffs Sqrt_with_coeffs
    {the_N=_N
    ,times_of_sqrtN=time
    ,offset_of_timed_sqrtN=offset
    ,whole_denominator4offsetted_timed_sqrtN=d
    }
    = Sqrt_with_coeffs
    {the_N=_N
    ,times_of_sqrtN=time
    ,offset_of_timed_sqrtN=offset
    ,whole_denominator4offsetted_timed_sqrtN= (-d)
    }
maybe_inv_of_sqrt_with_coeffs
    :: Sqrt_with_coeffs -> Maybe Sqrt_with_coeffs
    {-
        d/(sqrtN*t+offset)
        = d*(sqrtN*t-offset)/((sqrtN*t-offset)*(sqrtN*t+offset))
        = d*(sqrtN*t-offset)/(N*t^2-offset^2)
        = (sqrtN*(d*t)-(d*offset))/(N*t^2-offset^2)
    -}
maybe_inv_of_sqrt_with_coeffs Sqrt_with_coeffs
    {the_N=_N
    ,times_of_sqrtN=time
    ,offset_of_timed_sqrtN=offset
    ,whole_denominator4offsetted_timed_sqrtN=d
    }
    -- = if new_d == 0 then Nothing else Just result
    = result
    where
        new_d = _N*time^2 - offset^2
        new_time = d*time
        new_offset = -d*offset

        _result = Sqrt_with_coeffs
            {the_N=_N
            ,times_of_sqrtN=new_time
            ,offset_of_timed_sqrtN=new_offset
            ,whole_denominator4offsetted_timed_sqrtN=new_d
            }
        result = maybe_half_standardize__Sqrt_with_coeffs _result

        {-
        _g = gcd new_d $ gcd new_time new_offset
        g = if new_d < 0 then -_g else _g
        d' = div new_d g
        time' = div new_time g
        offset' = div new_offset g

        result = Sqrt_with_coeffs
            {the_N=_N
            ,times_of_sqrtN=time'
            ,offset_of_timed_sqrtN=offset'
            ,whole_denominator4offsetted_timed_sqrtN=d'
            }
        -}

continued_fraction_digits_of_sqrt_with_coeffs
    :: Sqrt_with_coeffs -> [Integer]
continued_fraction_digits_of_sqrt_with_coeffs_ex
    :: Sqrt_with_coeffs -> [(Sqrt_with_coeffs, Integer)]
continued_fraction_digits_of_sqrt_with_coeffs
    = map snd . continued_fraction_digits_of_sqrt_with_coeffs_ex
continued_fraction_digits_of_sqrt_with_coeffs_ex = f
    where
        step = continued_fraction_digits_of_sqrt_with_coeffs__step
        f x =   let (digit, maybe_inv) = step x
                in  (x, digit) : case maybe_inv of
                    Just inv -> f inv
                    Nothing -> []

continued_fraction_digits_of_sqrt_with_coeffs__step
    :: Sqrt_with_coeffs -> (Integer, Maybe Sqrt_with_coeffs)
continued_fraction_digits_of_sqrt_with_coeffs__step input@Sqrt_with_coeffs
    {the_N=_N
    ,times_of_sqrtN=time
    ,offset_of_timed_sqrtN=offset
    ,whole_denominator4offsetted_timed_sqrtN=d
    }
    -- d != 0
    = (output_digit, maybe_inv)
    where
        output_digit = floor_sqrt_with_coeffs input
        maybe_inv = maybe_inv_of_sqrt_with_coeffs Sqrt_with_coeffs
                    {the_N=_N
                    ,times_of_sqrtN=time
                    ,offset_of_timed_sqrtN=offset-d*output_digit
                    ,whole_denominator4offsetted_timed_sqrtN=d
                    }


main :: IO ()
main = do
    print "continued_fraction_digits_of_sqrt_with_coeffs 10/3"
    print $ continued_fraction_digits_of_sqrt_with_coeffs Sqrt_with_coeffs
        {the_N = 1
        ,times_of_sqrtN = 10
        ,offset_of_timed_sqrtN = 0
        ,whole_denominator4offsetted_timed_sqrtN=3
        }

    let _2sqrt2_less = Sqrt_with_coeffs_less
                        {the_N_ = 2
                        ,non_negative_times_of_sqrtN_ = 2
                        ,offset_of_timed_sqrtN_ = 0
                        }
        _2sqrt2 = sqrt_with_coeffs_less2sqrt_with_coeffs _2sqrt2_less
        sqrt2_less = Sqrt_with_coeffs_less
                        {the_N_ = 2
                        ,non_negative_times_of_sqrtN_ = 1
                        ,offset_of_timed_sqrtN_ = 0
                        }
    let _2_2sqrt2_less = Sqrt_with_coeffs_less
                        {the_N_ = 2
                        ,non_negative_times_of_sqrtN_ = 2
                        ,offset_of_timed_sqrtN_ = 2
                        }
        _1_sqrt2_less = Sqrt_with_coeffs_less
                        {the_N_ = 2
                        ,non_negative_times_of_sqrtN_ = 1
                        ,offset_of_timed_sqrtN_ = 1
                        }
    -- sqrt(2/3) = sqrt(6)/3
    let sqrt2_3 = Sqrt_with_coeffs
                    {the_N = 6
                    ,times_of_sqrtN = 1
                    ,offset_of_timed_sqrtN = 0
                    ,whole_denominator4offsetted_timed_sqrtN = 3
                    }
    -- 3*sqrt(2/5) = 3*sqrt(10)/5
    let _3sqrt2_5 = Sqrt_with_coeffs
                    {the_N = 10
                    ,times_of_sqrtN = 3
                    ,offset_of_timed_sqrtN = 0
                    ,whole_denominator4offsetted_timed_sqrtN = 5
                    }
    -- 3/2*sqrt(7/5) = 3*sqrt(35)/10
    let _3div2sqrt7div5 = Sqrt_with_coeffs
                    {the_N = 35
                    ,times_of_sqrtN = 3
                    ,offset_of_timed_sqrtN = 0
                    ,whole_denominator4offsetted_timed_sqrtN = 10
                    }
    -- 1/3*sqrt(11) = sqrt(11/9)
    let sqrt11_9 = Sqrt_with_coeffs
                    {the_N = 11
                    ,times_of_sqrtN = 1
                    ,offset_of_timed_sqrtN = 0
                    ,whole_denominator4offsetted_timed_sqrtN = 3
                    }
    print "PeriodSplittedDigits of 2*sqrt(2)"
    print $ take 1000 . show $ period_splitted_continued_fraction_digits_of_sqrt_with_coeffs__less _2sqrt2_less
    print "PeriodSplittedDigits of sqrt(2)"
    print $ take 1000 . show $ period_splitted_continued_fraction_digits_of_sqrt_with_coeffs__less sqrt2_less
    print "PeriodSplittedDigits of 2+2*sqrt(2)"
    print $ take 1000 . show $ period_splitted_continued_fraction_digits_of_sqrt_with_coeffs__less _2_2sqrt2_less
    print "PeriodSplittedDigits of 1+1*sqrt(2)"
    print $ take 1000 . show $ period_splitted_continued_fraction_digits_of_sqrt_with_coeffs__less _1_sqrt2_less
    print "PeriodSplittedDigits of sqrt(2/3)"
    print $ take 1000 . show $ period_splitted_continued_fraction_digits_of_sqrt_with_coeffs sqrt2_3
    print "PeriodSplittedDigits of 3*sqrt(2/5)"
    print $ take 1000 . show $ period_splitted_continued_fraction_digits_of_sqrt_with_coeffs _3sqrt2_5
    print "PeriodSplittedDigits of 3/2*sqrt(7/5)"
    print $ take 1000 . show $ period_splitted_continued_fraction_digits_of_sqrt_with_coeffs _3div2sqrt7div5
    print "PeriodSplittedDigits of 1/3*sqrt(11)"
    print $ take 1000 . show $ period_splitted_continued_fraction_digits_of_sqrt_with_coeffs sqrt11_9



sqrt_with_coeffs_less2sqrt_with_coeffs
    :: Sqrt_with_coeffs_less -> Sqrt_with_coeffs
sqrt_with_coeffs_less2sqrt_with_coeffs
    Sqrt_with_coeffs_less
        {the_N_ = _N
        ,non_negative_times_of_sqrtN_ = times
        ,offset_of_timed_sqrtN_ = offset
        }
    = Sqrt_with_coeffs
        {the_N = _N
        ,times_of_sqrtN = times
        ,offset_of_timed_sqrtN = offset
        ,whole_denominator4offsetted_timed_sqrtN = 1
        }
data Sqrt_with_coeffs_less = Sqrt_with_coeffs_less
    {the_N_ :: Integer
    ,non_negative_times_of_sqrtN_ :: Integer
    ,offset_of_timed_sqrtN_ :: Integer
    }
    deriving (Show, Read)
data PeriodSplittedDigits = PeriodSplittedDigits
    {non_periodic_digits:: [Integer]
    ,periodic_digits :: [Integer]
    }
    deriving (Show, Read)

newtype HalfStd__Sqrt_with_coeffs -- private for Eq/Ord only
    = Private_HalfStd__Sqrt_with_coeffs (Integer, Integer, Integer, Integer)
    deriving (Eq, Ord)
mkHalfStd__Sqrt_with_coeffs
    :: Sqrt_with_coeffs -> HalfStd__Sqrt_with_coeffs
mkHalfStd__Sqrt_with_coeffs
    = maybe err f . maybe_half_standardize__Sqrt_with_coeffs
  where
    err = (error "mkHalfStd__Sqrt_with_coeffs: d=0")
    f Sqrt_with_coeffs
        {the_N=_N
        ,times_of_sqrtN=time
        ,offset_of_timed_sqrtN=offset
        ,whole_denominator4offsetted_timed_sqrtN=d
        } = Private_HalfStd__Sqrt_with_coeffs (_N, time, offset, d)
unHalfStd__Sqrt_with_coeffs
    (Private_HalfStd__Sqrt_with_coeffs (_N, time, offset, d))
    = Sqrt_with_coeffs
        {the_N=_N
        ,times_of_sqrtN=time
        ,offset_of_timed_sqrtN=offset
        ,whole_denominator4offsetted_timed_sqrtN=d
        }




period_splitted_continued_fraction_digits_of_sqrt_with_coeffs__less
    :: Sqrt_with_coeffs_less -> PeriodSplittedDigits
period_splitted_continued_fraction_digits_of_sqrt_with_coeffs
    :: Sqrt_with_coeffs -> PeriodSplittedDigits
period_splitted_continued_fraction_digits_of_sqrt_with_coeffs
    = (f . continued_fraction_digits_of_sqrt_with_coeffs_ex)
  where
    err = error "period_splitted_continued_fraction_digits_of_sqrt_with_coeffs: d=0"
    -- f :: [(Sqrt_with_coeffs, digit)] -> PeriodSplittedDigits
    f pairs = PeriodSplittedDigits
        {non_periodic_digits=take i digits
        -- bug: ,periodic_digits=take j digits
        ,periodic_digits=take (j-i) $ drop i digits
        }
        where
            digits = map snd pairs
            (i, j) = g M.empty . map mkHalfStd__Sqrt_with_coeffs $ map fst pairs
    -- g :: state2index -> [HalfStd__Sqrt_with_coeffs] -> (end_of_non_periodic_digits, end_of_periodic_digits)
    g state2index (state:remaining_states)
        = case M.lookup state state2index of
            Nothing -> g (M.insert state len state2index) remaining_states
            Just i -> (i, len)
        where
            len = M.size state2index

period_splitted_continued_fraction_digits_of_sqrt_with_coeffs__less
    Sqrt_with_coeffs_less
        {the_N_ = _N
        ,non_negative_times_of_sqrtN_ = times
        ,offset_of_timed_sqrtN_ = offset
        }
    = if times < 0
        then error "period_splitted_continued_fraction_digits_of_sqrt_with_coeffs__less: non_negative_times_of_sqrtN_ < 0"
        else PeriodSplittedDigits
            {non_periodic_digits=non_periodic_digits
            ,periodic_digits=periodic_digits
            }
  where
    state_digit_pairs_0_oo
        = maybe undefined continued_fraction_digits_of_sqrt_with_coeffs_ex
        $ maybe_half_standardize__Sqrt_with_coeffs
        Sqrt_with_coeffs
            {the_N = _N
            ,times_of_sqrtN = times
            ,offset_of_timed_sqrtN = 0 -- instead of offset
            ,whole_denominator4offsetted_timed_sqrtN = 1
            }

    (non_periodic_digits, periodic_digits)
        = if fst_digit == 2*_the_non_periodic_digit
            then ([], fst_digit:init _periodic_digits)
            else ([fst_digit], _periodic_digits)
    fst_digit = _the_non_periodic_digit + offset
    (_the_non_periodic_digit, _periodic_digits)
        = case state_digit_pairs_0_oo of
            [] -> error "logic error: continued_fraction_digits_of_sqrt_with_coeffs_ex _ == []"

            -- continued_fraction_of sqrt(n) = [a0] | [a0]++(ls++[2*a0])*(+oo)

            -- [a0]
            [(state0, digit0)] -> (digit0, [])

            -- [a0]++(ls++[2*a0])*(+oo)
            -- [n*times is not square] ==>> [n >= 2][times!=0]
            -- [times >= 0] ==>> [times>0][a0 >= 1][a0<2*a0]
            -- periodic_digits = [a0]
            --  i.e. the first a0 not in periodic_digits
            --      , otherwise a0==2*a0
            ((state0, digit0)
             :state_digit_pairs_1_oo@((state1, digit1):_)
             )
                -> (digit0, f state1 (2*digit0) state_digit_pairs_1_oo)

    f state1 _2digit0 = g where
        g state_digit_pairs_ge1_oo = case state_digit_pairs_ge1_oo of
            ((state_ge1, digit_ge1)
             :state_digit_pairs_ge2_oo@((state_ge2, _):_)
             )
                -> if digit_ge1==_2digit0
                        -- bug: && struct_eq__Sqrt_with_coeffs state_ge1 state1
                        && struct_eq__Sqrt_with_coeffs state_ge2 state1
                    then [digit_ge1]
                    else digit_ge1 : g state_digit_pairs_ge2_oo
            [] -> error "state_digit_pairs_ge1_oo = []"








