
{-
https://stackoverflow.com/questions/10863132/composing-floor-and-sqrt-in-haskell
-}

module FloorSqrt
    (floor_sqrt
    ,ceil_sqrt
    ,floor_sqrt_ex
    ,ceil_sqrt_ex
    ,Sqrt_with_coeffs(..)
    ,floor_sqrt_with_coeffs
    ,maybe_inv_of_sqrt_with_coeffs
    ,negate_of_sqrt_with_coeffs
    ,continued_fraction_digits_of_sqrt_with_coeffs
    ,continued_fraction_digits_of_sqrt_with_coeffs__step
    )
where

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
    = if new_d == 0 then Nothing else Just result
    where
        new_d = _N*time^2 - offset^2
        new_time = d*time
        new_offset = -d*offset
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

continued_fraction_digits_of_sqrt_with_coeffs
    :: Sqrt_with_coeffs -> [Integer]
continued_fraction_digits_of_sqrt_with_coeffs = f
    where
        step = continued_fraction_digits_of_sqrt_with_coeffs__step
        f x =   let (digit, maybe_inv) = step x
                in  digit : case maybe_inv of
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
