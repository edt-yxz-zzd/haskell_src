
--------- gcd
--------- addmod / mulmod / powmod / invmod / ginvmod (gcd = a*n + b*m)


--------- log / ln (i.e. log_e) / log2
-- log1p (i.e. log b (1+n)) / ln1p (ln (1+n)
-- floor_log / floor_ln / floor_log2...

class OpSafeFloorLogEx r b n where
    safe_floor_log_ex :: b -> n -> Maybe r
    unsafe_floor_log_ex :: b -> n -> r

    default safe_floor_log_ex :: OpFloorLogEx r b n => b -> n -> Maybe r
    safe_floor_log_ex n = Just . floor_log_ex n
    unsafe_floor_log_ex b = unjust . safe_floor_log_ex b
class OpFloorLogEx r b n where -- ^^~=
    floor_log_ex :: b -> n -> r

class OpSafeLogEx r b n where
    safe_log_ex :: b -> n -> Maybe r
    unsafe_log_ex :: b -> n -> r

    default safe_log_ex :: OpLogEx r b n => b -> n -> Maybe r
    safe_log_ex n = Just . log_ex n
    unsafe_log_ex b = unjust . safe_log_ex b
class OpLogEx r b n where -- ^~=
    -- log b n = r = log_b n
    log_ex :: b -> n -> r




-- pow / pow2 / exp (i.e. pow_e)
-- powm1 (pow b n - 1) / expm1 (i.e. exp n - 1)
-- floor_pow / floor_pow2 / floor_exp
-- floor_sqrt/floor_kth_root/floor_pow_inv/floor_exp_inv



class OpSafeFloorPowInvEx r b n where
    safe_floor_pow_inv_ex :: b -> n -> Maybe r
    unsafe_floor_pow_inv_ex :: b -> n -> r

    default safe_floor_pow_inv_ex
        :: OpFloorPowInvEx r b n => b -> n -> Maybe r
    safe_floor_pow_inv_ex n = Just . floor_pow_inv_ex n
    unsafe_floor_pow_inv_ex b = unjust . safe_floor_pow_inv_ex b
class OpFloorPowInvEx r b n where -- ^
    -- floor_pow_inv b n = r = floor $ b ** (1/n)
    -- floor_pow_inv b 2 = floor $ sqrt b
    -- floor_pow_inv b n = floor $ kth_root b n
    floor_pow_inv_ex :: b -> n -> r


class OpSafeFloorKthRootEx r b n where
    safe_floor_kth_root_ex :: b -> n -> Maybe r
    unsafe_floor_kth_root_ex :: b -> n -> r

    default safe_floor_kth_root_ex :: OpFloorKthRootEx r b n => b -> n -> Maybe r
    safe_floor_kth_root_ex n = Just . floor_kth_root_ex n
    unsafe_floor_kth_root_ex b = unjust . safe_floor_kth_root_ex b
class OpFloorKthRootEx r b n where -- ^
    -- floor_kth_root b n = r = floor $ b ** (1/n)
    floor_kth_root_ex :: b -> n -> r



class OpSafeFloorSqrtEx r b where
    safe_floor_sqrt_ex :: b -> Maybe r
    unsafe_floor_sqrt_ex :: b -> r

    default safe_floor_sqrt_ex :: OpFloorSqrtEx r b => b -> Maybe r
    safe_floor_sqrt_ex = Just . floor_sqrt_ex
    unsafe_floor_sqrt_ex = unjust . safe_floor_sqrt_ex
class OpFloorSqrtEx r b where -- ^
    -- floor_sqrt b = r = floor $ sqrt b = floor_kth_root b 2
    floor_sqrt_ex :: b -> r



-- b > 1, n > 0 ==>> r >= 0
unsafe_floor_log__Integer :: Integer -> Integer -> Integer
unsafe_floor_log2__Integer :: Integer -> Integer
unsafe_floor_log2__Integer = undefined
unsafe_floor_log__Integer = undefined
instance OpSafeFloorLogEx Integer Integer Integer where
    safe_floor_log_ex b n = justif (n > 0 && b > 1) $
        unsafe_floor_log__Integer b n



