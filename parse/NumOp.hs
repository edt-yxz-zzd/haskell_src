{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}



module NumOp where
import SeedUtils (unjust, map2, map2_2, justif, lift2, map3)
-- import Prelude as P
import qualified Prelude as P
import Prelude hiding (Num(..), gcd, Fractional(..))

infixl 6 +, @+@, !+!, ?+?
infixl 6 -, @-@, !-!, ?-?
infixl 7 *, @*@, !*!, ?*?
infixl 7 //, !//!, ?//?
infixl 7 %%, !%%!, ?%%?
infixl 7 /, !/!, ?/?

---------------------- add

class OpSafeAdd a where
    safe_add, (?+?) :: a -> a -> Maybe a
    unsafe_add, (!+!) :: a -> a -> a

    default safe_add :: OpAdd a => a -> a -> Maybe a
    default unsafe_add :: OpAdd a => a -> a -> a
    safe_add a = Just . add a
    -- unsafe_add a = unjust . safe_add a
    unsafe_add = add
    (!+!) = unsafe_add
    (?+?) = safe_add
class OpSafeAdd a => OpLimitedAdd a where
    limited_add, (@+@) :: a -> a -> a -- e.g. 1-3 = 0

    default limited_add :: OpAdd a => a -> a -> a
    limited_add = add
    (@+@) = limited_add
class OpLimitedAdd a => OpAdd a where
    add, (+) :: a -> a -> a
    -- add = unsafe_add
    (+) = add


instance OpSafeAdd Integer where
instance OpLimitedAdd Integer where
instance OpAdd Integer where
    add = (P.+)
instance OpSafeSub Integer where
instance OpLimitedSub Integer where
instance OpSub Integer where
    sub = (P.-)
instance OpSafeMul Integer where
instance OpLimitedMul Integer where
instance OpMul Integer where
    mul = (P.*)
















---------------------- sub

class OpSafeSub a where
    safe_sub, (?-?) :: a -> a -> Maybe a
    unsafe_sub, (!-!) :: a -> a -> a

    default safe_sub :: OpSub a => a -> a -> Maybe a
    default unsafe_sub :: OpSub a => a -> a -> a
    safe_sub a = Just . sub a
    -- unsafe_sub a = unjust . safe_sub a
    unsafe_sub = sub
    (!-!) = unsafe_sub
    (?-?) = safe_sub
class OpSafeSub a => OpLimitedSub a where
    limited_sub, (@-@) :: a -> a -> a -- e.g. 1-3 = 0

    default limited_sub :: OpSub a => a -> a -> a
    limited_sub = sub
    (@-@) = limited_sub
class OpLimitedSub a => OpSub a where
    sub, (-) :: a -> a -> a
    -- sub = unsafe_sub
    (-) = sub






















---------------------- mul

class OpSafeMul a where
    safe_mul, (?*?) :: a -> a -> Maybe a
    unsafe_mul, (!*!) :: a -> a -> a

    default safe_mul :: OpMul a => a -> a -> Maybe a
    default unsafe_mul :: OpMul a => a -> a -> a
    safe_mul a = Just . mul a
    -- unsafe_mul a = unjust . safe_mul a
    unsafe_mul = mul
    (!*!) = unsafe_mul
    (?*?) = safe_mul
class OpSafeMul a => OpLimitedMul a where
    limited_mul, (@*@) :: a -> a -> a -- e.g. 1-3 = 0

    default limited_mul :: OpMul a => a -> a -> a
    limited_mul = mul
    (@*@) = limited_mul
class OpLimitedMul a => OpMul a where
    mul, (*) :: a -> a -> a
    -- mul = unsafe_mul
    (*) = mul




---------------------- gcd / lcm / even / odd

class OpSafeGcd a where
    safe_gcd :: a -> a -> Maybe a
    unsafe_gcd :: a -> a -> a

    default safe_gcd :: OpGcd a => a -> a -> Maybe a
    default unsafe_gcd :: OpGcd a => a -> a -> a
    safe_gcd a = Just . gcd a
    unsafe_gcd = gcd
class OpSafeGcd a => OpGcd a where
    -- if Ord a ==>> gcd >= 0
    -- gcd a b == 0 <==> a == b == 0
    -- gcd 0 b == gcd b 0 == abs b
    -- g = gcd a b ==>> [g|a][g|b][[x|a][x|b] -->> [x|g]]
    gcd :: a -> a -> a
    default gcd :: OpGcdEx a => a -> a -> a
    gcd a b = let (g, _, _) = gcd_ex a b in g
class OpSafeGcd a => OpSafeGcdEx a where
    safe_gcd_ex :: a -> a -> Maybe (a, a, a)
    unsafe_gcd_ex :: a -> a -> (a, a, a)
    default safe_gcd_ex :: OpGcdEx a => a -> a -> Maybe (a, a, a)
    default unsafe_gcd_ex :: OpGcdEx a => a -> a -> (a, a, a)
    safe_gcd_ex a = Just . gcd_ex a
    unsafe_gcd_ex = gcd_ex
class (OpGcd a, OpSafeGcdEx a) => OpGcdEx a where
    -- gcd_ex a b = (g, ka, kb) ==>> g = ka*a + kb*b = gcd a b >= 0
    -- Ord =>
    --      b /= 0 ==>> 0 <= ka < abs(b/g)
    --      b == 0 ==>> kb == 0, ka in [-1,0,1]
    --      a == 0 ==>> ka == 0, kb in [-1,0,1]
    gcd_ex :: a -> a -> (a, a, a)

instance OpSafeGcd Integer where
instance OpGcd Integer where
    gcd = P.gcd
instance OpSafeGcdEx Integer where
instance OpGcdEx Integer where
    -- gcd_ex a b = (g, ka, kb) where
    gcd_ex a_ b_ = r' where
        (aa, ba, a, _, _, _) = f a_ b_
        r = if a == 0 then (0,0,0) else
            if a < 0 then (-a, -aa, -ba)
            else (a, aa, ba)
        f = raw_gcd_ex (0==) (P.-) (P.*) P.divMod 0 1
        (g, ka, kb) = r
        r' = if ka >= 0 then r else
             let (a_', b_') = if b_ < 0 then (-a_, -b_) else (a_, b_)
                 ka' = ka P.+ b_'
                 kb' = kb P.- a_'
                 g' = ka' P.* a_ P.+ kb' P.* b_
             in  if g' == g && ka' > 0 then (g, ka', kb')
                 else error "logic error@gcd_ex<Integer>"

{-
gcd_inputs :: P.Num a => [(a,a)]
gcd_inputs = [(0,0), (0,3), (3,0), (2,6), (6,2), (3,5), (5,3), (-3,5), (5, -3), (3,-5), (-5,3), (-3,-5), (-5,-3)]
gcd_ex_anss = [(0,0,0),(3,0,1),(3,1,0),(2,1,0),(2,0,1),(1,2,-1),(1,2,-3),(1,3,2),(1,2,3),(1,2,1),(1,1,2),(1,3,-2),(1,1,-2)]
gcd_ex__input_ans_ls = zip gcd_inputs gcd_ex_anss
gcd_ex_anss__calcInteger :: [(Integer, Integer, Integer)]
gcd_ex_anss__calcInteger = map (uncurry gcd_ex) gcd_inputs
-}
gcd_ex__input_ans_ls :: P.Num a => [((a,a), (a,a,a))]
gcd_ex__input_ans_ls =
    [((0,0),(0,0,0))
    ,((0,3),(3,0,1))
    ,((3,0),(3,1,0))
    ,((2,6),(2,1,0))
    ,((6,2),(2,0,1))
    ,((3,5),(1,2,-1))
    ,((5,3),(1,2,-3))
    ,((-3,5),(1,3,2))
    ,((5,-3),(1,2,3))
    ,((3,-5),(1,2,1))
    ,((-5,3),(1,1,2))
    ,((-3,-5),(1,3,-2))
    ,((-5,-3),(1,1,-2))
    ]
test_gcd_ex :: (Eq a, OpGcdEx a) => (Integer -> a) -> Bool
test_gcd_ex to = calcs == anss' where
    (inputs, anss) = unzip gcd_ex__input_ans_ls
    inputs' = map (map2 to) inputs
    anss' = map (map3 to) anss
    calcs = map (uncurry gcd_ex) inputs'
raw_gcd_ex
    -- is_zero      sub          mul          divmod
    :: (a->Bool) -> (a->a->a) -> (a->a->a) -> (a->a->(a,a))
    -- zero one  a_   b_
    -> a -> a -> a -> a
    --  aa ba a  ab bb b
    -> (a, a, a, a, a, a)
raw_gcd_ex is_zero sub mul divmod zero one a_ b_ = -- (ka,kb)*+-1
    f one zero a_ zero one b_ where
        submul x y z = sub x $ mul y z
        -- aa*a_ + ba*b_ = a
        -- ab*a_ + bb*b_ = b
        -- abs(s_gcd a b) = abs(s_gcd a_ b_)
        f aa ba a ab bb b
            | is_zero b =
                -- gcd = abs a
                -- (ska, skb, sg, pb, pa, 0)
                -- b_ /= 0 ==>> 0 <= abs ska < abs b_
                -- b_ == 0 ==>> (1, 0, a_, 0, 1, 0)
                -- a_ /= 0 ==>> 0 <= abs skb < abs a_
                -- a_ == 0, b_ /= 0 ==>> (0, 1, b_, 1, 0, 0)
                -- a_ /= 0, b_ /= 0, q*a_ == b_ ==>> (0, 1, b_, 1, -q, 0)
                -- abs sg == gcd a_ b_
                -- sg == 0 <==> a_ == b_ == 0 ==>> (1,0,0,0,1,0)
                -- pb*a_ + pa*b_ == 0
                -- sg /= 0 ==>> abs pa == abs (a_/sg) <= abs(a_)
                -- sg /= 0 ==>> abs pb == abs (b_/sg) <= abs(b_)
                -- abs pb <= abs b_
                (aa, ba, a, ab, bb, b) -- as +-1*(ka, kb, g, _, _, 0)
            | otherwise = let (q, r) = divmod a b
                              c = r
                              -- a = b*q+r
                              -- c = r = a - b*q
                              -- c = ac*a_ + bc*b_
                              --   = (aa-ab*q)*a_ + (ba-bb*q)*b_
                              -- ac = aa - ab*q
                              -- bc = ba - bb*q
                              ac = submul aa ab q
                              bc = submul ba bb q

                              -- abs c < abs b
                          in  f ab bb b ac bc c











class OpSafeFloorDivEx i n d -- | n d -> i
    where
    -- i = floor (n/d)
    safe_floor_div_ex, (?//?) :: n -> d -> Maybe i
    unsafe_floor_div_ex, (!//!) :: n -> d -> i

    default safe_floor_div_ex :: OpFloorDivEx i n d => n -> d -> Maybe i
    -- default unsafe_floor_div_ex :: OpFloorDivEx i n d => n -> d -> i
    safe_floor_div_ex n = Just . floor_div_ex n
    -- unsafe_floor_div_ex = floor_div_ex
    unsafe_floor_div_ex n = unjust . safe_floor_div_ex n
    (!//!) = unsafe_floor_div_ex
    (?//?) = safe_floor_div_ex
class OpSafeFloorDivEx i n d => OpFloorDivEx i n d where
    floor_div_ex, (//) :: n -> d -> i
    (//) = floor_div_ex

class OpSafeModEx r n d -- | n d -> r
    where
    -- r = n - d * floor (n/d) ==>> d > 0 -->> r >= 0
    safe_mod_ex, (?%%?) :: n -> d -> Maybe r
    unsafe_mod_ex, (!%%!) :: n -> d -> r

    default safe_mod_ex :: OpModEx r n d => n -> d -> Maybe r
    -- default unsafe_mod_ex :: OpModEx r n d => n -> d -> r
    safe_mod_ex n = Just . mod_ex n
    -- unsafe_mod_ex = mod_ex
    unsafe_mod_ex n = unjust . safe_mod_ex n
    (!%%!) = unsafe_mod_ex
    (?%%?) = safe_mod_ex
class OpSafeModEx r n d => OpModEx r n d where
    mod_ex, (%%) :: n -> d -> r
    (%%) = mod_ex

class (OpSafeModEx r n d, OpSafeFloorDivEx q n d)
    => OpSafeDivModEx q r n d where
    safe_divmod_ex :: n -> d -> (Maybe q, Maybe r)
    unsafe_divmod_ex :: n -> d -> (q, r)

    safe_divmod_ex n d = (safe_floor_div_ex n d, safe_mod_ex n d)
    unsafe_divmod_ex n d = map2_2 (unjust, unjust) $ safe_divmod_ex n d
class (OpModEx r n d, OpFloorDivEx q n d, OpSafeDivModEx q r n d)
    => OpDivModEx q r n d where
    divmod_ex :: n -> d -> (q, r)
    divmod_ex n d = (floor_div_ex n d, mod_ex n d)
class OpSafeDivEx r n d where
    safe_div_ex, (?/?) :: n -> d -> Maybe r
    unsafe_div_ex, (!/!) :: n -> d -> r

    default safe_div_ex :: OpDivEx r n d => n -> d -> Maybe r
    safe_div_ex n = Just . div_ex n
    unsafe_div_ex b = unjust . safe_div_ex b
    (?/?) = safe_div_ex
    (!/!) = unsafe_div_ex
class OpSafeDivEx r n d => OpDivEx r n d where -- /  *~=
    -- div n d = r = n/d
    div_ex, (/) :: n -> d -> r
    (/) = div_ex


instance OpSafeDivModEx Integer Integer Integer Integer where
instance OpSafeFloorDivEx Integer Integer Integer where
    safe_floor_div_ex n d = justif (0 /= d) (P.div n d)
instance OpSafeModEx Integer Integer Integer where
    safe_mod_ex n d = justif (0 /= d) (P.mod n d)
instance OpSafeDivEx Integer Integer Integer where
    safe_div_ex n d = let (q, r) = P.divMod n d in
        justif (0 /= d && 0 == r) q
instance OpSafeDivEx Rational Integer Integer where
    safe_div_ex n d =
        justif (0 /= d) (lift2 toRational (P./) n d)







class OpSafePowEx r b n where
    safe_pow_ex :: b -> n -> Maybe r
    unsafe_pow_ex :: b -> n -> r

    default safe_pow_ex :: OpPowEx r b n => b -> n -> Maybe r
    safe_pow_ex n = Just . pow_ex n
    unsafe_pow_ex b = unjust . safe_pow_ex b
class OpPowEx r b n where -- ^
    -- pow b n = r = b ** n
    -- 0 ** 0 = 1
    pow_ex :: b -> n -> r
instance OpSafePowEx Integer Integer Integer where
    safe_pow_ex b n = justif (n>=0) $ b^n












------------- unary
----------------------- abs

class OpSafeAbs a where
    safe_abs :: a -> Maybe a
    unsafe_abs :: a -> a

    default safe_abs :: OpAbs a => a -> Maybe a
    default unsafe_abs :: OpAbs a => a -> a
    safe_abs = Just . abs
    unsafe_abs = abs
class OpSafeAbs a => OpLimitedAbs a where
    limited_abs :: a -> a -- e.g. -3 = 0

    default limited_abs :: OpAbs a => a -> a
    limited_abs = abs
class OpLimitedAbs a => OpAbs a where
    abs :: a -> a

---------------------- neg

class OpSafeNeg a where
    safe_neg :: a -> Maybe a
    unsafe_neg :: a -> a

    default safe_neg :: OpNeg a => a -> Maybe a
    default unsafe_neg :: OpNeg a => a -> a
    safe_neg = Just . neg
    unsafe_neg = neg
class OpSafeNeg a => OpLimitedNeg a where
    limited_neg :: a -> a -- e.g. -3 = 0

    default limited_neg :: OpNeg a => a -> a
    limited_neg = neg
class OpLimitedNeg a => OpNeg a where
    neg :: a -> a

instance OpSafeNeg Integer where
instance OpLimitedNeg Integer where
instance OpNeg Integer where
    neg = P.negate

instance OpSafeAbs Integer where
instance OpLimitedAbs Integer where
instance OpAbs Integer where
    abs = P.abs





---------------------- inv

class OpSafeInv a where
    safe_inv :: a -> Maybe a
    unsafe_inv :: a -> a

    default safe_inv :: OpInv a => a -> Maybe a
    default unsafe_inv :: OpInv a => a -> a
    safe_inv = Just . inv
    unsafe_inv = inv
class OpSafeInv a => OpLimitedInv a where
    limited_inv :: a -> a -- e.g. -3 = 0

    default limited_inv :: OpInv a => a -> a
    limited_inv = inv
class OpLimitedInv a => OpInv a where
    inv :: a -> a


