
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

BiLinearFractionalTransformation:
let z x y = (A*z'' x y + B)/(C*z'' x y + D) = F<a,b,c,d; e,f,g,h> x y
    x = (A*x''+B)/(C*x''+D)
    y = (A*y''+B)/(C*y''+D)
z'' x y = F<-D*a+B*e, -D*b+B*f, -D*c+B*g, -D*d+B*h
            ; C*a-A*e, C*b-A*f, C*c-A*g, C*d-A*h> x y
    (*F4*) # arbitray transform on output-side
z x y = z''' x'' y = ???
        = F<a*A+c*C, b*A+d*C, c*D+a*B, d*D+b*B
           ;e*A+g*C, f*A+h*C, g*D+e*B, h*D+f*B> x'' y
    (*F5*) # arbitray transform on input-x
z x y = z'''' x y'' = ???
        = F<a*A+b*C, a*B+b*D, c*A+d*C, c*B+d*D
            ;e*A+f*C, e*B+f*D, g*A+h*C, g*B+h*D> x y''
    (*F6*) # arbitray transform on input-y

-}

module LinearFractionalTransformation
    (LinearFractionalTransformation(..)
    ,BiLinearFractionalTransformation(..)
    ,LinearTransformation(..)
    ,BiLinearTransformation(..)
    ,are_all_zeros
    ,zero_LinearFractionalTransformation
    ,zero_BiLinearFractionalTransformation
    --------------
    ,interval_transformation_LFT
    ,interval_transformation_ex_BiLFT
    ,output_matrix_mul_BiLFT
    ,inv_output_matrix_mul_BiLFT
        -- (inv_output_matrix)_mul_(BiLFT)
    ,mul_fst_input_matrix_BiLFT
    ,mul_snd_input_matrix_BiLFT
    --------------
    ,the_mul_matrix_BiLFT
    ,the_div_matrix_BiLFT
    ,the_add_matrix_BiLFT
    ,the_sub_matrix_BiLFT
    )
where


import Interval
import IntervalEx
import Interval4
    (cycle_preserved_transform2interval_transfrom
    ,cycle_preserved_bitransform2interval_bitransfrom_ex
    ,CyclePreservedTransform(..)
    ,CyclePreservedBiTransform(..)
    )


import Callable
import PartialCallable
import Standardizable
import EqStandardizable
import ToList
import UnsafeFromList
import Data.Semigroup

-- not output
pattern LinearFractionalTransformation_
        :: a -> a -> a -> a -> LinearFractionalTransformation a
pattern LinearFractionalTransformation_
            {up_left, up_right, down_left, down_right}
        = LinearFractionalTransformation
            (LinearTransformation up_left up_right)
            (LinearTransformation down_left down_right)

data LinearTransformation a = LinearTransformation a a
    -- a*x+b
    deriving (Read, Show)
data BiLinearTransformation a = BiLinearTransformation a a a a
    -- a*x*y+b*x+c*y+d
    deriving (Read, Show)
instance ToList (LinearTransformation a) where
    type ElementType4ToList (LinearTransformation a) = a
    toList (LinearTransformation a b) = [a,b]
instance ToList (BiLinearTransformation a) where
    type ElementType4ToList (BiLinearTransformation a) = a
    toList (BiLinearTransformation a b c d) = [a,b,c,d]
instance ToList (LinearFractionalTransformation a) where
    type ElementType4ToList (LinearFractionalTransformation a) = a
    toList (LinearFractionalTransformation up down)
        = toList up ++ toList down
instance ToList (BiLinearFractionalTransformation a) where
    type ElementType4ToList (BiLinearFractionalTransformation a) = a
    toList (BiLinearFractionalTransformation up down)
        = toList up ++ toList down

instance UnsafeFromList (LinearTransformation a) where
    unsafe_fromList [a,b] = LinearTransformation a b
    unsafe_fromList _ = undefined
instance UnsafeFromList (BiLinearTransformation a) where
    unsafe_fromList [a,b,c,d] = BiLinearTransformation a b c d
    unsafe_fromList _ = undefined
instance UnsafeFromList (LinearFractionalTransformation a) where
    unsafe_fromList [a,b,c,d] = LinearFractionalTransformation
        (LinearTransformation a b)
        (LinearTransformation c d)
    unsafe_fromList _ = undefined
instance UnsafeFromList (BiLinearFractionalTransformation a) where
    unsafe_fromList [a,b,c,d,e,f,g,h] = BiLinearFractionalTransformation
        (BiLinearTransformation a b c d)
        (BiLinearTransformation e f g h)
    unsafe_fromList _ = undefined

data LinearFractionalTransformation a
    = LinearFractionalTransformation
        {upLinearTransformation :: LinearTransformation a
        ,downLinearTransformation :: LinearTransformation a
        }
    deriving (Read, Show)
    {-
    matrix[ul, ur; dl, dr](x) = (ul*x+ur)/(dl*x+dr)
    I = matrix[1,0; 0,1]
    I(x) = (1*x+0)/(0*x+1) = x
    -}

data BiLinearFractionalTransformation a
    = BiLinearFractionalTransformation
        {upBiLinearTransformation :: BiLinearTransformation a
        ,downBiLinearTransformation :: BiLinearTransformation a
        }
        deriving (Read, Show)


instance Num a => Callable (LinearTransformation a) where
    type InputType4Callable (LinearTransformation a) = a
    type OutputType4Callable (LinearTransformation a) = a
    call (LinearTransformation a b) x = a*x + b
instance Num a => Callable (BiLinearTransformation a) where
    type InputType4Callable (BiLinearTransformation a) = (a,a)
    type OutputType4Callable (BiLinearTransformation a) = a
    call (BiLinearTransformation a b c d) (x,y) = a*x*y + b*x + c*y + d

instance Real a => Callable (LinearFractionalTransformation a) where
    type InputType4Callable (LinearFractionalTransformation a) = Rational
    type OutputType4Callable (LinearFractionalTransformation a) = Maybe Rational
    call (LinearFractionalTransformation up down) x
        = if denominator == 0 then Nothing
            else Just $ numerator/denominator
        where
            numerator  = f up
            denominator = f down
            f coeffs = call (g coeffs) x
            g (LinearTransformation a b)
                = LinearTransformation
                    (toRational a)
                    (toRational b)

instance Real a => Callable (BiLinearFractionalTransformation a) where
    type InputType4Callable (BiLinearFractionalTransformation a) = (Rational, Rational)
    type OutputType4Callable (BiLinearFractionalTransformation a) = Maybe Rational
    call (BiLinearFractionalTransformation up down) xy
        = if denominator == 0 then Nothing
            else Just $ numerator/denominator
        where
            numerator  = f up
            denominator = f down
            f coeffs = call (g coeffs) xy
            g (BiLinearTransformation a b c d)
                = BiLinearTransformation
                    (toRational a)
                    (toRational b)
                    (toRational c)
                    (toRational d)


instance Real a => PartialCallable (LinearFractionalTransformation a) where
    type PartialOutputType4Callable (LinearFractionalTransformation a) = Rational
instance Real a => PartialCallable (BiLinearFractionalTransformation a) where
    type PartialOutputType4Callable (BiLinearFractionalTransformation a) = Rational


{-
instance (Integral a
    ) => Standardizable (LinearFractionalTransformation a) where
    standardize LinearFractionalTransformation_
            {up_left=ul, up_right=ur, down_left=dl, down_right=dr}
        = if (dl == 0 && dr == 0) then _mkLFT (0,0,0,0) else
            let g = gcd ul . gcd ur $ gcd dl dr
                -- sign of (dl, dr) <- [0+, +0, ++, +-]
                to_neg = if dr < 0 then dl <= 0 else dl < 0
                g' = if to_neg then -g else g
            in  _mkLFT . _divLFT g' $ (ul, ur, dl, dr)
_unLFT LinearFractionalTransformation_
    {up_left=ul, up_right=ur, down_left=dl, down_right=dr}
    = (ul, ur, dl, dr)
_mkLFT (ul, ur, dl, dr) = LinearFractionalTransformation_
    {up_left=ul, up_right=ur, down_left=dl, down_right=dr}
_divLFT g (ul, ur, dl, dr) = (div ul g, div ur g, div dl g, div dr g)
_negLFT (ul, ur, dl, dr) = (-ul, -ur, -dl, -dr)
-}

gcds
    :: (ToList a, Integral (ElementType4ToList a))
    => a -> ElementType4ToList a
gcds = foldr gcd 0 . toList
divs
    :: (UnsafeFromList a, Integral (ElementType4ToList a))
    => a -> ElementType4ToList a -> a
divs a divisor
    = unsafe_fromList [div n divisor | n <- toList a]
zeros
    :: (UnsafeFromList a, Num (ElementType4ToList a))
    => Integer -> a
zeros n = unsafe_fromList [0 | _ <- [1..n]]

zero_LinearFractionalTransformation
    :: Num a => LinearFractionalTransformation a
zero_BiLinearFractionalTransformation
    :: Num a => BiLinearFractionalTransformation a
zero_LinearFractionalTransformation = zeros 4
zero_BiLinearFractionalTransformation = zeros 8
are_all_zeros
    :: (ToList a, Real (ElementType4ToList a))
    => a -> Bool
are_all_zeros a = all (0==) $ toList a

instance (Integral a
    ) => Standardizable (LinearFractionalTransformation a) where
    standardize (LinearFractionalTransformation up down)
        = if gDOWN == 0
            then zero_LinearFractionalTransformation
            else LinearFractionalTransformation
                    (divs up g)
                    (divs down g)
        where
            gUP = gcds up
            gDOWN = gcds down
            neg = _nums2neg . toList
            g_ = gcd gUP gDOWN
            g = if neg down then -g_ else g_
instance (Integral a
    ) => Standardizable (BiLinearFractionalTransformation a) where
    standardize (BiLinearFractionalTransformation up down)
        = if gDOWN == 0
            then zero_BiLinearFractionalTransformation
            else BiLinearFractionalTransformation
                    (divs up g)
                    (divs down g)
        where
            gUP = gcds up
            gDOWN = gcds down
            neg = _nums2neg . toList
            g_ = gcd gUP gDOWN
            g = if neg down then -g_ else g_

_to_neg :: Real a => a -> Bool -> Bool
_to_neg x b = case compare x 0 of
    LT -> True
    EQ -> b
    GT -> False
_nums2neg :: Real a => [a] -> Bool
_nums2neg = foldr _to_neg undefined



instance (Integral a
    ) => EqStandardizable (LinearFractionalTransformation a) where
    nonstd_eq lhs rhs = toList lhs == toList rhs
instance (Integral a
    ) => EqStandardizable (BiLinearFractionalTransformation a) where
    nonstd_eq lhs rhs = toList lhs == toList rhs



instance Num a => Semigroup (LinearFractionalTransformation a) where
    (<>) = mappend
instance Num a => Monoid (LinearFractionalTransformation a) where
    mempty = LinearFractionalTransformation_
        {up_left=1, up_right=0, down_left=0, down_right=1}

    mappend
        LinearFractionalTransformation_
            {up_left=ul, up_right=ur, down_left=dl, down_right=dr}
        LinearFractionalTransformation_
            {up_left=ul', up_right=ur', down_left=dl', down_right=dr'}
        = LinearFractionalTransformation_
            {up_left  =ul*ul' + ur*dl', up_right  =ul*ur' + ur*dr'
            ,down_left=dl*ul' + dr*dl', down_right=dl*ur' + dr*dr'
            }





-----------------------------------
is_constant_LFT :: Real a => LinearFractionalTransformation a -> Bool
-- matrix[0,_;0,_]
is_constant_LFT mx = a==0 && c==0
    where
        [a,b,c,d] = toList mx

interval_transformation_LFT
    :: (Real a)
    => LinearFractionalTransformation a
    -> IntervalEx Rational -> IntervalEx Rational
interval_transformation_LFT mx
    = if is_constant
        then const $ case may_div b d of
            Just r -> IntervalEx (Inside r r)
            Nothing -> Inside_oo_oo
        else cycle_preserved_transform2interval_transfrom
                $ CyclePreservedTransform f
    where
        [a,b,c,d] = toList mx
        is_constant = a==0 && c==0

        f (Just r) = call mx r
        f Nothing = may_div a c

        may_div n d = if d == 0
                        then Nothing
                        else (Just $ toRational n / toRational d)



interval_transformation_ex_BiLFT
    :: forall a. (Real a)
    => BiLinearFractionalTransformation a
    -> IntervalEx Rational -> IntervalEx Rational
    -> (IntervalEx Rational, Bool)
interval_transformation_ex_BiLFT mx
    = if is_constant
        then \_ _ -> (result_when_const, False)
        else cycle_preserved_bitransform2interval_bitransfrom_ex
                $ CyclePreservedBiTransform ff
    where
        [a,b,c,d  ,e,f,g,h] = toList mx
        is_constant = all (0==) [a,b,c  ,e,f,g]

        mx__x_oo, mx__y_oo :: forall. LinearFractionalTransformation a
            -- ScopedTypeVariables
        mx__x_oo = unsafe_fromList [a,b, e,f]
        mx__y_oo = unsafe_fromList [a,c, e,g]

        ff (Just x) (Just y) = call mx (x,y)
        ff Nothing (Just y) = call mx__x_oo y
        ff (Just x) Nothing = call mx__y_oo x
        ff Nothing Nothing = may_div a e

        may_div n d = if d == 0
                        then Nothing
                        else (Just $ toRational n / toRational d)

        result_when_const = case may_div d h of
            Just r -> IntervalEx (Inside r r)
            Nothing -> Inside_oo_oo





-------------------------------------------


{-
inv matrix[A,B;C,D] ~~ matrix[-D,B;C,-A]
matrix[A,B;C,D] * matrix[-D,B;C,-A] = matrix[-AD+BC,0; 0, CB-AD]
    = I*(-AD+BC) ~~ I
 -}
output_matrix_mul_BiLFT, inv_output_matrix_mul_BiLFT
    :: Num a
    => LinearFractionalTransformation a
    -> BiLinearFractionalTransformation a
    -> BiLinearFractionalTransformation a
    -- (*F4*)
{-
z'' x y = F<-D*a+B*e, -D*b+B*f, -D*c+B*g, -D*d+B*h
            ; C*a-A*e, C*b-A*f, C*c-A*g, C*d-A*h> x y
-}
output_matrix_mul_BiLFT mx bimx = bimx' where
    [a,b,c,d ,e,f,g,h] = toList bimx
    [_A,_B,_C,_D] = toList mx
    bimx' = unsafe_fromList
                [-_D*a+_B*e, -_D*b+_B*f, -_D*c+_B*g, -_D*d+_B*h
                ,_C*a-_A*e, _C*b-_A*f, _C*c-_A*g, _C*d-_A*h]
inv_output_matrix_mul_BiLFT inv_mx bimx = bimx' where
    [a,b,c,d ,e,f,g,h] = toList bimx
    [_A,_B,_C,_D] = toList inv_mx
    bimx' = unsafe_fromList
                [_A*a+_B*e, _A*b+_B*f, _A*c+_B*g, _A*d+_B*h
                ,_C*a+_D*e, _C*b+_D*f, _C*c+_D*g, _C*d+_D*h]


mul_fst_input_matrix_BiLFT
    :: Num a
    => BiLinearFractionalTransformation a
    -> LinearFractionalTransformation a
    -> ()
    -> BiLinearFractionalTransformation a
    -- (*F5*)
{-
z x y = z''' x'' y = ???
        = F<a*A+c*C, b*A+d*C, c*D+a*B, d*D+b*B
           ;e*A+g*C, f*A+h*C, g*D+e*B, h*D+f*B> x'' y
-}
mul_fst_input_matrix_BiLFT bimx mx () = bimx' where
    [a,b,c,d ,e,f,g,h] = toList bimx
    [_A,_B,_C,_D] = toList mx
    bimx' = unsafe_fromList
                [a*_A+c*_C, b*_A+d*_C, c*_D+a*_B, d*_D+b*_B
                ,e*_A+g*_C, f*_A+h*_C, g*_D+e*_B, h*_D+f*_B]


mul_snd_input_matrix_BiLFT
    :: Num a
    => BiLinearFractionalTransformation a
    -> ()
    -> LinearFractionalTransformation a
    -> BiLinearFractionalTransformation a
    -- (*F6*)
{-
z x y = z'''' x y'' = ???
        = F<a*A+b*C, a*B+b*D, c*A+d*C, c*B+d*D
            ;e*A+f*C, e*B+f*D, g*A+h*C, g*B+h*D> x y''
-}
mul_snd_input_matrix_BiLFT bimx () mx = bimx' where
    [a,b,c,d ,e,f,g,h] = toList bimx
    [_A,_B,_C,_D] = toList mx
    bimx' = unsafe_fromList
                [a*_A+b*_C, a*_B+b*_D, c*_A+d*_C, c*_B+d*_D
                ,e*_A+f*_C, e*_B+f*_D, g*_A+h*_C, g*_B+h*_D]



-------------------------
the_mul_matrix_BiLFT :: Num a => BiLinearFractionalTransformation a
the_div_matrix_BiLFT :: Num a => BiLinearFractionalTransformation a
the_add_matrix_BiLFT :: Num a => BiLinearFractionalTransformation a
the_sub_matrix_BiLFT :: Num a => BiLinearFractionalTransformation a

the_mul_matrix_BiLFT = unsafe_fromList
    [1,0,0,0
    ,0,0,0,1
    ]
the_div_matrix_BiLFT = unsafe_fromList
    [0,1,0,0
    ,0,0,1,0
    ]
the_add_matrix_BiLFT = unsafe_fromList
    [0,1,1,0
    ,0,0,0,1
    ]
the_sub_matrix_BiLFT = unsafe_fromList
    [0,1,-1,0
    ,0,0,0,1
    ]

