
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}


module LinearFractionalTransformation
    (LinearFractionalTransformation(..)
    ,BiLinearFractionalTransformation(..)
    ,LinearTransformation(..)
    ,BiLinearTransformation(..)
    ,are_all_zeros
    ,zero_LinearFractionalTransformation
    ,zero_BiLinearFractionalTransformation
    )
where

import Data.Semigroup
import Callable
import PartialCallable
import Standardizable
import EqStandardizable
import ToList
import UnsafeFromList

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



