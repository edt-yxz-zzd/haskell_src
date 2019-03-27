
{-# LANGUAGE TypeFamilies #-}
{-
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-}

module RadixDigitList
    (RadixDigitList() -- nothing
    ,unRadixDigitList
    ,unsafe_mkRadixDigitList
    ,maybe_mk_finite_RadixDigitList

    ,assert_RadixDigits
    ,are_finite_RadixDigits
    ,maybe_make_finite_RadixDigits
    ,is_RadixDigit
    ,is_RadixBase
    )
where


import Control.Exception(assert)
--import Data.Semigroup
--import NewList
import ToList


data RadixDigitList = Private_RadixDigitList Integer [Integer]
    deriving (Show, Eq)
unRadixDigitList (Private_RadixDigitList radix_base digits)
    = (radix_base, digits)

is_RadixBase :: Integer -> Bool
is_RadixBase = (2<=)
is_RadixDigit :: Integer -> Integer -> Bool
is_RadixDigit radix_base digit = (0 <= digit && digit < radix_base)
assert_RadixDigits :: Integer -> [Integer] -> [Integer]
assert_RadixDigits radix_base = f where
    f (h:ls) = assert (is_RadixDigit radix_base h) h : f ls
    f [] = []
are_finite_RadixDigits :: Integer -> [Integer] -> Bool
are_finite_RadixDigits = all . is_RadixDigit
maybe_make_finite_RadixDigits :: Integer -> [Integer] -> Maybe [Integer]
maybe_make_finite_RadixDigits radix_base ls
    = if are_finite_RadixDigits radix_base ls then Just ls else Nothing

unsafe_mkRadixDigitList :: Integer -> [Integer] -> RadixDigitList
unsafe_mkRadixDigitList radix_base_
    = Private_RadixDigitList radix_base . assert_RadixDigits radix_base
    where
        radix_base = assert (is_RadixBase radix_base_) radix_base_
maybe_mk_finite_RadixDigitList :: Integer -> [Integer] -> Maybe RadixDigitList
maybe_mk_finite_RadixDigitList radix_base_ ls_ = do
    ls <- maybe_make_finite_RadixDigits radix_base ls_
    return $ Private_RadixDigitList radix_base ls
    where
        radix_base = assert (is_RadixBase radix_base_) radix_base_


instance ToList RadixDigitList where
    type ElementType4ToList RadixDigitList = Integer
    toList (Private_RadixDigitList radix_base ls) = ls
{-
instance NewList RadixDigitList where
    unsafe_mkNewList = unsafe_mkRadixDigitList
    maybe_mk_finite_NewList = maybe_mk_finite_RadixDigitList

instance NewList__verify_each_element RadixDigitList where
    verify_element4NewList _ = (1<=)
    __used_by_instance_write_only__boxNewList_without_verify_each_element = Private_RadixDigitList

-}



