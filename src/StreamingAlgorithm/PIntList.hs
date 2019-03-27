
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PIntList
    (PIntList() -- nothing
    ,unPIntList
    ,unsafe_mkPIntList
    ,maybe_mk_finite_PIntList

    ,assert_PInts
    ,are_finite_PInts
    ,maybe_make_finite_PInts
    ,is_PInt
    )
where


import Control.Exception(assert)
import Data.Semigroup
import ToList
import NewList


newtype PIntList = Private_PIntList [Integer]
    deriving (Read, Show, Eq, Ord, Monoid, Semigroup)

is_PInt :: Integer -> Bool
is_PInt = (1<=)
assert_PInts :: [Integer] -> [Integer]
assert_PInts (h:ls) = assert (is_PInt h) h : assert_PInts ls
assert_PInts [] = []
are_finite_PInts :: [Integer] -> Bool
are_finite_PInts = all is_PInt
maybe_make_finite_PInts :: [Integer] -> Maybe [Integer]
maybe_make_finite_PInts ls = if are_finite_PInts ls then Just ls else Nothing

unPIntList :: PIntList -> [Integer]
unPIntList (Private_PIntList ls) = ls
unsafe_mkPIntList :: [Integer] -> PIntList
unsafe_mkPIntList = Private_PIntList . assert_PInts
maybe_mk_finite_PIntList :: [Integer] -> Maybe PIntList
maybe_mk_finite_PIntList ls = do
    ls <- maybe_make_finite_PInts ls
    return $ Private_PIntList ls


instance ToList PIntList where
    type ElementType4ToList PIntList = Integer
    toList = unPIntList
instance NewList PIntList where
    unsafe_mkNewList = unsafe_mkPIntList
    maybe_mk_finite_NewList = maybe_mk_finite_PIntList

instance NewList__verify_each_element PIntList where
    verify_element4NewList _ = is_PInt
    __used_by_instance_write_only__boxNewList_without_verify_each_element = Private_PIntList





