
{-# LANGUAGE TypeFamilies #-}
module NewList
    (NewList(..)
    ,NewList__verify_each_element(..)
    )
where

import Data.Semigroup
import qualified Data.List as Ls
--import TheType
import ToList

type ElementType4NewList a = ElementType4ToList a
class ToList a => NewList a where
    -- for newtype XXX = XXX [DDD]
    unsafe_mkNewList :: [ElementType4NewList a] -> a
    maybe_mk_finite_NewList :: [ElementType4NewList a] -> Maybe a

    length_finite_NewList :: a -> Int
    null_NewList :: a -> Bool

    length_finite_NewList = Ls.length . toList
    null_NewList = Ls.null . toList

__box :: NewList__verify_each_element a => [ElementType4NewList a] -> a
__box = __used_by_instance_write_only__boxNewList_without_verify_each_element
class (NewList a, Monoid a, Semigroup a
    ) => NewList__verify_each_element a where

    {-# MINIMAL
        (verify_element4NewList
        ,  (__used_by_instance_write_only__boxNewList_without_verify_each_element
           |(uncons_NewList
            ,splitAt_NewList
            ,span_NewList
            )
          )
        )
        #-}
    __used_by_instance_write_only__boxNewList_without_verify_each_element
        :: [ElementType4NewList a] -> a
    __used_by_instance_write_only__boxNewList_without_verify_each_element
        = undefined

    -- m ~ TheType/Maybe/...
    verify_element4NewList :: m a -> ElementType4NewList a -> Bool


    uncons_NewList :: a -> Maybe (ElementType4NewList a, a)
    splitAt_NewList :: Int -> a -> (a, a)
    span_NewList :: (ElementType4NewList a -> Bool) -> a -> (a, a)

    uncons_NewList a = do
        (e, ls) <- Ls.uncons $ toList a
        return (e, __box ls)
    splitAt_NewList n a = case Ls.splitAt n $ toList a of
        (init_ls, tail_ls) -> (__box init_ls, __box tail_ls)
    span_NewList pred a = case Ls.span pred $ toList a of
        (init_ls, tail_ls) -> (__box init_ls, __box tail_ls)



    ------
    take_NewList :: Int -> a -> a
    drop_NewList :: Int -> a -> a
    takeWhile_NewList :: (ElementType4NewList a -> Bool) -> a -> a
    dropWhile_NewList :: (ElementType4NewList a -> Bool) -> a -> a
    break_NewList :: (ElementType4NewList a -> Bool) -> a -> (a, a)

    take_NewList n = fst . splitAt_NewList n
    drop_NewList n = snd . splitAt_NewList n
    takeWhile_NewList pred = fst . span_NewList pred
    dropWhile_NewList pred = snd . span_NewList pred
    break_NewList pred = span_NewList (not . pred)


    unsafe_cons_NewList :: ElementType4NewList a -> a -> a
    maybe_cons_NewList :: ElementType4NewList a -> a -> Maybe a
    unsafe_cons_NewList e a = unsafe_mkNewList [e] <> a
    maybe_cons_NewList e a = do
        es <- maybe_mk_finite_NewList [e]
        return $ es <> a
