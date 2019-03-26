{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}


module Container_static_instance__List where
import Container_base_static
import Container_base_op
import qualified Prelude as P
import Prelude hiding (length)

instance Iterable a [a] where
    iter = id
instance Container a [a]
instance Countable a [a]
instance OpIterLe a [a]
instance OpAnyElem a [a]
instance OpSafeUnSingleton a [a]
instance OpLenLe a [a]
instance OpNull a [a] where
    type NullResult [a] = Bool
    null_ex = P.null
instance UnsafeSized a [a] where
    unsafe_len = P.toInteger . P.length



----------
instance Eq a => OpMember a [a] where
    type MemberResult [a] = Bool
    member_ex = elem
----------
instance OpFilter a [a] where
    filter = P.filter
instance OpPartitionList a [a] where
    -- partition_list :: (a -> Maybe r) -> [a] -> ([r], [a])
    partition_list f ls = foldr g ([], []) ls where
        g a (rs, ls) = case f a of
            Just r -> (r:rs, ls)
            _ -> (rs, a:ls)

