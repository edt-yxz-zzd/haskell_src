{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RoleAnnotations #-}
--{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ImplicitParams #-}


module ADT.ISemigroupBy
where
import ADT.IPair
import Data.Semigroup hiding (Sum, Product)
import Data.Proxy
import GHC.Types (type (~~))
import Data.Kind (type (*))
import Seed.ProxyOps (last1P, withBy, withBy_)
import Seed.By
import Seed.OpTest

--type GetKind (a :: ka) = ka
--type role Semigroup phantom representational
--class ISemigroupBy (by :: k) a where
class ISemigroupBy (by :: *) a where
    -- a *? (b *? c) === (a *? b) *? c
    mulBy :: proxy by -> a -> a -> a
    mulBy = withBy (*?)
    (*?) :: (?by :: proxy by) => a -> a -> a
    (*?) = withBy_ mulBy
    {-# MINIMAL mulBy | (*?) #-}
    {- exist mul_err mul_id && mul_id == mul_err ==>> G = {mul_id} -}
infixr 6 *?



----------------------- Zero
--  zero for *?
--  infinite for +?
--  so I call it Error -- sink??
--  ArrowZero ~~~ LeftError
class (ISemigroupBy by a) => ISemigroupLeftErrorBy by a where
    -- left_errorBy_ *? a === left_errorBy_
    left_errorBy :: proxy by -> a
    left_errorBy_ :: (?by :: proxy by) => a
    left_errorBy = withBy left_errorBy_
    left_errorBy_ = withBy_ left_errorBy
    {-# MINIMAL left_errorBy | left_errorBy_ #-}
class (ISemigroupBy by a) => ISemigroupRightErrorBy by a where
    -- right_errorBy_ === a *? right_errorBy_
    right_errorBy :: proxy by -> a
    right_errorBy_ :: (?by :: proxy by) => a
    default right_errorBy_
        :: (ISemigroupErrorBy by a, (?by :: proxy by)) => a
    right_errorBy = withBy right_errorBy_
    right_errorBy_ = errorBy_
class (ISemigroupLeftErrorBy by a, ISemigroupRightErrorBy by a)
    => ISemigroupErrorBy by a where
    -- errorBy_ *? a === errorBy_ === a *? errorBy_
    errorBy :: proxy by -> a
    errorBy_ :: (?by :: proxy by) => a
    errorBy = left_errorBy
    errorBy_ = left_errorBy_
instance (ISemigroupLeftErrorBy by a, ISemigroupRightErrorBy by a)
    => ISemigroupErrorBy by a where

----------------------- Identity
-- 1 for *?
-- 0 for +?
class (ISemigroupBy by a) => ISemigroupLeftIdentityBy by a where
    -- left_identityBy_ *? a === a
    left_identityBy :: proxy by -> a
    left_identityBy_ :: (?by :: proxy by) => a
    left_identityBy = withBy left_identityBy_
    left_identityBy_ = withBy_ left_identityBy
    {-# MINIMAL left_identityBy | left_identityBy_ #-}
class (ISemigroupBy by a) => ISemigroupRightIdentityBy by a where
    -- a === a *? right_identityBy_
    right_identityBy :: proxy by -> a
    right_identityBy_ :: (?by :: proxy by) => a
    default right_identityBy_
        :: (ISemigroupIdentityBy by a, (?by :: proxy by)) => a
    right_identityBy = withBy right_identityBy_
    right_identityBy_ = identityBy_
class (ISemigroupLeftIdentityBy by a, ISemigroupRightIdentityBy by a)
    => ISemigroupIdentityBy by a where
    -- identityBy_ *? a === a === a *? identityBy_
    identityBy, _1By :: proxy by -> a
    identityBy_, _1_ :: (?by :: proxy by) => a
    identityBy = left_identityBy
    identityBy_ = left_identityBy_
    _1_ = identityBy_
    _1By = identityBy
instance (ISemigroupLeftIdentityBy by a, ISemigroupRightIdentityBy by a)
    => ISemigroupIdentityBy by a where

















-----------------------
instance Semigroup a => ISemigroupBy () a where
    (*?) = (<>)


instance ISemigroupBy by a => ISemigroupBy (ByDual by) a where
    mulBy p a b = mulBy (last1P p) b a
instance Num a => ISemigroupBy BySum a where
    (*?) = (+)
instance Num a => ISemigroupBy ByProduct a where
    (*?) = (*)
instance OpTestBy by Bool => ISemigroupBy (ByAny by) Bool where
    mulBy p a b = f a || f b where f = testBy (last1P p)
instance OpTestBy by Bool => ISemigroupBy (ByAll by) Bool where
    mulBy p a b = f a && f b where f = testBy (last1P p)
instance ISemigroupBy ByFirst (Maybe a) where
    a@(Just _) *? _ = a
    _ *? a = a
instance ISemigroupBy ByEndo (Endo a) where
    mulBy _ = mappend
instance ISemigroupBy ByEndo (a -> a) where
    (*?) = (.)

instance Ord a => ISemigroupBy ByMin a where
    (*?) = min





instance (IPair a, ISemigroupBy by1 (Fst a), ISemigroupBy by2 (Snd a))
    => ISemigroupBy (by1 :*: by2) a where
        {-
    mulBy p12 lhs rhs = r where
        (p1, p2) = to_proxy_pairP p12
        (l1, l2) = to_pair lhs
        (r1, r2) = to_pair rhs
        (o1, o2) = (mulBy p1 l1 r1, mulBy p2 l2 r2)
        r = from_pair (o1, o2)

        -}
    mulBy p12 lhs = r where
        r = mapPair (f1, f2)
        (f1, f2) = mapPair_ (mulBy p1, mulBy p2) lhs
        (p1, p2) = to_proxy_pairP p12










{-
Ordering mul table
-1 0 1
if 0*0 == 0
    a' == 0*1 == (0*0)*1 == 0*(0*1) == 0*a'
    if 1 == 0*1


-}
