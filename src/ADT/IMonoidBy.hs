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
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}


module ADT.IMonoidBy
    ( module ADT.IMonoidBy
    , module ADT.ISemigroupBy
    )
where
import ADT.IPair
import Data.Semigroup hiding (Sum, Product)
import Data.Proxy
import GHC.Types (type (~~))
import Data.Kind (type (*))
import Seed.ProxyOps (last1P, withBy, withBy_, last2P)
import Seed.By
import Seed.OpTest
import ADT.ISemigroupBy

--type GetKind (a :: ka) = ka
--type role Semigroup phantom representational
class ISemigroupIdentityBy by a => IMonoidBy by a where
    memptyBy :: proxy by -> a
    memptyBy_ :: (?by :: proxy by) => a
    memptyBy_ = left_identityBy_
    memptyBy = left_identityBy
    {-
    memptyBy_ = withBy_ memptyBy
    memptyBy = withBy memptyBy_
    --{-# MINIMAL memptyBy | memptyBy_ #-}
    -}
instance ISemigroupIdentityBy by a => IMonoidBy by a where


{-
instance (Semigroup a, ISemigroupBy () a, Monoid a)
    => IMonoidBy () a where
    memptyBy_ = mempty


instance IMonoidBy by a => IMonoidBy (ByDual by) a where
    memptyBy p = memptyBy (last1P p)
instance Num a => IMonoidBy BySum a where
    memptyBy_ = 0
instance Num a => IMonoidBy ByProduct a where
    memptyBy_ = 1
instance OpTestBy by Bool => IMonoidBy (ByAny by) Bool where
    memptyBy_ = False
instance OpTestBy by Bool => IMonoidBy (ByAll by) Bool where
    memptyBy_ = True
instance IMonoidBy ByFirst (Maybe a) where
    memptyBy_ = Nothing
instance IMonoidBy ByEndo (Endo a) where
    memptyBy_ = mempty
instance IMonoidBy ByEndo (a -> a) where
    memptyBy_ = id



instance (IPair a, IMonoidBy by1 (Fst a), IMonoidBy by2 (Snd a))
    => IMonoidBy (by1 :*: by2) a where
    memptyBy p = mkPair (memptyBy $ last2P p) (memptyBy $ last1P p)
-}


------------------

instance (Semigroup a, ISemigroupBy () a, Monoid a)
    => ISemigroupLeftIdentityBy () a where
    left_identityBy_ = mempty


instance IMonoidBy by a => ISemigroupLeftIdentityBy (ByDual by) a where
    left_identityBy p = memptyBy (last1P p)
instance Num a => ISemigroupLeftIdentityBy BySum a where
    left_identityBy_ = 0
instance Num a => ISemigroupLeftIdentityBy ByProduct a where
    left_identityBy_ = 1
instance OpTestBy by Bool => ISemigroupLeftIdentityBy (ByAny by) Bool where
    left_identityBy_ = False
instance OpTestBy by Bool => ISemigroupLeftIdentityBy (ByAll by) Bool where
    left_identityBy_ = True
instance ISemigroupLeftIdentityBy ByFirst (Maybe a) where
    left_identityBy_ = Nothing
instance ISemigroupLeftIdentityBy ByEndo (Endo a) where
    left_identityBy_ = mempty
instance ISemigroupLeftIdentityBy ByEndo (a -> a) where
    left_identityBy_ = id



instance (IPair a, IMonoidBy by1 (Fst a), IMonoidBy by2 (Snd a))
    => ISemigroupLeftIdentityBy (by1 :*: by2) a where
    left_identityBy p = mkPair (memptyBy $ last2P p) (memptyBy $ last1P p)




------

instance (Semigroup a, ISemigroupBy () a, Monoid a)
    => ISemigroupRightIdentityBy () a where


instance IMonoidBy by a => ISemigroupRightIdentityBy (ByDual by) a where
instance Num a => ISemigroupRightIdentityBy BySum a where
instance Num a => ISemigroupRightIdentityBy ByProduct a where
instance OpTestBy by Bool => ISemigroupRightIdentityBy (ByAny by) Bool where
instance OpTestBy by Bool => ISemigroupRightIdentityBy (ByAll by) Bool where
instance ISemigroupRightIdentityBy ByFirst (Maybe a) where
instance ISemigroupRightIdentityBy ByEndo (Endo a) where
instance ISemigroupRightIdentityBy ByEndo (a -> a) where



instance (IPair a, IMonoidBy by1 (Fst a), IMonoidBy by2 (Snd a))
    => ISemigroupRightIdentityBy (by1 :*: by2) a where




--}
--}
--}
--}
