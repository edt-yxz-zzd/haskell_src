{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , DefaultSignatures
            , TypeFamilies
            , Rank2Types
            , TypeOperators
            , KindSignatures
            , GeneralizedNewtypeDeriving
            , ScopedTypeVariables #-}

import Boxed
import SeedUtils (sametype)

newtype MPropertyBox m attrs v a = MPropertyBox { unMPropertyBox :: (m a) }
    -- deriving (Monad)
instance Monad m => Monad (MPropertyBox m attrs v) where
    return = box . return
    bma >> bmb = box $ unbox bma >> unbox bmb
    bma >>= bf = box $ unbox bma >>= unbox . bf -- cannot access this function??
instance Boxed (m a) (MPropertyBox m name v a) where
    box = MPropertyBox
    unbox = unMPropertyBox

class VStatePR p v


mcase :: MPropertyBox m attrs v a -> MPropertyBox m attrs' v' a
mcase = box . unbox

class MStateR m v | m -> v where
    mgetb :: m v
class MStateR (n v) v => MStatePR n v | n -> v where
    mget :: (n v) v
instance MStatePR (MPropertyBox m attrs) v
    => MStateR (MPropertyBox m attrs v) v where
    mgetb = mget
instance MStateR m s => MStatePR (MPropertyBox m SelfF) s where
    mget = box (mgetb :: m s)
instance (MStatePR (MPropertyBox m attr1) v, 
          VStatePR (VPropertyBox v attr2) u)
    => MStatePR (MPropertyBox m (Child attr1 attr2)) u where
    mget = mcase $ mget >>= f

-- lift = liftM
lift :: (Monad m) => (a->b) -> MPropertyBox m attrs v a -> MPropertyBox m attrs v b
lift f bma = bma >>= return . f

mget :: Monad m => m v
mget = return undefined

-- _1 = sametype(mget, mget')
-- mget' :: forall m v. Monad m => m v
-- mget' :: Monad m => m v -- the same as above but differ below
-- mget' = undefined
mget' :: forall m v. Monad m => m v
mget' = m' where
    -- m' :: (m ~ m1) => MPropertyBox m1 () () v
    m' = mget :: m v
--mget' = unbox $ lift id (mget :: Monad m => MPropertyBox m () () v)
    -- why need "Monad m" in last line??
--}-}


may1 = mget' :: Maybe v
ls1 = mget' :: [Int]


b = case may1 of
    Nothing -> False
    Just _ -> True




e0 :: Monad m => m a
e0 = undefined
e1 :: Monad m => m a
e1 = e0 -- correct
-- e1 = e0 :: m a -- ERROR: this m is a new m that not in sig of e1
-- e2 :: Monad m => m Int
-- e2 = e0 :: m Int -- ERROR: see above

e3 :: forall m a. Monad m => m a
e3 = e0 :: m a -- SUCCESS: this m is the m in sig of e3













