{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}

module Seed.OpTest
    ( module Seed.OpTest
    , module Data.Bool
    )
where
import Data.Bool
import Seed.ProxyOps
import Data.Maybe
import Data.Either
import Seed.By
import Numeric.Natural

{-
bool :: a -> a -> Bool -> a
bool a a' b = if b then a' else a
-}

class OpTest a where
    test :: a -> Bool
class OpTestBy by a where
    testBy :: proxy by -> a -> Bool
    testBy = withBy testBy_
    testBy_ :: (?by :: proxy by) => a -> Bool
    testBy_ = withBy_ testBy
    {-# MINIMAL testBy | testBy_ #-}
instance OpTest a => OpTestBy () a where
    testBy _ = test
instance OpTestBy by a => OpTestBy (ByNot by) a where
    testBy = testBy . last1P



instance OpTest [a] where
    test = not . null
instance OpTest (Maybe a) where
    test = isJust
instance OpTest (Either a b) where
    test = isRight

instance OpTest Ordering where
    test = (EQ ==)
instance OpTest Bool where
    test = id
instance OpTest Integer where
    test = (0 ==)
instance OpTest Int where
    test = (0 ==)
instance OpTest Natural where
    test = (0 ==)



last2_1P = last2P . last1P
last1_1P = last1P . last1P
instance (OpTestBy by1 a, OpTestBy by2 b)
    => OpTestBy (ByAll (by1, by2)) (a, b) where
    testBy p (a, b) = testBy (last2_1P p) a && testBy (last1_1P p) b
instance (OpTestBy by1 a, OpTestBy by2 b)
    => OpTestBy (ByAny (by1, by2)) (a, b) where
    testBy p (a, b) = testBy (last2_1P p) a || testBy (last1_1P p) b



