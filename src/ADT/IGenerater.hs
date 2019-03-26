{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}


module ADT.IGenerater
where

import Control.Arrow
import ADT.IArrowLift
import Seed.ProxyOps

class IGeneraterBy (Generater2By arr a) arr a
    => IGeneraterDefaultBy arr a where
    type Generater2By arr a
    type Generater2By arr a = ()
type Gen2Value arr a = Gen2ValueBy (Generater2By arr a) arr a
class IGeneraterDefaultBy arr a => IGenerater arr a where
    -- "a" is the generater
    generateA :: arr a (Gen2Value arr a, a)
    generateAPI :: proxy (arr x y) -> arr a (Gen2Value arr a, a)
    lift_generateAPI
        :: IArrowLift arr arr' => proxy (arr x y) -> arr' a (Gen2Value arr a, a)
    lift_generateAP
        :: IArrowLift arr arr'
        => proxy (arr x y, arr z w) -> arr' a (Gen2Value arr a, a)
instance (by ~ Generater2By arr a, IGeneraterDefaultBy arr a)
    => IGenerater arr a where
    generateA = generateByA (Proxy :: Proxy by)
    generateAPI _ = generateByAI (Proxy :: Proxy (arr by x))
    lift_generateAPI _ = lift_generateByAI (Proxy :: Proxy (arr by x))
    lift_generateAP _ = lift_generateByAP (Proxy :: Proxy (arr by x, arr' y z))
class Arrow arr => IGeneraterBy by arr a where
    type Gen2ValueBy by arr a
    generateByA :: proxy by -> arr a (Gen2ValueBy by arr a, a)
    generateByAI :: proxy (arr by x) -> arr a (Gen2ValueBy by arr a, a)
    generateByAI = generateByA . last2P

    lift_generateByAI
        :: IArrowLift arr arr'
        => proxy (arr by x) -> arr' a (Gen2ValueBy by arr a, a)
    lift_generateByAI = liftArrow . generateByAI
    lift_generateByAP
        :: IArrowLift arr arr'
        => proxy (arr by x, arr' y z) -> arr' a (Gen2ValueBy by arr a, a)
    lift_generateByAP = lift_generateByAI . last2P

    {-# MINIMAL generateByA #-}


class (Gen2ValueBy by arr a ~ val, IGeneraterBy by arr a)
    => IGeneraterByEx by arr a val | by arr a -> val
instance (Gen2ValueBy by arr a ~ val, IGeneraterBy by arr a)
    => IGeneraterByEx by arr a val where
class (Gen2Value arr a ~ val, IGenerater arr a)
    => IGeneraterEx arr a val | arr a -> val
instance (Gen2Value arr a ~ val, IGenerater arr a)
    => IGeneraterEx arr a val where






