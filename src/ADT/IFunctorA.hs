{-# LANGUAGE Arrows #-}

module ADT.IFunctorA
where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)


class IFunctorA f where
    fmapA :: ArrowChoice arr => arr i o -> arr (f i) (f o)
class IBifunctorA f where
    bimapA :: ArrowChoice arr => arr i o -> arr i' o' -> arr (f i i') (f o o')
    bimapA f g = snd_mapA g . fst_mapA f
    fst_mapA :: ArrowChoice arr => arr i o -> arr (f i i') (f o i')
    fst_mapA = flip bimapA id
    snd_mapA :: ArrowChoice arr => arr i' o' -> arr (f i i') (f i o')
    snd_mapA = bimapA id
    {-# MINIMAL (bimapA | (fst_mapA, snd_mapA)) #-}

instance IFunctorA Maybe where
    fmapA i2o = proc mi -> case mi of
        Nothing -> returnA -< Nothing
        Just i -> i2o >>^ Just -< i
instance IFunctorA (Either e) where
    fmapA = right
    {-
    fmapA i2o = proc ei -> case ei of
        Left e -> returnA -< Left e
        Right i -> i2o >>^ Right -< i
    -}
instance IFunctorA ((,) e) where
    fmapA = second

instance IBifunctorA Either where
    bimapA = (+++)
    {-
    bimapA a2c b2d = proc aEb -> case aEb of
        Left a -> a2c >>^ Left -< a
        Right b -> b2d >>^ Right -< b
    -}
instance IBifunctorA (,) where
    bimapA = (***)

