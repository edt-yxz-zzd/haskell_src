{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}



module Arrows.ArrSS2
    ( ArrSS2()
    , pureArrSS
    , setArrSS
    , getArrSS
    )
where

import ADT.IArrowSuccess

import Control.Arrow
import Seed.ArrowOps (mk_app, fmapA2M, pureA2M, (<**>), (>>==))
import Control.Category
import Prelude hiding ((.), id)
import Seed.CategoryData -- (CategoryPair(..))
import Data.Semigroup



import Seed.Types
import Seed.Boxed
newtype ArrSS2 arr i o = ArrSS2 {runArrSS2 :: arr i o -> arr i o}
    -- f alter = head -|< (alter ||| tail)
instance Boxed (ArrSS2 arr i o) where
    box = ArrSS2
    unbox = runArrSS2

mkArrSS2
    :: (ArrowChoice arr, OpDoOrNopA arr)
    => arr i x -> arr x o -> ArrSS2 arr i o
mkArrSS2 head tail = box $ \alter -> do_or_nopEA head >>> (alter ||| tail)
unArrSS2 :: ArrowZero arr => ArrSS2 arr i o -> arr i o
unArrSS2 a = unbox a zeroArrow

left_biased_plus :: Op (ArrSS2 arr i o)
left_biased_plus = opmapbBox (.)

instance Category arr => Category (ArrSS2 arr) where
    id = box $ const id
    (.) = ?????????

