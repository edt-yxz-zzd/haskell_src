{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


module ADT.IArrowLift
where

import Seed.ArrowOps (withInput, ab_c2ac_bA)
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Seed.Boxed

class (Arrow from, Arrow to) => IArrowLift from to where
    liftArrow :: from i o -> to i o
    liftArrowPO :: proxy to -> from i o -> to i o
    liftArrowPI :: proxy from -> from i o -> to i o
    liftArrowP :: proxy from -> proxy to -> from i o -> to i o
    liftArrowPO _ = liftArrow
    liftArrowPI _ = liftArrow
    liftArrowP _ _ = liftArrow
instance Arrow arr => IArrowLift arr arr where
    liftArrow = id

