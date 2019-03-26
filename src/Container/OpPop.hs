{-# LANGUAGE TypeFamilies #-}
module Container.OpPop
where
import Container.IContainer
import Container.OpIter
import Data.Maybe
import Data.List

class   ( OpIter a
        -- , OpEmpty a, maybe infinite...
        )
    => OpPop a where
    popls :: a -> [(Element a, a)]
    pop :: a -> Maybe (Element a, a)

    pop = listToMaybe . popls
    popls = unfoldr (fmap f . pop) where
        f ea = (ea, snd ea)
    {-# MINIMAL pop | popls #-}



