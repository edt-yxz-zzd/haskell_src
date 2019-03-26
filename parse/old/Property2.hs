{-# LANGUAGE KindSignatures
 #-}
{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , Rank2Types
            , KindSignatures
            , ScopedTypeVariables #-}
import Boxed
-- import Control.Monad.State.Class
import MonadStateRW

-- F - from mapping (p s v)
-- newtype PropertyF (p :: * -> * -> *) s v = PropertyF { unPropertyF :: v }
newtype PropertyF p s v = PropertyF { unPropertyF :: v }

instance New (PropertyF p s) where
    wrap = PropertyF
    unwrap (PropertyF v) = v

-- V - value
class PropertyRV p s v | p s -> v where
    vget :: s -> p s v
class PropertyWV p s v | p s -> v where
    vset :: p s v -> s -> s
class PropertyWV p s v => PropertyMV p s v where
    vupdate :: (p s v -> Maybe (p s v)) -> s -> s
    vtran :: (p s v -> p s v) -> s -> s
    vtran :: ()
class (PropertyWV p s v, PropertyRV p s v)
    => PropertyV p s v
instance (PropertyWV p s v, PropertyRV p s v)
    => PropertyV p s v

data D a = D { size :: Int, iter :: [a]}

data SizeF
instance PropertyRV (PropertyF SizeF) (D a) Int where
    vget = wrap . size
instance PropertyWV (PropertyF SizeF) (D a) Int where
    vset v s = s { size = unwrap v }



{-
newtype SizeF s v = SizeF v
instance New (SizeF s) where
    wrap = SizeF
    unwrap (SizeF v) = v

instance PropertyV SizeF (D a) Int where
    vget = wrap . size
    vset v s = s { size = unwrap v }
-}
class PropertyRV (PropertyF SizeF) s v => HasSizeRP s v | s->v where
    getSizeP :: s -> v
class PropertyWV (PropertyF SizeF) s v => HasSizeWP s v | s->v where
    setSizeP :: v -> s -> s
class (HasSizeRP s v, HasSizeWP s v) => HasSizeP s v
instance PropertyRV (PropertyF SizeF) s v => HasSizeRP s v where
    getSizeP = unwrap . (vget :: s -> PropertyF SizeF s v)
instance PropertyWV (PropertyF SizeF) s v => HasSizeWP s v where
    setSizeP = (vset :: PropertyF SizeF s v -> s -> s) . wrap
instance (HasSizeRP s v, HasSizeWP s v) => HasSizeP s v


class MonadStateR s m => PropertyRM m p s v | p s -> v, m -> s where
    mvget :: m (p s v)
class MonadStateM s m => PropertyMM m p s v | p s -> v, m -> s where
    mvset :: p s v -> m () {-
class PropertyRM m p s v => PropertyM m p s v | p s -> v, m -> s where
    mset :: p s v -> m ()-}
instance (MonadStateR s m, PropertyRV (PropertyF f) s v)
    => PropertyRM m (PropertyF f) s v where
    mvget = mget >>= return . vget
instance (MonadStateM s m, PropertyMV m p s v)
    => PropertyWM m p s v where
    mvset v = mget >>= put v
class (MonadStateR s m, HasSizeRP s v) => HasSizeRM m s v where
    getSizeM :: m v
    getSizeM = mget >>= return . getSizeP
-- 















