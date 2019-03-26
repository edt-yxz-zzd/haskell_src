{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances #-}



module Property (Property(..), ptry, pmod) where
-- import Control.Monad.Trans.State.Strict
import Control.Monad.State.Class





class Property v x | x->v  where
    -- f :: s -> s
    -- f s = s''
    -- where x = as_V s -- see XxxProperty, e.g. SizeProperty
    --       v = pget x
    --       v' = gv v
    --       x'= pset v' x
    --       s' = unbox x' -- see Boxed
    --       y = as_U s'
    --       u = pget y
    --       u' = gu u
    --       y' = pset u' y
    --       s'' = unbox y'
    pget :: x -> v
    pset :: v -> x -> x
    ptry_modify :: (v -> Either e (a, v)) -> x -> Either e (a, x)
    ptry_modify f x = case f $ pget x of
        Left e -> Left e
        Right (a, v) -> Right (a, pset v x)
    pmodify :: (v->(a, v)) -> x -> (a, x)
    pmodify f x = let (a, v) = f $ pget x in (a, pset v x)

ptry :: Property v x => (v -> Either e (a, v)) -> x -> Either e (a, x)
ptry = ptry_modify
pmod :: Property v x => (v->(a, v)) -> x -> (a, x)
pmod = pmodify


newtype PropertyM bs m a = PropertyM (m a)
--instance (Monad m) => Monad (PropertyM bs m) deriving (Monad)
instance (Monad m) => Monad (PropertyM bs m) where
    return = box . return
    pma >>= f = box $ unbox pma >>= (unbox . f)
    fail = box . fail
instance Boxed (m a) (PropertyM bs m a) where
    box = PropertyM
    unbox (PropertyM ma) = ma


instance (MonadState s m, Property v bs, Boxed s bs)
    => MonadState v (PropertyM bs m) where
    -- get :: PropertyM bs m v
    get = box $ get >>= return . pget . box
    -- put :: v -> PropertyM bs m ()
    -- put v = box $ get >>= put . rliftB $ pset v
    put v = box $ get >>= \s -> put . unbox . pset v $ box s


class Boxed o n | n->o where 
    -- cannot o->n since we will make many newtypes
    box :: o -> n
    unbox :: n -> o

    liftB :: (o->o) -> n -> n
    liftB f = box . f . unbox
    rliftB :: (n->n) -> o -> o
    rliftB f = unbox . f . box
{-
class New n where
    wrap :: o -> n o
    unwrap :: n o -> o
instance New n => Boxed o (n o) where
    box = wrap
    unbox = unwrap
-}

class Boxed org box => AsProperty x_property org box
    | box->x_property where
    -- x_property :: *->*->*
    as_property :: org -> box -- x_property org box => org -> box
    as_property = box
class (Boxed org box) => SizeProperty org box | org->box where
    as_size :: org -> box
    as_size = box

instance (SizeProperty org box) => AsProperty Size org box where
    as_property = as_size


data D a = D {size::Int, ls::[a]}
    deriving (Show)
instance Boxed (D a) (Size a) where
    box = Size
    unbox (Size d) = d

-- fail: newtype Ls2 a = Ls2 a
-- fail: instance Property [a] (Ls (D a)) 

instance SizeProperty (D a) (Size a)

newtype Size a = Size (D a) deriving (Show)
newtype Ls a = Ls (D a) deriving (Show)
instance Property Int (Size a) where
    pget (Size d) = size d
    pset i (Size d) = Size $ d {size=i}
instance Property [a] (Ls a) where
    pget (Ls d) = ls d
    pset i (Ls d) = Ls $ d {ls=i}


a = D {size = 0, ls = []}
b = pget $ Size a
c = pset [0] $ Ls a

d = pset 3 $ as_size a




--newtype OSize = Int
--newtype OLs a = [a]
--instance Property OSize (D a) where






class MProperty v m | m -> v where
    -- type x :: *->*
    mget :: m v
    mset :: v -> m ()
    mmodify :: (v->(a, v)) -> m a
    mtry_modify :: (v->Either e (a, v)) -> m (Either e a)
    --__f :: m () -> x v
    --__f = undefined


{-
instance (Property v y, Boxed x y, Monad m, Boxed (StateT x m a) (b x m a)) => MProperty v (b x m)  where
    mget = do
        x <- get
        return . pget $ box x
    mset v = do
        x <- get
        put $ rliftB (pset v) x
    mmodify f = do
        x <- get
        let (a, v) = f $ pget $ box x
        put $ rliftB (pset v) x
        return a
    mtry_modify f = do
        x <- get
        case f $ pget $ box x of
            Left e -> return $ Left e
            Right (a, v) -> do
                put $ rliftB (pset v) x
                return $ Right a

-}








