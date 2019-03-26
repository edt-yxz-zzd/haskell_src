{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , Rank2Types
            , KindSignatures
            , ScopedTypeVariables #-}
            -- 

-- NOTE: class AsProperty p v a without a -> p
-- since a will have many properties
-- but when we want to use it, we need a -> p1 p2 p3...
-- e.g. class (AsP p1 v1 a, AsP p2 v2 a)
--          => A p1 v1 p2 v2 a | a -> p1 v1 p2 v2 where
--          get_p1 a = pget (wrap a :: p1 a)
--          set_p1 v1 a = unwrap $ pset v1 (wrap a :: p1 a)
-- or   class AsP p1 v1 a => AsP1 p1 v1 a | a -> p1 v1 where
--          get_p1 a = pget (wrap a :: p1 a)
--          set_p1 v1 a = unwrap $ pset v1 (wrap a :: p1 a)
--      class (AsP1 p1 v1 a, AsP2 p2 v2 a) => A p1 v1 p2 v2


module Boxed
    ( New(..)
    , Boxed(..)
    , Property(..)
    , AsProperty(..)
    , ReadProperty(..)
    , WriteProperty(..)
    , TransformProperty(..)
    , OptionalProperty(..)
    )
where

import Control.Monad.State.Class as M
import Data.Monoid


class Boxed o bo | bo -> o where
    -- cannot o->n since we will make many newtypes
    box :: o -> bo
    unbox :: bo -> o

    liftB :: (o->o) -> bo -> bo
    liftB f = box . f . unbox
    rliftB :: (bo -> bo) -> o -> o
    rliftB f = unbox . f . box


-- class (forall o. Boxed o (b o)) => New b
-- instance (forall o. Boxed o (b o)) => New b
{-
class New b where
    wrap :: o -> b o
    unwrap :: b o -> o
instance New b => Boxed o (b o) where
    box = wrap
    unbox = unwrap
-- but now we cannot define: instance Boxed (m a) (P b s m a)
-- Functional dependencies conflict between instance declarations
-- -}
class New b where
    wrap :: o -> b o
    unwrap :: b o -> o

    liftN :: (o -> o) -> b o -> b o
    liftN f = wrap . f . unwrap
    rliftN :: (b o -> b o) -> o -> o
    rliftN f = unwrap . f . wrap
newtype New2Boxed a = New2Boxed a
instance New New2Boxed where
    wrap = New2Boxed
    unwrap (New2Boxed a) = a

instance New b => Boxed o (New2Boxed (b o)) where
    box = wrap . wrap
    unbox = unwrap . unwrap




class ReadProperty v x | x->v  where
    pget :: x -> v
class WriteProperty v x | x->v  where
    pset :: v -> x -> x
class TransformProperty v x | x->v  where
    ptran :: (v->v) -> x -> x
    ptran f x = maybe x id (pupdate (Just . f) x)
    pupdate :: (v->Maybe v) -> x -> Maybe x

maybe2either :: e -> Maybe a -> Either e a
maybe2either e = maybe (Left e) Right
maybef2eitherf :: e -> (s -> Maybe a) -> (s -> Either e a)
maybef2eitherf e f x = maybe2either e $ f x
instance Monad ((,) a) where
    return b = (undefined, b)
    (a, b) >>= f = (a, snd $ f b)

class TransformProperty v x => OptionalProperty v x where
    -- if no state at all or input function return Left,
    -- then return Left
    -- return Right ==>> update
    pget_maybe :: x -> Maybe v
    pset_maybe :: v -> x -> Maybe x
    pget_either :: e -> x -> Either e v
    pget_either e = maybef2eitherf e pget_maybe
    pset_either :: e -> v -> x -> Either e x
    pset_either e v = maybef2eitherf e $ pset_maybe v

    ptry :: e -> (v -> Either e (a, v)) -> x -> Either e (a, x)
    ptry e f x = pget_either e x >>= f >>= \(a, v) -> 
        pset_either e v x >>= \x -> return (a, x)
        --(maybe (Left e) f $ pget_maybe x) >>= \av -> 
        -- set may fail:
        -- maybe (Left e) (\x->(fst av, x)) $ pset_maybe (snd av) x
    pcall :: e -> (v -> Either e v) -> x -> Either e x
    pcall e f x = (ptry e $ \v -> f v >>= \v -> return ((), v)) x
                    >>= return . snd

    -- pupdate :: (v->Maybe v) -> x -> Maybe x
    -- pupdate f x = pget_maybe x >>= f >>= $ \v -> pset_maybe v x

    -- ptran :: (v->v) -> x -> x
    -- ptran f x = either (const x) id $ pcall undefined (Right . f) x


instance (ReadProperty v x, WriteProperty v x)
    => TransformProperty v x where
    ptran f x = pset (f $ pget x) x
    pupdate f x = (f $ pget x) >>= \v -> return $ pset v x

instance (ReadProperty v x, WriteProperty v x)
    => OptionalProperty v x where
    pget_maybe = return . pget
    pset_maybe v = return . pset v
    pget_either _ = return . pget
    pset_either _ v = return . pset v

    ptry _ f x = f (pget x) >>= \av -> return $ av >>= \v -> return $ pset v x
        -- flip . pset $ x -- \(a, v) -> Right (a, pset v x)
    pcall _ f x = f (pget x) >>= \v -> return $ pset v x


class (ReadProperty v x, WriteProperty v x)
    => Property v x | x->v  where
    -- f :: s -> s
    -- f s = s''
    -- where x = as_V s -- see XxxProperty, e.g. SizeProperty
    --       v = pget x
    --       v' = gv v
    --       x'= pset v' x
    --       s' = unbox x' -- see New
    --       y = as_U s'
    --       u = pget y
    --       u' = gu u
    --       y' = pset u' y
    --       s'' = unbox y'
    pmodify :: (v->(a, v)) -> x -> (a, x)
    pmodify f x = (f $ pget x) >>= \v -> return $ pset v x
instance (ReadProperty v x, WriteProperty v x)
    => Property v x



{-
ptry :: TransformProperty v x => (v -> Either e (a, v)) -> x -> Either e (a, x)
ptry = ptry_modify
pmod :: Property v x => (v->(a, v)) -> x -> (a, x)
pmod = pmodify
-}


-- will, we use (wrap a :: p a) directly
class (New p, ReadProperty v (p a)) => AsReadProperty p v a
class (New p, WriteProperty v (p a)) => AsWriteProperty p v a
class (New p, TransformProperty v (p a)) => AsTransformProperty p v a
class (New p, Property v (p a)) => AsProperty p v a where
    -- no a->p since a will have many properties
    -- as_property :: a -> p a -- x_property org box => org -> box
    -- as_property = wrap
    -- type AsP p v a = a -> p a
-- why below fail:??
--      see 7.11.2. The context of a type signature
-- type AsP p v a = AsProperty p v a => a -> p a
-- type AsProperty p v a => AsP p v a = a -> p a
-- type AsP p v a = forall p v a. AsProperty p v a => a -> p a

-- instance SizeP Size a => AsProperty Size a
-- instance AsProperty Size (D a)
instance (New p, ReadProperty v (p a)) => AsReadProperty p v a
instance (New p, WriteProperty v (p a)) => AsWriteProperty p v a
instance (New p, TransformProperty v (p a)) => AsTransformProperty p v a
instance (New p, Property v (p a)) => AsProperty p v a


newtype Size a = Size a
instance New Size where
    wrap = Size
    unwrap (Size a) = a
{-
class New p => SizeP p a | a->p where
    as_size :: a -> p a
    as_size = box
instance SizeP Size (D a)
-}

newtype Iter a = Iter a
instance New Iter where
    wrap = Iter
    unwrap (Iter a) = a
{-
class New p => IterP p a | a->p where
    as_iter :: a -> p a
    as_iter = box
instance IterP Iter (D a)
-- instance IterP Iter a => AsProperty Iter a
-}



{-
data D a = D { size :: Int, iter :: [a]}
instance Property Int (Size (D a)) where
    pget (Size d) = size d
    pset i (Size d) = Size $ d {size=i}
instance Property [a] (Iter (D a)) where
    pget (Iter d) = iter d
    pset ls (Iter d) = Iter $ d {iter=ls}
a = D {size=0, iter=[]}
s = pget $ (as_property :: AsProperty Size v a => a -> Size a) a
s' = pget $ (as_property a :: Size (D a))
s'' = pget (wrap a :: Size (D a))
-- fail?? s''' = pget (box a :: Size a)
l = pget (wrap a :: Iter (D a))
-}










newtype PropertyM (b :: * -> *) s m a = PropertyM (m a)
-- fail: type T b s (m a) = PropertyM b s m a
-- instance New (T b s)

instance Boxed (m a) (PropertyM b s m a) where
    box = PropertyM
    unbox (PropertyM ma) = ma

--type T (m a) = m a
--instance (Monad m) => Monad (PropertyM bs m) deriving (Monad)
instance (Monad m) => Monad (PropertyM b s m) where
    return = box . return
    pma >>= f = box $ unbox pma >>= (unbox . f)
    fail = box . fail

{-
class F a b c | c-> a b where
    h :: c (a b)
class Default a b where
    d :: a b

instance (New c, Default a b) => F a b c where
    h = (wrap :: (New c, Default a b) => a b -> c (a b)) d :: (New c, Default a b) => c (a b)
instance MonadState () Maybe where
    -- get :: Maybe ()
    get = Just () :: Maybe ()
    put _ = Just ()

g :: (New b, Property v (b s)) => s -> b s
g s = wrap s
-}


{- ERROR:
ff = \x -> v where v = x
-- = (\x -> v) where v = x
ff = \x -> v
    where 
        v = x
-}

class Monad m => MonadStateR s m | m->s where
    mget :: m s

class Monad m => MonadStateW s m | m->s where
    mput :: s -> m ()
class (MonadStateR s m, MonadStateW s m) => MonadStateRW s m


{-
class (Monoid s, Monad m, Monad g) => MonadLogState s g m | m -> s g where
    -- global log state
    -- (m a, g s)
    log :: s -> m ()
    log s = logger (return ()) >> return s
    logger :: m a -> g s
class (Monoid s, Monad m) => MonadLocalState s m | m->s where
    -- s go with a
    -- m (a, s)
    hold :: s -> m ()
    look :: m a -> m (a, s)
-}


instance (MonadStateR s m, MonadStateW s m) => MonadStateRW s m

instance MonadStateRW s m => M.MonadState s m where
    get = mget
    put = mput



-- Reader and Writer are so difference
instance (MonadStateR s m, AsProperty b v s) --, New b, Property v (b s)
         => MonadStateR v (PropertyM b s m) where
    mget = box $ mget >>= \x -> return $ pget (wrap x :: b s)
instance (MonadStateRW s m, AsProperty b v s) --, New b, Property v (b s)
         => MonadStateW v (PropertyM b s m) where
    mput v = box $ mget >>= mput . rliftN (pset v :: b s -> b s)
    {-
    put v = box $ (get :: m s) >>= f
        where f (s::s) = m_
                where s' = unwrap $ (pset v) (as_property s :: b s)
                      m_ = put s' :: m ()
    -}






