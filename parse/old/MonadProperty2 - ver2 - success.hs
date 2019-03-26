{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , DefaultSignatures
            , Rank2Types
            , KindSignatures
            , GeneralizedNewtypeDeriving
            , ScopedTypeVariables #-}


{-
module MonadStateRW
    ( MonadStateR(..)
    , MonadStateW(..)
    , MonadStateM(..)
    , MonadStateRW(..)
    )
where
-}



{-
    pget/pset or mget/mset?
    if we use MonadState to implement MonadState
        then we need pget/pset
    but how we implement pget/pset?
        modify all those properties can be tedious

 -}



-- 1) define newtype property XxxxP
-- 2)   instance New, PropertyR/W...
-- 3) instance MonadStateR/W...
-- 4)   may need to define a WrapConstaintNewType
-- 5) define convience access function: get/setXxxxP


import Boxed
import qualified Control.Monad.State.Class as M
import qualified Control.Monad.State as MS
import Control.Monad (liftM, ap)
import SeedUtils


-- liftM
-- appM :: Monad m => (a -> b) -> m a -> m b
-- appM = ap . return
class (Monad m, New p) => MonadNew p m where
    -- bug fixed: use liftN instead of wrap . f . unwrap
    lift_mp :: (a -> b) -> m (p a) -> m (p b)
    lift_mp f mpa = mpa >>= return . liftN f
    lift_pm :: (a -> b) -> p (m a) -> p (m b)
    lift_pm = liftN . liftM
    swap_mp :: m (p a) -> p (m a)
    swap_mp mpa = wrap $ mpa >>= return . unwrap
    swap_pm :: p (m a) -> m (p a)
    swap_pm pma = unwrap pma >>= return . wrap
    (>==) :: m (p a) -> (a -> m b) -> m (p b)
    mpa >== f = mpa >>= swap_pm . liftN f
    mwrap :: m a -> m (p a)
    mwrap = liftM wrap
    munwrap :: m (p a) -> m a
    munwrap = liftM unwrap

instance (Monad m, New p) => MonadNew p m




-- Control.Applicative
class New p => PropertyR v p s | p s -> v where
    pget :: p s -> p v
    -- pget = liftN s2v
class New p => PropertyW v p s | p s -> v where
    pset :: p v -> p s -> p s
class PropertyW v p s => PropertyU v p s where
    -- s -> no update? or update with new state s
    pupdate_if :: (p v -> p (Maybe v)) -> p s -> p (Maybe s)
    pupdate :: (p v -> p v) -> p s -> p s
    -- mupdate f = unjust . mupdate_if (Just . f)
class (PropertyR v p s, PropertyU v p s) => PropertyRW v p s where
    prun_if :: (p v -> Either e (a, p v)) -> p s -> Either e (a, p s)
    prun_if f ps = f (pget ps) >>= \(a, pv) -> return (a, pset pv ps)
    prun :: (p v -> (a, p v)) -> p s -> (a, p s)
    prun f ps = case f (pget ps) of
        (a, pv) -> (a, pset pv ps)
instance (PropertyR v p s, PropertyU v p s) => PropertyRW v p s

class (Monad m, PropertyR v p s) => MonadStateR v p s m
    -- m p -> ...
    | m -> s, p s -> v where
    -- mself :: m (p s)
    mget :: m (p v)
    -- mget = liftM pget mself
    meval :: (v -> a) -> m (p a)
    meval f = lift_mp f mget -- mget >>= return . liftN f

class (Monad m, PropertyW v p s) => MonadStateW v p s m
    | m -> s, p s -> v where
    -- error: mset_self :: s -> m (p ())
    -- return  m ()  or m (p())??
    mset :: p v -> m (p()) -- must be (p v) cannot only v???


class (MonadStateW v p s m, PropertyU v p s)
    => MonadStateU v p s m where
    -- s -> no update? or update with new state s
    -- mupdate_if :: (p v -> Maybe (p v)) -> m (p Bool)
    mupdate_if :: (p v -> p (Maybe v)) -> m (p Bool)
    mupdate :: (p v -> p v) -> m (p ())
    mupdate f = lift_mp (const ()) $ mupdate_if (liftN Just . f)
class (MonadStateU v p s m, MonadStateR v p s m, PropertyRW v p s)
    => MonadStateRW v p s m where
    mrun_if :: (p v -> p (Either e (a, v))) -> m (p (Either e a))
    mrun_if f = do
        peeav <- liftM f mget
        case unwrap peeav of
          Left e -> return $ wrap $ Left e
          Right (a, v) -> do
            mset $ liftN (const v) peeav
            return $ wrap $ Right a
{-
    mrun_if :: (p v -> Either e (a, p v)) -> m (Either e (p a))
    mrun_if f = mget >>= \pv -> case f pv of
        Left e -> return $ Left e
        Right (a, pv) -> do
            mset pv
            return . Right $ wrap a
-}
    mrun :: (p v -> p (a, v)) -> m (p a)
    mrun f = do
        pav <- liftM f mget
        mset $ liftN snd pav
        return $ liftN fst pav

instance (MonadStateU v p s m, MonadStateR v p s m, PropertyRW v p s)
    => MonadStateRW v p s m











data D = D { size :: Int, time :: Int }
data SelfF
data SizeF
data TimeF
newtype SelfP a = SelfP a
instance New SelfP where
    wrap = SelfP
    unwrap (SelfP a) = a
instance PropertyR s SelfP s where
    pget = id
newtype SizeP a = SizeP a
instance New SizeP where
    wrap = SizeP
    unwrap (SizeP a) = a
instance PropertyR Int SizeP D where
    pget = liftN size
instance PropertyW Int SizeP D where
    pset = liftN2 $ \s -> \d -> d {size=s}
instance PropertyU Int SizeP D where
    pupdate_if f ps = swap_mp $ swap_pm 
        (f $ pget ps) >>= return . flip pset ps
    pupdate f ps = pset (f $ pget ps) ps

newtype TimeP a = TimeP a
instance New TimeP where
    wrap = TimeP
    unwrap (TimeP a) = a
instance PropertyR Int TimeP D where
    pget = liftN time
instance PropertyW Int TimeP D where
    pset = liftN2 $ \t -> \d -> d {time=t}

newtype WrapMonadState s m a = WrapMonadState (m a) deriving (Monad)
wget :: M.MonadState s m => WrapMonadState s m s
wget = WrapMonadState M.get
wset :: M.MonadState s m => s -> WrapMonadState s m ()
wset = WrapMonadState . M.put
unWrapMonadState (WrapMonadState ma) = ma
instance (M.MonadState s m, PropertyR s SelfP s)
    => MonadStateR s SelfP s (WrapMonadState s m) where
    mget = liftM pget $ liftM wrap wget

mself :: (M.MonadState s m, MonadStateR v p s (WrapMonadState s m))
    => WrapMonadState s m (p s)
mself = liftM wrap wget

instance (M.MonadState s m, PropertyR v p s)
    => MonadStateR v p s (WrapMonadState s m) where
    mget = liftM pget mself
    -- mself = liftM wrap wget
instance (M.MonadState s m, PropertyW v p s)
    => MonadStateW v p s  (WrapMonadState s m) where
    mset pv = do
        ps' <- liftM (pset pv) $ mwrap wget
        wset $ unwrap ps'
        return $ wrap ()

instance (M.MonadState s m, MonadStateW v p s  (WrapMonadState s m),
            PropertyU v p s)
    => MonadStateU v p s (WrapMonadState s m) where

    mupdate_if f = do
        s <- wget
        let may_ps = swap_pm $ pupdate_if f $ wrap s
        case may_ps of
          Nothing -> return . wrap $ False
          Just ps' -> do
            wset $ unwrap ps'
            return . wrap $ True
    {-
    mupdate_if f = do
        s <- wget
        let may_ps = pupdate_if f $ wrap s
        case may_ps of
          Nothing -> return . wrap $ False
          Just ps' -> do
            wset $ unwrap ps'
            return . wrap $ True
    -}
    -- mupdate f = lift_mp (const ()) $ mupdate_if (Just . f)

{-
instance (M.MonadState s m,
            MonadStateR v p s  (WrapMonadState s m),
            MonadStateU v p s  (WrapMonadState s m),
            PropertyRW v p s)
    => MonadStateRW v p s (WrapMonadState s m) where
-}









{-
instance (MonadState s m, PropertyR v SizeP s)
    => MonadStateR v SizeP s (WrapMonadState s m) where
    mself = liftM wrap wget
instance (MonadState D m, PropertyR Int SizeP D)
    => MonadStateR Int SizeP D (WrapMonadState D m) where
    mself = liftM wrap wget
-}

mgetSizeP :: MonadStateR v SizeP s m => m (SizeP v)
mgetSizeP = mget
msetSizeP :: MonadStateW v SizeP s m => SizeP v -> m (SizeP ())
msetSizeP = mset
mupdateSizeP :: MonadStateU v SizeP s m
                => (SizeP v -> SizeP v) -> m (SizeP())
mupdateSizeP = mupdate
mupdateifSizeP :: MonadStateU v SizeP s m
                => (SizeP v -> SizeP (Maybe v)) -> m (SizeP Bool)
mupdateifSizeP = mupdate_if

mrunSizeP :: MonadStateRW v SizeP s m
            => (SizeP v -> SizeP (a, v)) -> m (SizeP a)
mrunSizeP = mrun
mrunifSizeP :: MonadStateRW v SizeP s m
            => (SizeP v -> SizeP (Either e (a, v)))
            -> m (SizeP (Either e a))
mrunifSizeP = mrun_if






--getSizedS :: MonadStateR v SizeP s m => m (SizeP s)
--getSizedS = mself

{-
instance (MonadState s m, PropertyR v TimeP s)
    => MonadStateR v TimeP s (WrapMonadState s m) where
    mself = liftM wrap wget
-}

getTimeP :: MonadStateR v TimeP s m => m (TimeP v)
getTimeP = mget
setTimeP :: MonadStateW v TimeP s m => TimeP v -> m (TimeP ())
setTimeP = mset

d = D {size=1, time=1000}
s = pget (SizeP d)
fd p = unwrap $ MS.evalState (unWrapMonadState p) d
s0 = mgetSizeP
ss = fd s0
tt = fd getTimeP
s1 = msetSizeP (wrap 3)
ss' = fd (s1 >> mgetSizeP)
tt' = fd (setTimeP (wrap 300) >> getTimeP)

s2 = s1 >> mupdateSizeP (liftN (+1))
ss'' = fd (s2 >> mgetSizeP)



{-
class (MonadStateR p m s, MonadStateM p m s)
    => MonadStateRW p m s where
    mexe_if :: (p m s -> Maybe (a, p m s)) -> m (Maybe a)
    mexe_if f = mcall f >>= \mp -> case mp of
        Nothing -> return Nothing
        Just (a, pms) -> mset pms  >> (return $ Just a)

    mexe :: (p m s -> (a, p m s)) -> m a
    mexe f = mcall f >>= \(a, pms) -> mset pms >> return a
instance (MonadStateR p m s, MonadStateM p m s)
    => MonadStateRW p m s


{-
-- Functional dependencies conflict between instance declarations
instance MonadStateRW (StateF SelfF) m s
    => M.MonadState (StateF SelfF m s) m where
    get = mget
    put = mput
-}



-- -}



