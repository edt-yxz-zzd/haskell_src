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


module Property where

import Boxed
import qualified Control.Monad.State.Class as M
import qualified Control.Monad.State as MS
import Control.Monad (liftM, ap)
import SeedUtils
import MonadNew

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



data D = D { size :: Int, time :: Int } deriving(Show, Eq, Ord)
data SelfF
data SizeF
data TimeF

-- WrapNew -- newtype PropertyF2P f a = PropertyF2P { unPropertyF2P :: a }
type Type2New = NamedNew
type SelfP = Type2New SelfF
type SizeP = Type2New SizeF
type TimeP = Type2New TimeF

instance PropertyR s SelfP s where
    pget = id

instance PropertyR Int SizeP D where
    pget = liftN size
instance PropertyW Int SizeP D where
    pset = liftN2 $ \s -> \d -> d {size=s}
instance PropertyU Int SizeP D where
    pupdate_if f ps = swap_mp $ swap_pm 
        (f $ pget ps) >>= return . flip pset ps
    pupdate f ps = pset (f $ pget ps) ps

instance PropertyR Int TimeP D where
    pget = liftN time
instance PropertyW Int TimeP D where
    pset = liftN2 $ \t -> \d -> d {time=t}


pgetSizeP :: PropertyR v SizeP s => SizeP s -> SizeP v
pgetSizeP = pget


{-
newtype SelfP a = SelfP a
instance New SelfP where
    wrap = SelfP
    unwrap (SelfP a) = a

newtype SizeP a = SizeP a
instance New SizeP where
    wrap = SizeP
    unwrap (SizeP a) = a
newtype TimeP a = TimeP a
instance New TimeP where
    wrap = TimeP
    unwrap (TimeP a) = a


-}
