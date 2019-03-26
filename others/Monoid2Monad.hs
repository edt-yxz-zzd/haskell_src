{-# LANGUAGE TypeFamilies #-}
module Monoid2Monad 
    ( ToMonad(..)
    , TMState(..)
    , TMValue(..)
    ) where

-- import Data.Functor (Functor(..))
import Data.Monoid (Monoid(..))

-- 'st' is state, x is value
newtype ToMonad st x = ToMonad {unToMonad :: Either String (st, x)}
newtype TMState x st = TMState {unTMState :: ToMonad st x}
instance Functor (TMState x) where
    fmap _ (TMState (ToMonad (Left err))) = TMState . ToMonad . Left $ err
    fmap f (TMState (ToMonad (Right (st, x)))) = TMState . ToMonad . Right $ (f st, x)

newtype TMValue st x = TMValue {unTMValue :: ToMonad st x}
instance Functor (TMValue st) where
    fmap _ (TMValue (ToMonad (Left err))) = TMValue . ToMonad . Left $ err
    fmap f (TMValue (ToMonad (Right (st, x)))) = TMValue . ToMonad . Right $ (st, f x)
instance Monoid st => Monad (ToMonad st) where
    return x = ToMonad $ Right (mempty, x)
    ToMonad (Left err) >>= _ = ToMonad $ Left err
    ToMonad (Right (st',x)) >>= x2my = case x2my x of 
        err@(ToMonad (Left _)) -> err
        -- reverse append
        ToMonad (Right (st'', y)) -> ToMonad $ Right (mappend st'' st', y)
    fail = ToMonad . Left
class Outputable io where
    -- data StateT :: * -> *
    type StateT io :: *
    -- type StateT :: *
    output :: StateT io -> io -> io
instance Monoid st => Outputable (ToMonad st x) where
    type StateT (ToMonad st x) = st 
    -- right-hand 'st' should appear in left-hand,
    -- so, using "StateT io" instead of "StateT"
    -- type StateT = st # err

    -- type M = ToMonad st x # why fail??
    -- output :: st -> M -> M
    -- output :: st -> (ToMonad st x) -> (ToMonad st x)
    output _ err@(ToMonad (Left _)) = err
    output st (ToMonad (Right (st', x))) = ToMonad . Right $ (mappend st st', x)


