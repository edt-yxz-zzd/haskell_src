{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

module Visit
    ( Visit (..)
    , VisitByTable (..)
    , WildCase (..)
    , RT (..)
    , RM (..)
    , typeAssertMonadReader
    , typeAssertVisit
    )
where
import SeedUtils__TH
import SeedUtils (lift2, for)
import PrintQ
import Language.Haskell.TH as TH
import Language.Haskell.Syntax
import Language.Haskell.TH.Syntax -- (StrictType, VarStrictType)
import Control.Monad
-- import SeedUtils__TH
{-# LANGUAGE TemplateHaskell #-}
import Data.Maybe
import qualified Data.Map as M 
import Data.Map (Map)
import Control.Monad.Reader
import Data.List as L -- (nub)


class (Visit m a, MonadReader t m) => VisitByTable t m a where
instance (Visit m a, MonadReader t m) => VisitByTable t m a where
class Monad m => Visit m a where
    visit :: a -> m a
class Monad m => WildCase m a where
    wild_case :: a -> m a
instance Visit m a => WildCase m [a] where
    wild_case = mapM visit
instance Visit m a => WildCase m (Maybe a) where
    wild_case Nothing = return Nothing
    wild_case (Just a) = visit a >>= return . Just
instance (Visit m a, Visit m b) => WildCase m (a, b) where
    wild_case (a, b) = do
        a' <- visit a
        b' <- visit b
        return (a', b')
instance (Visit m a, Visit m b, Visit m c) => WildCase m (a, b, c) where
    wild_case (a, b, c) = do
        a' <- visit a
        b' <- visit b
        c' <- visit c
        return (a', b', c')


instance Monad m => WildCase m Name where
    wild_case = return



-- rT : ReaderT
-- t_ : Table
-- t : Table m' - m' = RT t_ rT m
-- m : in : ReaderT t m
newtype RT t_ rT (m :: * -> *) a = RT {unRT :: rT (t_ (RT t_ rT m)) m a}
deriving instance Monad (rT (t_ (RT t_ rT m)) m) => Monad (RT t_ rT m)

instance MonadReader (t_ (RT t_ rT m)) (rT (t_ (RT t_ rT m)) m)
    => MonadReader (t_ (RT t_ rT m)) (RT t_ rT m) where
    ask = RT ask
    local f (RT ma) = RT $ local f ma
    reader = RT . reader


-- t2m : *->*->* : e.g. (t->)
-- t_ : Table
-- t : Table m' - m' = RM t_ t2m
newtype RM t_ t2m a = RM {unRM :: t2m (t_ (RM t_ t2m)) a}
deriving instance Monad (t2m (t_ (RM t_ t2m))) => Monad (RM t_ t2m)
instance MonadReader (t_ (RM t_ t2m))(t2m (t_ (RM t_ t2m)))
    => MonadReader (t_ (RM t_ t2m))(RM t_ t2m) where
    ask = RM ask
    local f (RM ma) = RM $ local f ma
    reader = RM . reader

typeAssertMonadReader :: MonadReader r m => m a -> m a
typeAssertMonadReader = id
typeAssertVisit :: Visit m a => m a -> m a
typeAssertVisit = id




