{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
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

import Control.Monad.Reader

class Visit t a where
    visit :: MonadReader (t m) m => a -> m a

class   ( MonadReader (t (R1 t)) (R1 t)
        , MonadReader (t (R3x t rT m)) (R3x t rT m)
        )
    => Readers (t :: (* -> *) -> *) rT m where
    data R1 t :: * -> *
    data R3x t (rT :: * -> (* -> *) -> * -> *) (m :: * -> *) :: * -> *
instance    ( r ~ t m_out, m_out~R3x t rT m, m_in~rT r m
            , MonadReader r m_in)
    => Readers t rT m where
    newtype R1 t a = R1 { unR1 :: (t (R1 t) -> a) }
    newtype R3x t rT m a = R3x { unR3x :: rT (t (R3x t rT m)) m a }

-- fail? deriving instance Monad (R1 Table)
instance Monad (R1 t) where
    R1 f >>= g = R1 (f >>= unR1 . g)
    return = R1 . return
    fail = R1 . fail
instance MonadReader (t (R1 t)) (R1 t) where
    ask = R1 ask
    local f (R1 m) = R1 $ local f m
    reader = R1 . reader
instance    ( r ~ t m_out, m_out~R3x t rT m, m_in~rT r m
            , Monad m_in)
    => Monad (R3x t rT m) where
    R3x f >>= g = R3x (f >>= unR3x . g)
    return = R3x . return
    fail = R3x . fail
instance    ( r ~ t m_out, m_out~R3x t rT m, m_in~rT r m
            , MonadReader r m_in)
    => MonadReader r (R3x t rT m) where
    ask = R3x ask
    local f (R3x m) = R3x $ local f m
    reader = R3x . reader
type M a m = a -> m a
type MM_ t a m = MonadReader (t m) m => a -> m a
type MT_ t subt m = MonadReader (t m) m => subt m
type CM_ t m a = MonadReader (t m) m => a
























-- ! fail ! a -> ((Table m) -> a) where m = ((Table m) ->)
newtype R a = R ((Table R) -> a)
    deriving (Monad)--, MonadReader)
newtype R2 f a = R2 (f (Table (R2 f)) a)
deriving instance Monad (f (Table (R2 f))) => Monad (R2 f)
newtype R3 f (m:: * -> *) a = R3 (f (Table (R3 f m)) m a)
deriving instance Monad (f (Table (R3 f m)) m) => Monad (R3 f m)

instance MonadReader (Table R) R where
    ask = R ask -- R id
    local f (R g) = R (local f g) -- R (g . f)
    -- reader = R
    reader = R . reader

instance MonadReader (Table (R2 f)) (f (Table (R2 f)))
    => MonadReader (Table (R2 f)) (R2 f) where
    ask = R2 ask
    local f (R2 g) = R2 (local f g)
    reader = R2 . reader

instance MonadReader (Table (R3 f m)) (f (Table (R3 f m)) m)
    => MonadReader (Table (R3 f m)) (R3 f m) where
    ask = R3 ask
    local f (R3 g) = R3 (local f g)
    reader = R3 . reader

typeAssertMonadReader :: MonadReader r m => m a -> m a
typeAssertMonadReader = id
__test__ :: Monad m => R3 ReaderT m a
__test__ = typeAssertMonadReader (undefined :: Monad m => R3 ReaderT m a)

__test2 :: Monad m => R3x Table ReaderT m a
__test2 = typeAssertMonadReader undefined


type MM a m = MM_ Table a m
type MT subt m = MT_ Table subt m
type CM m a = CM_ Table m a
{-
type MM a m = MonadReader (Table m) m => a -> m a
type MT t m = MonadReader (Table m) m => t m
type CM m a = MonadReader (Table m) m => a
-}
data Table m = Table { caseA :: TableA m, caseB :: TableB m }
data TableA m = TableA { case_A :: M A m, case_AB :: M A m }
data TableB m = TableB { case_B1 :: M B m, case_B2 :: M B m }
data A = A | AB B
    deriving (Show)
data B = B1 | B2
    deriving (Show)

instance Visit Table B where
    visit b = do
        tableB <- reader $ caseB
        case b of
            B1 -> case_B1 tableB b
            B2 -> case_B2 tableB b


instance Visit Table A where
    visit a = do
        tableA <- reader $ caseA
        case a of
            AB _ -> case_AB tableA a
            A -> case_A tableA a
default_TableB :: MT TableB m
default_TableB = TableB
    {case_B1 = default_case_B1
    ,case_B2 = default_case_B2
    }
default_case_B1 :: MM B m
default_case_B1 = return
default_case_B2 :: MM B m
default_case_B2 = return

default_TableA :: MT TableA m
default_TableA = TableA
    {case_A = default_case_A
    ,case_AB = default_case_AB
    }
default_case_A :: MM A m
default_case_A = return
default_case_AB :: MM A m
--default_case_AB (AB b) = visit b >>= return . AB
default_case_AB (AB b) = con1 AB b

con1 :: (Visit t a, MonadReader (t m) m) => (a->ca) -> a -> m ca
con1 con a = visit a >>= return . con
con2 :: (Visit t a, Visit t b, MonadReader (t m) m)
    => (a->b->cab) -> a -> b -> m cab
con2 con a b = do
    a' <- visit a
    b' <- visit b
    return $ con a' b'

default_Table :: MT Table m
default_Table = Table
    { caseA = default_TableA
    , caseB = default_TableB
    }

appTables :: (Visit t a, MonadReader (t m) m) => [t] -> (a -> m a)

after :: MonadReader r m => (a->a) -> M a m -> M a m
after a2a a2ma a = a2ma a >>= return . a2a -- fmap a2a $ a2ma a
updateB :: CM m ((B->B) -> Table m)
updateB f = table' where
    table = default_Table
    _caseB = caseB table
    __case_B1 = case_B1 _caseB
    __case_B1' = after f __case_B1
    __case_B2 = case_B2 _caseB
    __case_B2' = after f __case_B2
    _caseB' = _caseB {case_B1 = __case_B1', case_B2 = __case_B2'}
    table' = table {caseB = _caseB'}


t' :: MT Table m
t' = updateB id




