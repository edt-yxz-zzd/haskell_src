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



class MonadReader t m => Visit t m a where
    visit :: a -> m a

-- method
type M a m = a -> m a
-- default context
type DC m typ = MonadReader (Table m) m => typ -- Monad m => typ
type DCM a m = DC m (M a m)
type C m typ = MonadReader (Table m) m => typ
type C_ m = MonadReader (Table m) m
type CM a m = C m (M a m)
data Table m = Table { tableA :: TableA m }
data TableA m = TableA { caseA1 :: M A m, caseA2 :: M A m }
data A = A1 | A2 deriving Show
data TableB m = TableB { caseB1 :: M B m, caseB2 :: M B m }
data B = B1 | B2 Int deriving Show

default_caseA1 :: DCM A m
default_caseA1 = return
default_caseA2 :: DCM A m
default_caseA2 = return
default_tableA :: DC m (TableA m)
default_tableA = TableA {caseA1 = default_caseA1, caseA2 = default_caseA2}
default_table :: DC m (Table m)
default_table = Table {tableA = default_tableA}

default_caseB1 :: DCM B m
default_caseB1 = return
default_caseB2 :: DCM B m
default_caseB2 (B2 a) = do
    a' <- visit a
    return $ B2 a'
default_tableB :: DC m (TableB m)
default_tableB = TableB {caseB1 = default_caseB1, caseB2 = default_caseB2}

update_caseA1_ :: C m ((A -> m A) -> TableA m -> TableA m)
update_caseA1_ f ta@TableA {caseA1 = ca1} = ta' where
    ca1' = ca1 >=> f
    ta' = ta {caseA1 = ca1'}
updateA_ :: C m ((A -> m A) -> TableA m -> TableA m)
updateA_ f ta@TableA {caseA1 = ca1, caseA2 = ca2} = ta' where
    -- ca1 = caseA1 ta
    ca1' = ca1 >=> f
    ca2' = ca2 >=> f
    ta' = ta {caseA1 = ca1', caseA2 = ca2'}
update_caseA1 :: C m ((A -> m A) -> Table m -> Table m)
update_caseA1 f t = t' where
    ta = tableA t
    ta' = update_caseA1_ f ta
    t' = t {tableA = ta'}

updateA :: C_ m => (A -> m A) -> Table m -> Table m
updateA f t = t' where
    ta = tableA t
    ta' = updateA_ f ta
    t' = t {tableA = ta'}

visitBy_ :: (t_~Table, m_~(->), m~RM t_ m_, t~t_ m, Visit t m a)
    => a -> m a
visitBy_ = visit
visitBy :: (t_~Table, m_~(->), m~RM t_ m_, t~t_ m, Visit t m a)
    => a -> Table m -> a
visitBy = unRM . visitBy_

table__A1toA2 :: C m (Table m)
table__A1toA2 = update_caseA1 (return . _flipA1_A2) default_table where
table__flipA1_A2 :: C m (Table m)
table__flipA1_A2 = updateA (return . _flipA1_A2) default_table where
_flipA1_A2 = f where
    f A1 = A2
    f A2 = A1
flipA1_A2 :: A -> A
flipA1_A2 a = visitBy a table__flipA1_A2
flipA1 :: A -> A
flipA1 a = visitBy a table__A1toA2



instance MonadReader (Table m) m => Visit (Table m) m Int where
    visit = return
instance MonadReader (Table m) m => Visit (Table m) m A where
    visit x = let table = tableA in case x of
        A1 -> do
            caseA1_ <- asks $ caseA1 . table
            caseA1_ x
        A2 -> do
            caseA2_ <- asks $ caseA2 . table
            caseA2_ x

newtype RT t_ rT (m :: * -> *) a = RT {unRT :: rT (t_ (RT t_ rT m)) m a}
deriving instance Monad (rT (t_ (RT t_ rT m)) m) => Monad (RT t_ rT m)

instance MonadReader (t_ (RT t_ rT m)) (rT (t_ (RT t_ rT m)) m)
    => MonadReader (t_ (RT t_ rT m)) (RT t_ rT m) where
    ask = RT ask
    local f (RT ma) = RT $ local f ma
    reader = RT . reader

newtype RM t_ t2m a = RM {unRM :: t2m (t_ (RM t_ t2m)) a}
deriving instance Monad (t2m (t_ (RM t_ t2m))) => Monad (RM t_ t2m)
instance MonadReader (t_ (RM t_ t2m))(t2m (t_ (RM t_ t2m)))
    => MonadReader (t_ (RM t_ t2m))(RM t_ t2m) where
    ask = RM ask
    local f (RM ma) = RM $ local f ma
    reader = RM . reader

typeAssertMonadReader :: MonadReader r m => m a -> m a
typeAssertMonadReader = id

_test1 = typeAssertMonadReader (undefined :: RM Table (->) a)
_test2 = typeAssertMonadReader (undefined :: RT Table ReaderT IO a)






