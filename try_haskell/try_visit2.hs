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

-- import Control.Monad.Reader
import Control.Arrow


class Arrow a => ArrDouble a where
    -- (x->x->y) -> (x->y)
    -- double f x = f x x
    arrDouble :: (a x (a x y)) -> (a x y)
    -- a x (a x y, x) >>> app === a x y
    -- a x (a x y) &&& arr id === a x (a x y, x)
    -- arrDouble a_x_axy = a_x_axy &&& arr id >>> app
    --
    default arrDouble :: ArrowApply a => (a x (a x y)) -> (a x y)
    arrDouble a_x_axy = a_x_axy &&& arr id >>> app
    case_arr :: (x -> (a x y)) -> (a x y)
    case_arr = arrDouble . arr
instance ArrDouble (->) where
    -- arrDouble f x = f x x
{-
class Arrow a => CaseArr a where
    -- (x->x->y) -> (x->y)
    -- double f x = f x x
    case_arr :: (x -> a x y) -> (a x y)
    case_arr f = case_arr_ex $ arr f
    case_arr_ex :: (a x (a x y)) -> (a x y)
    case_arr_ :: a (a x (a x y)) (a x y)
    case_arr_ = arr case_arr_ex
    -- case_arr :: a (a x z) z
instance CaseArr (->) where
    case_arr_ex x2x2y x = x2x2y x x
-}



{-
class (Arrow a) => Visit a v where
    visit :: t -> a v v
class (Visit a (TableType a, v), VisitBy_Arr a)
    => VisitBy a v where
    visitBy :: TableType a -> a v v
    visitBy t = arr ((,) t) >>> visit_byArrIn >>> arr snd
class Arrow a => VisitBy_Arr a where
    type TableType a :: *
    --type VisitBy_ArrIn a :: * -> * -> *
    visit_byArrIn :: Visit a (TableType a, v) => a (TableType a, v) (TableType a, v)
    visit_byArrIn = visit
-}
class (Arrow (Ax t), Arrow (Ai t)) => VisitTable t where
    type Ax t :: * -> * -> *
    type Ai t :: * -> * -> *
class (VisitTable t) => Visit t v where
    -- visit :: (Ax t) t ((Ai t) v v)
    visit :: Ax t t (Ai t v v)

{-
type M a ar = ar a a
type MM_ t a (ar :: * -> * -> *) = Arrow ar => M a ar
type MT_ subt (ar :: * -> * -> *) = Arrow ar => subt ar
type M a m = a -> m a
type MM_ t a m = MonadReader (t m) m => a -> m a
type CM_ t m a = MonadReader (t m) m => a
-}























data A = A | AB B
    deriving (Show)
data B = B1 | B2
    deriving (Show)

type M_ ax ai t v = ax t (ai v v)
type M v ax ai = M_ ax ai (Table ax ai) v
data Table x i = Table { tableA :: TableA x i }
data TableA x i = TableA { caseA :: M A x i }

instance (Arrow x, Arrow i) => VisitTable (Table x i) where
    type Ax (Table x i) = x
    type Ai (Table x i) = i
class (Arrow a, Arrow b) => Run a b where
    run :: a x y -> b x y
class (Arrow ex, Arrow i) => Input ex i where
    input :: a -> ex (i a b) b

lookupTable
    :: (Arrow ex, Arrow i, Input (->) ex, ArrDouble i)
    => (t -> v -> ex t (i v v)) -> ex t (i v v)
    -- Input ex u
    -- input t :: u (ex t ?) ?
    -- look t v :: ex t (i v v)
    -- let ? = i v v
    -- u (ex t ?) ?  $  ex t ?  === ?
lookupTable look = arr $ \t -> case_arr (\v -> input t $ look t v)
instance (Arrow ex, Arrow i, Input (->) ex, ArrDouble i) => Visit (Table ex i) A where
    -- (t -> v -> x t (i v v)) -> x t (i v v)
    -- t -> x t (i v v)
    visit = lookupTable $ \t v -> case v of
        -- caseA $ tableA t :: x t (i A A)
        A -> caseA $ tableA t
    {-
    visit = arr (\t -> case_arr (f t)) where
        f t a = case a of
            -- caseA $ tableA t :: x t (i A A)
            A -> input t . caseA $ tableA t
    -}
    {-
    visit = arr f where
        -- f :: t -> i A A
        f t = iAA where
            ta = tableA t
            -- caseA ta :: x t (i A A)
            -- iAA :: i A A
            iAA = input t $ caseA ta
    -}







{-
{-
type MM a m = MM_ a m
type MT subt m = MT_ subt m
type CM m a = CM_ Table m a
-}
{-
type MM a m = MonadReader (Table m) m => a -> m a
type MT t m = MonadReader (Table m) m => t m
type CM m a = MonadReader (Table m) m => a
-}
data Table m = Table { caseA :: TableA m, caseB :: TableB m }
data TableA m = TableA { case_A :: M A m, case_AB :: M A m }
data TableB m = TableB { case_B1 :: M B m, case_B2 :: M B m }

default_TableB :: MT TableB m
default_TableB = TableB
    {case_B1 = default_case_B1
    ,case_B2 = undefined
    }
default_case_B1 :: MM B m
default_case_B1 = returnA
instance Arrow m => Visit m (Table m) B where
    visit table = arr f where
        tableB = caseB table
        f b = case b of
            B1 -> case_B1 tableB b
            B2 -> case_B2 tableB b
    {-
    visit b = do
        tableB <- reader $ caseB
        case b of
            B1 -> case_B1 tableB b
            B2 -> case_B2 tableB b
    -}

{-
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



--}
--}
--}
--}
--}
--}
