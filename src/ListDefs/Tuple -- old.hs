
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Seed.Tuple
    ( --module Seed.ListType
    Tuple (..)
    )
where

import Data.Kind
import Data.Typeable

data Tuple (a :: [*]) where
--data Tuple a where
    (:/) :: h -> Tuple ts -> Tuple (h ': ts)
    Empty :: Tuple '[]
    -- deriving (Eq, Ord, Read, Show)

--deriving instance Read (Tuple '[])
--deriving instance Read (Tuple (h ': ts))

infixr 5 :/ , //
(//) :: h -> tt -> Tuple '[h,tt]
h // t = h :/ t :/ Empty

to_pair :: Tuple (h ': ts) -> (h, Tuple ts)
to_pair (h :/ ts) = (h, ts)
type family RPair (a :: [*])
type instance RPair '[] = ()
type instance RPair (h ': ts) = (h, RPair ts)
to_rpair :: Tuple a -> RPair a
to_rpair Empty = ()
to_rpair (h :/ ts) = (h, to_rpair ts)
instance Eq (Tuple '[]) where
    _ == _ = True
instance Ord (Tuple '[]) where
    compare _ _ = EQ
instance (Eq h, Eq (Tuple ts)) => Eq (Tuple (h ': ts)) where
    lhs == rhs = to_pair lhs == to_pair rhs
instance (Ord h, Ord (Tuple ts)) => Ord (Tuple (h ': ts)) where
    compare lhs rhs = to_pair lhs `compare` to_pair rhs

{-
data Tuple t (a :: [t]) where
    (:/) :: (h :: t) -> Tuple t (ts :: [t]) -> Tuple t (h : ts)
    (:/:) :: (h :: t) -> tt -> Tuple t [h,tt]
    Empty :: Tuple t '[]
--}


a = (1::Int) :/ 'c' // ([]::String)
main = do
    print $ typeOf a


