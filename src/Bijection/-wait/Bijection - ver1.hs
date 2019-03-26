{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}




module Bijection.Bijection
    ( Bijection (..)
    , BijectionObj ()
    , make_bijection_obj
    , reverse_bijection
    --, ChainBijection ()
    --, make_chain_bijection
    )
where

import Lambda.CombinatoryLogic (CategoryF (..))
import qualified Control.Category as CC
import qualified Prelude as P
import Prelude ((.), ($))


class Bijection a where
    type BijectionFrom a :: *
    type BijectionTo a :: *
    forward :: a -> BijectionFrom a -> BijectionTo a
    backward :: a -> BijectionTo a -> BijectionFrom a

newtype RevserseBijection a = RevserseBijection a
instance Bijection a => Bijection (RevserseBijection a) where
    type BijectionFrom (RevserseBijection a) = BijectionTo a
    type BijectionTo (RevserseBijection a) = BijectionFrom a
    forward (RevserseBijection a) = backward a
    backward (RevserseBijection a) = forward a



{-

bijection_vtable
    :: Bijection a => BijectionVTable (BijectionFrom a) (BijectionTo a) a
bijection_vtable = BVT {_forward = forward, _backward = backward}
data BijectionVTable from_ to_ a = BVT
    { _forward :: a -> from_ -> to_
    , _backward :: a -> to_ -> from_
    }


data BijectionObj from_ to_ = forall a. BiObj
    { _obj :: a
    , _vtable :: BijectionVTable from_ to_ a
    }

make_bijection_obj
    :: Bijection a => a -> BijectionObj (BijectionFrom a) (BijectionTo a)
make_bijection_obj a = BiObj {_obj = a, _vtable = bijection_vtable}
instance Bijection (BijectionObj a b) where
    type BijectionFrom (BijectionObj a b) = a
    type BijectionTo (BijectionObj a b) = b
    forward BiObj {_obj = a, _vtable = table} = _forward table a
    backward BiObj {_obj = a, _vtable = table} = _backward table a
-}

data BijectionObj from_ to_ where
    BiObj
        :: Bijection a
        -- => a -> BijectionVTable from_ to_ a -> BijectionObj from_ to_
        => a -> BijectionObj (BijectionFrom a) (BijectionTo a)

make_bijection_obj
    :: Bijection a => a -> BijectionObj (BijectionFrom a) (BijectionTo a)
--make_bijection_obj a = BiObj a bijection_vtable
make_bijection_obj = BiObj
instance Bijection (BijectionObj a b) where
    type BijectionFrom (BijectionObj a b) = a
    type BijectionTo (BijectionObj a b) = b
    --forward (BiObj a table) = _forward table a
    --backward (BiObj a table) = _backward table a
    forward (BiObj a) = forward a
    backward (BiObj a) = backward a

reverse_bijection :: BijectionObj f t -> BijectionObj t f
reverse_bijection = make_bijection_obj . RevserseBijection





data ChainBijection outer inner = CB outer inner
instance (Bijection a, Bijection b, BijectionTo a ~ BijectionFrom b)
    => Bijection (ChainBijection a b) where
    type BijectionFrom (ChainBijection a b) = BijectionFrom a
    type BijectionTo (ChainBijection a b) = BijectionTo b
    forward (CB a b) = forward b . forward a
    backward (CB a b) = backward a . backward b
make_chain_bijection
    :: (Bijection a, Bijection b)
    => a -> b -> ChainBijection a b
make_chain_bijection = CB







data Bijection__id a
instance Bijection (Bijection__id a) where
    type BijectionFrom (Bijection__id a) = a
    type BijectionTo (Bijection__id a) = a
    forward = P.const P.id
    backward = forward

bijection_obj__id :: BijectionObj a a
bijection_obj__composition
    :: (Bijection a, Bijection b, BijectionTo a ~ BijectionFrom b)
    => b -> a -> BijectionObj (BijectionFrom a) (BijectionTo b)
bijection_obj__id = make_bijection_obj (P.undefined :: Bijection__id a)
bijection_obj__composition b a =
    make_bijection_obj $ make_chain_bijection a b


instance CC.Category BijectionObj where
    id = bijection_obj__id
    (.) = bijection_obj__composition
instance CategoryF BijectionObj where
    _id_ = CC.id
    (<*>) = (CC..)




