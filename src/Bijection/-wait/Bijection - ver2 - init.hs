{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}



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
import Prelude ((.), ($), Bool (..))
import Prelude
import Seed.Boxed

import Data.Proxy


unlabelP :: proxy c -> Label c a -> a
unlabelP _ = unbox
constLabelP :: proxy c -> Label c a -> Label c a
constLabelP _ = id
proxyf2label :: (Proxy c -> a) -> Label c a
proxyf2label f = r where
    r = box . f $ label2proxy r
--proxy2label :: proxy c -> Label c a
--proxy2label _ = undefined
--label2proxy :: Label c a -> proxy c
--label2proxy = undefined
label2proxy :: Label c a -> Proxy c
label2proxy _ = Proxy
toProxy :: a -> Proxy a
toProxy _ = Proxy

class Constraint c a where
    verifyP :: proxy c -> a -> Bool
    verifyP p a = unlabelP p (verifyL a)

    verifyL :: a -> Label c Bool
    verifyL a = proxyf2label (flip verifyP a)
    {-# MINIMAL verifyP | verifyL #-}
data NoConstraint = NoConstraint
    deriving (Eq, Ord, Show, Read)
instance Constraint NoConstraint a where
    verifyP _ _ = True
class Configure c a where
    extract_config :: a -> c
data NoConfigure = NoConfigure
    deriving (Eq, Ord, Show, Read)
instance Configure NoConfigure a where
    extract_config _ = NoConfigure
class TransformCls t where
    type TransformClsFrom t
    type TransformClsTo t
    transformP :: proxy t -> TransformClsFrom t -> TransformClsTo t
class (TransformCls t, TransformClsFrom t ~ a, TransformClsTo a ~ b)
    => TransformClsEx t a b where
instance (TransformCls t, TransformClsFrom t ~ a, TransformClsTo a ~ b)
    => TransformClsEx t a b where

class TransformObj obj where
    type TransformObjFrom obj
    type TransformObjTo obj
    type TransformObjFrom_Constraint obj :: *
    type TransformObjTo_Constraint obj :: *
    type TransformObjFrom_ConfigureTransform obj :: *
    type TransformObjTo_ConfigureTransform obj :: *

    type TransformObjFrom_Constraint obj = NoConstraint
    type TransformObjTo_Constraint obj = NoConstraint
    type TransformObjFrom_ConfigureTransform obj = ID_Transform NoConfigure
    type TransformObjTo_ConfigureTransform obj = ID_Transform NoConfigure


    transform
        :: proxy -> obj
        -> TransformObjFrom obj -> TransformObjTo obj

data ID_Transform a = ID_Transform
instance TransformCls (ID_Transform a) where
    type TransformClsFrom (ID_Transform a) = a
    type TransformClsTo (ID_Transform a) = a
    transformP _ = id
type family TransformClsCompose a b :: *


type family BijectionFrom_Configure a
type instance BijectionFrom_Configure a
    = TransformClsFrom (BijectionFrom_ConfigureTransform a)
type family BijectionTo_Configure a
type instance BijectionTo_Configure a
    = TransformClsFrom (BijectionTo_ConfigureTransform a)
type family Bijection_ConfigureTransformResult a
type instance Bijection_ConfigureTransformResult a
    = TransformClsTo (BijectionTo_ConfigureTransform a)


class
    ( Constraint (BijectionFrom_Constraint a) (BijectionFrom a)
    , Constraint (BijectionTo_Constraint a) (BijectionTo a)
    , Configure (BijectionFrom_Configure a) (BijectionFrom a)
    , Configure (BijectionTo_Configure a) (BijectionTo a)
    , TransformCls (BijectionFrom_ConfigureTransform a)
    , TransformClsFrom (BijectionFrom_ConfigureTransform a)
        ~ (BijectionFrom_Configure a)
    , TransformClsTo (BijectionFrom_ConfigureTransform a)
        ~ (Bijection_ConfigureTransformResult a)
    , TransformCls (BijectionTo_ConfigureTransform a)
    , TransformClsFrom (BijectionTo_ConfigureTransform a)
        ~ (BijectionTo_Configure a)
    , TransformClsTo (BijectionTo_ConfigureTransform a)
        ~ (Bijection_ConfigureTransformResult a)
    , Eq (Bijection_ConfigureTransformResult a)
    ) => Bijection a where
    type BijectionFrom a :: *
    type BijectionTo a :: *
    type BijectionFrom_Constraint a :: *
    type BijectionTo_Constraint a :: *
    --type BijectionFrom_Configure a :: *
    --type BijectionTo_Configure a :: *
    --type Bijection_ConfigureTransformResult a :: *
    type BijectionFrom_ConfigureTransform a :: *
    type BijectionTo_ConfigureTransform a :: *

    type BijectionFrom_Constraint a = NoConstraint
    type BijectionTo_Constraint a = NoConstraint
    --type BijectionFrom_Configure a = Bijection_ConfigureTransformResult a
    --type BijectionTo_Configure a = Bijection_ConfigureTransformResult a
    --type Bijection_ConfigureTransformResult a = NoConfigure
    type BijectionFrom_ConfigureTransform a = ID_Transform NoConfigure
        -- = ID_Transform (Bijection_ConfigureTransformResult a)
    type BijectionTo_ConfigureTransform a = ID_Transform NoConfigure
        -- = ID_Transform (Bijection_ConfigureTransformResult a)


    -- [verifyBijectionFrom _ from] -->>
    --      from === backward (forward from)
    --      let to = forward from
    --      [verifyBijectionTo _ to]
    --      transformP _ extract_config from
    -- [verifyP _   to] -->> to   === forward  (backward  to)
    -- to = forward from ==>> 
    forward :: a -> BijectionFrom a -> BijectionTo a
    backward :: a -> BijectionTo a -> BijectionFrom a

toBijectionFrom_ConstraintP
    :: Bijection a => proxy a -> Proxy (BijectionFrom_Constraint a)
toBijectionFrom_ConstraintP _ = Proxy
toBijectionTo_ConstraintP
    :: Bijection a => proxy a -> Proxy (BijectionTo_Constraint a)
toBijectionTo_ConstraintP _ = Proxy
verifyBijectionFromP :: Bijection a => proxy a -> BijectionFrom a -> Bool
verifyBijectionFromP = verifyP . toBijectionFrom_ConstraintP
verifyBijectionToP :: Bijection a => proxy a -> BijectionTo a -> Bool
verifyBijectionToP = verifyP . toBijectionTo_ConstraintP

verifyBijectionFrom :: Bijection a => a -> BijectionFrom a -> Bool
verifyBijectionFrom = verifyBijectionFromP . toProxy
verifyBijectionTo :: Bijection a => a -> BijectionTo a -> Bool
verifyBijectionTo = verifyBijectionToP . toProxy

toBijectionFrom_ConfigureTransformP
    :: Bijection a => proxy a -> Proxy (BijectionFrom_ConfigureTransform a)
toBijectionFrom_ConfigureTransformP _ = Proxy
toBijectionFrom_ConfigureP
    :: Bijection a => proxy a -> BijectionFrom a -> BijectionFrom_Configure a
toBijectionFrom_ConfigureP _ = extract_config
transform_BijectionFrom_Configure2Bijection_ConfigureTransformResultP
    :: Bijection a => proxy a -> BijectionFrom_Configure a
    -> Bijection_ConfigureTransformResult a
transform_BijectionFrom_Configure2Bijection_ConfigureTransformResultP =
    transformP . toBijectionFrom_ConfigureTransformP
toBijectionFrom_ConfigureTransformResultP
    :: Bijection a => proxy a -> BijectionFrom a
    -> Bijection_ConfigureTransformResult a
toBijectionFrom_ConfigureTransformResultP pa from =
    transform_BijectionFrom_Configure2Bijection_ConfigureTransformResultP
        pa (extract_config from)



toBijectionTo_ConfigureTransformP
    :: Bijection a => proxy a -> Proxy (BijectionTo_ConfigureTransform a)
toBijectionTo_ConfigureTransformP _ = Proxy
toBijectionTo_ConfigureP
    :: Bijection a => proxy a -> BijectionTo a -> BijectionTo_Configure a
toBijectionTo_ConfigureP _ = extract_config
transform_BijectionTo_Configure2Bijection_ConfigureTransformResultP
    :: Bijection a => proxy a -> BijectionTo_Configure a
    -> Bijection_ConfigureTransformResult a
transform_BijectionTo_Configure2Bijection_ConfigureTransformResultP =
    transformP . toBijectionTo_ConfigureTransformP
toBijectionTo_ConfigureTransformResultP
    :: Bijection a => proxy a -> BijectionTo a
    -> Bijection_ConfigureTransformResult a
toBijectionTo_ConfigureTransformResultP pa to =
    transform_BijectionTo_Configure2Bijection_ConfigureTransformResultP
        pa (extract_config to)


--extract_config_BijectionFromP



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




