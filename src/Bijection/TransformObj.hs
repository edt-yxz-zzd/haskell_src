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
{-# LANGUAGE TypeOperators #-}



module Bijection.TransformObj
    ( Constraint (..)
    , NoConstraint (..)
    , Configure (..)
    , NoConfigure (..)
    , PairLabel (..), pairLabel, (:&&:)(..), (:||:)(..)
    , (:.:)(..)
    -- , First_Transform(..), Swap_Transform(..), Second_Transform(..)
    , FirstL_Transform(..), SwapL_Transform(..), SecondL_Transform(..)
    , Relabel_Transform(..)

    , FirstL_TransformF (..)
    , Relabel_TransformF (..)
    , TransformCls (..)
    , TransformClsEx (..)
    , ID_Transform (..)
    , TransformClsCompose (..)

    , TransformArgTpl (..)
    , TransformObj (..)

    , argTpl2ConstraintP
    , verifyArgP
    , argTpl2ConfigureTransformClsP
    , arg2ConfigP
    , arg2ConfigResultP


    , transformObj2FromTpl
    , verifyTransformObjFromP
    , transformObjFrom2ConfigP
    , transformObjFrom2ConfigResultP

    , transformObj2ToTpl
    , verifyTransformObjToP
    , transformObjTo2ConfigP
    , transformObjTo2ConfigResultP

    , verifyConfigResultEqP
    , transformWithFullVerify
    , transformWithFullVerify_
    )
where

import Seed.Boxed
import Seed.MaybeOps (bool2maybe, fromJust)
import Data.Proxy
import Seed.ProxyOps (last2P, last1P, toProxy)
import ADT.Pair
import Prelude hiding (fst, snd)

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






-- to be used in Constraint
data a :&&: b = a :&&: b
    deriving (Eq, Ord, Read, Show)
type (:||:) = Either
    --deriving (Eq, Ord, Read, Show)
infixr 3 :&&:
infixr 2 :||:


type instance ToPair (Proxy (:&&:)) a b = (a :&&: b)
instance ISwap (a :&&: b) where
    type Swap (a :&&: b) = (b :&&: a)
instance ISwapT (Proxy (:&&:)) where
    type SwapT (Proxy (:&&:)) = Proxy (:&&:)
instance IPairBase (a :&&: b) where
    type Fst (a :&&: b) = a
    type Snd (a :&&: b) = b
    type PairT (a :&&: b) = Proxy (:&&:)
    to_pair (a :&&: b) = (a, b)
    mkPair = (:&&:)
    swap (a :&&: b) = (b :&&: a)
{-
fstA :: a :&&: b -> a
sndA :: a :&&: b -> b
swapA :: a :&&: b -> b :&&: a
fstA (a :&&: b) = a
sndA (a :&&: b) = b
swapA (a :&&: b) = b :&&: a
-}


type family PairLabel x y where
    PairLabel (Label a b) (Label c d) = Label (a :&&: c) (b :&&: d)
pairLabel :: Label a b -> Label c d -> Label (a :&&: c) (b :&&: d)
pairLabel ab cd = box (unbox ab :&&: unbox cd)
{-
fstLA :: Label (a :&&: c) (b :&&: d) -> Label a b
sndLA :: Label (a :&&: c) (b :&&: d) -> Label c d
swapLA :: Label (a :&&: c) (b :&&: d) -> Label (c :&&: a) (d :&&: b)
fstLA = mapGBox fstA
sndLA = mapGBox sndA
swapLA = mapGBox swapA
-}

instance (Configure (Label explain0 c0) a, Configure (Label explain1 c1) a)
    => Configure (Label (explain0 :&&: explain1) (c0 :&&: c1)) a where
    extract_config a = extract_config a `pairLabel` extract_config a

instance (Constraint c0 a, Constraint c1 a) => Constraint (c0 :&&: c1) a where
    verifyP p a = b0 && b1 where
        b0 = verifyP (last2P p) a
        b1 = verifyP (last1P p) a
instance (Constraint c0 a, Constraint c1 a) => Constraint (c0 :||: c1) a where
    verifyP p a = b0 || b1 where
        b0 = verifyP (last2P p) a
        b1 = verifyP (last1P p) a












class TransformCls t where
    type TransformClsFrom t
    type TransformClsTo t
    transformP :: proxy t -> TransformClsFrom t -> TransformClsTo t
class (TransformCls t, TransformClsFrom t ~ a, TransformClsTo a ~ b)
    => TransformClsEx t a b where
instance (TransformCls t, TransformClsFrom t ~ a, TransformClsTo a ~ b)
    => TransformClsEx t a b where

data ID_Transform a = ID_Transform
instance TransformCls (ID_Transform a) where
    type TransformClsFrom (ID_Transform a) = a
    type TransformClsTo (ID_Transform a) = a
    transformP _ = id
type family TransformClsCompose a b :: *




data a :.: b = a :.: b
    deriving (Eq, Ord, Show, Read)
instance (TransformCls a, TransformCls b
        , TransformClsFrom a ~ TransformClsTo b
        ) => TransformCls (a :.: b) where
    type TransformClsFrom (a :.: b) = TransformClsFrom b
    type TransformClsTo (a :.: b) = TransformClsTo a
    transformP p = transformP (last2P p) . transformP (last1P p)


{-
data First_Transform a b = First_Transform
    deriving (Eq, Ord, Show, Read)
data Swap_Transform a b = Swap_Transform
    deriving (Eq, Ord, Show, Read)
type Second_Transform a b = First_Transform b a :.: Swap_Transform a b
instance TransformCls (First_Transform a b) where
    type TransformClsFrom (First_Transform a b) = a :&&: b
    type TransformClsTo (First_Transform a b) = a
    transformP _ (a :&&: _) = a
instance TransformCls (Swap_Transform a b) where
    type TransformClsFrom (Swap_Transform a b) = a :&&: b
    type TransformClsTo (Swap_Transform a b) = b :&&: a
    transformP _ (a :&&: b) = (b :&&: a)
-}

data FirstL_Transform p = FirstL_Transform
    deriving (Eq, Ord, Show, Read)
data SwapL_Transform p = SwapL_Transform
    deriving (Eq, Ord, Show, Read)
--type SecondL_Transform p = FirstL_Transform (Swap p) :.: SwapL_Transform p
type SecondL_Transform p = FirstL_Transform (SwapL_Transform p)
instance (Pair p) => TransformCls (FirstL_Transform p) where
    type TransformClsFrom (FirstL_Transform p) = p
    type TransformClsTo (FirstL_Transform p) = Fst p
    transformP _ = fst
instance (Pair p) => TransformCls (SwapL_Transform p) where
    type TransformClsFrom (SwapL_Transform p) = p
    type TransformClsTo (SwapL_Transform p) = Swap p
    transformP _ = swap


data Relabel_Transform n o a
instance TransformCls (Relabel_Transform n o a) where
    type TransformClsFrom (Relabel_Transform n o a) = Label o a
    type TransformClsTo (Relabel_Transform n o a) = Label n a
    transformP _ = relabel








class
    ( Constraint (ArgConstraint a) (ArgType a)
    , Configure (ArgConfigure a) (ArgType a)
    , TransformCls (ArgConfigureTransformCls a)
    , ArgConfigure a ~ TransformClsFrom (ArgConfigureTransformCls a)
    , ArgConfigureResult a ~ TransformClsTo (ArgConfigureTransformCls a)
    , Eq (ArgConfigureResult a)
    ) => TransformArgTpl a where
    type ArgType a
    type ArgConstraint a
    type ArgConfigure a
    type ArgConfigureTransformCls a
    type ArgConfigureResult a


    type ArgConstraint a = NoConstraint
    type ArgConfigureTransformCls a = ID_Transform (ArgConfigure a)
    --type ArgConfigure a = NoConfigure
    type ArgConfigure a = TransformClsFrom (ArgConfigureTransformCls a)
    type ArgConfigureResult a = TransformClsTo (ArgConfigureTransformCls a)

instance (TransformArgTpl a
        , TransformCls t
        , TransformClsFrom t ~ ArgConfigureResult a
        , Eq (ArgConfigureResult (t :.: a))
    ) => TransformArgTpl (t :.: a) where
    type ArgType (t :.: a) = ArgType a
    type ArgConstraint (t :.: a) = ArgConstraint a
    type ArgConfigure (t :.: a) = ArgConfigure a
    type ArgConfigureTransformCls (t :.: a) = t :.: ArgConfigureTransformCls a

type FirstL_TransformF a =
    FirstL_Transform (ArgConfigureResult a) :.: a

type Relabel_TransformF n a =
    Relabel_Transform n
        (Label2Name (ArgConfigureResult a))
        (Label2Data (ArgConfigureResult a))
        :.: a



argTpl2ConstraintP
    :: TransformArgTpl a => proxy a -> Proxy (ArgConstraint a)
argTpl2ConstraintP _ = Proxy
verifyArgP :: TransformArgTpl a => proxy a -> ArgType a -> Bool
verifyArgP = verifyP . argTpl2ConstraintP

argTpl2ConfigureTransformClsP
    :: TransformArgTpl a => proxy a -> Proxy (ArgConfigureTransformCls a)
argTpl2ConfigureTransformClsP _ = Proxy
arg2ConfigP :: TransformArgTpl a => proxy a -> ArgType a -> ArgConfigure a
arg2ConfigP _ = extract_config
arg2ConfigResultP
    :: TransformArgTpl a => proxy a -> ArgType a -> ArgConfigureResult a
arg2ConfigResultP pa arg =
    transformP (argTpl2ConfigureTransformClsP pa) (arg2ConfigP pa arg)


class
    ( TransformArgTpl (ArgTplFrom a)
    , TransformArgTpl (ArgTplTo a)
    , ArgConfigureResult (ArgTplFrom a) ~ ArgConfigureResult (ArgTplTo a)

    , TransformObjFrom a ~ ArgType (ArgTplFrom a)
    , TransformObjTo a ~ ArgType (ArgTplTo a)
    ) => TransformObj a where
    type ArgTplFrom a
    type ArgTplTo a

    -- [verifyP _ from] -->>
    --      let to = transform _ from
    --      [verifyP _ to][verifyConfigResultEqP _ from to]
    --      transformP _ (extract_config from)
    --          == transformP _ (extract_config to)
    transform :: a -> TransformObjFrom a -> TransformObjTo a
    -- default transform :: TransformCls a => a -> TransformObjFrom a -> TransformObjTo a
    type TransformObjFrom a
    type TransformObjTo a
    type TransformObjFrom a = ArgType (ArgTplFrom a)
    type TransformObjTo a = ArgType (ArgTplTo a)



transformObj2FromTpl :: TransformObj a => proxy a -> Proxy (ArgTplFrom a)
transformObj2FromTpl _ = Proxy
verifyTransformObjFromP
    :: TransformObj a => proxy a -> TransformObjFrom a -> Bool
verifyTransformObjFromP = verifyArgP . transformObj2FromTpl
transformObjFrom2ConfigP
    :: TransformObj a => proxy a
    -> TransformObjFrom a -> ArgConfigure (ArgTplFrom a)
transformObjFrom2ConfigP = arg2ConfigP . transformObj2FromTpl
transformObjFrom2ConfigResultP
    :: TransformObj a => proxy a
    -> TransformObjFrom a -> ArgConfigureResult (ArgTplFrom a)
transformObjFrom2ConfigResultP = arg2ConfigResultP . transformObj2FromTpl

transformObj2ToTpl :: TransformObj a => proxy a -> Proxy (ArgTplTo a)
transformObj2ToTpl _ = Proxy
verifyTransformObjToP
    :: TransformObj a => proxy a -> TransformObjTo a -> Bool
verifyTransformObjToP = verifyArgP . transformObj2ToTpl
transformObjTo2ConfigP
    :: TransformObj a => proxy a
    -> TransformObjTo a -> ArgConfigure (ArgTplTo a)
transformObjTo2ConfigP = arg2ConfigP . transformObj2ToTpl
transformObjTo2ConfigResultP
    :: TransformObj a => proxy a
    -> TransformObjTo a -> ArgConfigureResult (ArgTplTo a)
transformObjTo2ConfigResultP = arg2ConfigResultP . transformObj2ToTpl


verifyConfigResultEqP
    :: TransformObj a => proxy a
    -> TransformObjFrom a -> TransformObjTo a -> Bool
verifyConfigResultEqP pa from to =
    transformObjFrom2ConfigResultP pa from
        == transformObjTo2ConfigResultP pa to

transformWithFullVerify_
    :: TransformObj a => a -> TransformObjFrom a -> TransformObjTo a
transformWithFullVerify_ a from = fromJust $ transformWithFullVerify a from
transformWithFullVerify
    :: TransformObj a => a
    -> TransformObjFrom a -> Maybe (TransformObjTo a)
transformWithFullVerify a from = r where
    pa = toProxy a
    b0 = verifyTransformObjFromP pa from
    to = transform a from
    b1 = verifyTransformObjToP pa to
    b2 = verifyConfigResultEqP pa from to
    r = bool2maybe (and [b0, b1, b2]) to

