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
{-# LANGUAGE DefaultSignatures #-}



module Bijection.BijectionC
where



import Lambda.CombinatoryLogic (CategoryF (..))
import qualified Control.Category as CC
import Seed.Boxed
import Data.Proxy
import Bijection.TransformObj
import TH.DefNew (def__instNewBoxed__byName)



{-
type family BijectionFrom_Configure a
type instance BijectionFrom_Configure a
    = TransformClsFrom (BijectionFrom_ConfigureTransform a)
type family BijectionTo_Configure a
type instance BijectionTo_Configure a
    = TransformClsFrom (BijectionTo_ConfigureTransform a)
type family Bijection_ConfigureTransformResult a
type instance Bijection_ConfigureTransformResult a
    = TransformClsTo (BijectionTo_ConfigureTransform a)
-}





data Forward a = Forward { unForward :: a }
data Backward a = Backward { unBackward :: a }
$(def__instNewBoxed__byName ''Forward 'Forward 'unForward)
$(def__instNewBoxed__byName ''Backward 'Backward 'unBackward)



class
    ( TransformObj (ForwardTransform a)
    , TransformObj (BackwardTransorm a)
    , ArgTplFrom (ForwardTransform a) ~ ArgTplTo (BackwardTransorm a)
    , ArgTplTo (ForwardTransform a) ~ ArgTplFrom (BackwardTransorm a)
    , BijectionCFrom a ~ ArgType (ArgTplFrom (ForwardTransform a))
    , BijectionCTo a ~ ArgType (ArgTplTo (ForwardTransform a))
    , BijectionCFromC a ~ ArgConstraint (ArgTplFrom (ForwardTransform a))
    , BijectionCToC a ~ ArgConstraint (ArgTplTo (ForwardTransform a))
    ) => BijectionC a where
    type ForwardTransform a
    type BackwardTransorm a
    type ForwardTransform a = Forward a
    type BackwardTransorm a = Backward a
    toForwardTransform :: a -> ForwardTransform a
    toBackwardTransorm :: a -> BackwardTransorm a

    default toForwardTransform :: a -> Forward a
    toForwardTransform = Forward
    default toBackwardTransorm :: a -> Backward a
    toBackwardTransorm = Backward

    -- [verifyP _ from] -->> from === backward (forward from)
    -- [verifyP _   to] -->> to   === forward  (backward  to)
    type BijectionCFrom a
    type BijectionCTo a
    type BijectionCFrom a = ArgType (ArgTplFrom (ForwardTransform a))
    type BijectionCTo a = ArgType (ArgTplTo (ForwardTransform a))
    type BijectionCFromC a
    type BijectionCToC a
    type BijectionCFromC a = ArgConstraint (ArgTplFrom (ForwardTransform a))
    type BijectionCToC a = ArgConstraint (ArgTplTo (ForwardTransform a))

forward
    :: BijectionC a => a
    -> Label (BijectionCFromC a) (BijectionCFrom a)
    -> Label (BijectionCToC a) (BijectionCTo a)
forward = mapGBox . forward_
backward
    :: BijectionC a => a
    -> Label (BijectionCToC a) (BijectionCTo a)
    -> Label (BijectionCFromC a) (BijectionCFrom a)
backward = mapGBox . backward_

forward_, forwardWithFullVerify_
    :: BijectionC a => a -> BijectionCFrom a -> BijectionCTo a
forward_ = transform . toForwardTransform
backward_, backwardWithFullVerify_
    :: BijectionC a => a -> BijectionCTo a -> BijectionCFrom a
backward_ = transform . toBackwardTransorm

forwardWithFullVerify_ = transformWithFullVerify_ . toForwardTransform
backwardWithFullVerify_ = transformWithFullVerify_ . toBackwardTransorm

forwardWithFullVerifyEq_
    :: (BijectionC a, Eq (BijectionCFrom a))
    => a -> BijectionCFrom a -> BijectionCTo a
forwardWithFullVerifyEq_ a from = r where
    to = forwardWithFullVerify_ a from
    from' = backward_ a to
    r = if from == from' then to else error "backward . forward =/= id"

backwardWithFullVerifyEq_
    :: (BijectionC a, Eq (BijectionCTo a))
    => a -> BijectionCTo a -> BijectionCFrom a
backwardWithFullVerifyEq_ a to = r where
    from = backwardWithFullVerify_ a to
    to' = forward_ a from
    r = if to == to' then from else error "forward . backward =/= id"


