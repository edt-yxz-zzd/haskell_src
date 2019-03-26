{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}



module ListDefs.AsTupleBase
    ( Tuple (..)
    , AsTuple (..)
    , max_tuple_length
    , defs__instExplain_Tuple_AsTuple
    )
where

-- import ListDefs.List
import Seed.MonadOps (concatTFM)

--import Data.Kind
import Data.Typeable
import Data.Data
import Numeric.Natural
import GHC.Types (Nat)
import Data.Proxy
import GHC.TypeLits (type (+), type (-), type (<=?))
import GHC.Tuple
import Explain.ExplainBase
import Seed.Boxed
import TH.PrintQ
import TH.SeedUtils
import TH.Transform
import Control.Monad
type family Tuple (ts :: [*]) = (r :: *) | r -> ts where
    Tuple '[] = ()
    Tuple (h ': ts) = (h, Tuple ts)
    -- deriving (Eq, Ord, Read, Show, Typeable, Data)
--deriving instance Eq (Tuple '[])

type family GetTupleArgs a = (r :: [*]) | r -> a where
    GetTupleArgs () = '[]
    GetTupleArgs (h, ts) = h ': GetTupleArgs ts


newtype AsTuple ls = AsTuple (Tuple ls)
    --deriving (Eq, Ord, Read, Show, Typeable, Data)
    --deriving (Eq)
to_pair :: AsTuple (h ': ts) -> (h, AsTuple ts)
to_pair (AsTuple (h, ts)) = (h, AsTuple ts)

{-
deriving instance Eq (AsTuple '[])
deriving instance Ord (AsTuple '[])
--deriving instance Eq a => Eq (AsTuple '[a])
--deriving instance (Eq h, Eq (AsTuple ts)) => Eq (AsTuple (h ': ts))
instance (Eq a, Eq (AsTuple ts)) => Eq (AsTuple (a ': ts)) where
    a == b = to_pair a == to_pair b
instance (Ord a, Ord (AsTuple ts)) => Ord (AsTuple (a ': ts)) where
    a `compare` b = to_pair a `compare` to_pair b
-}

deriving instance (Eq (Tuple ls)) => Eq (AsTuple ls)
deriving instance (Ord (Tuple ls)) => Ord (AsTuple ls)
deriving instance (Show (Tuple ls)) => Show (AsTuple ls)
deriving instance (Read (Tuple ls)) => Read (AsTuple ls)
deriving instance (Typeable (Tuple ls)) => Typeable (AsTuple ls)
deriving instance (Typeable ls, Data (Tuple ls)) => Data (AsTuple ls)
_s = show $ AsTuple ('c', ("c", ()))





instance Boxed (AsTuple ls) where
    type BoxedFrom (AsTuple ls) = Tuple ls
    box = AsTuple
    unbox (AsTuple a) = a

-- type E a = (Tuple '[a])
-- instance Explain () (Tuple '[])
instance Explain (Unit a) (AsTuple '[a]) where
    explain (AsTuple (a, ())) = Unit a
instance Explain (AsTuple '[a]) (Unit a) where
    explain (Unit a) = AsTuple (a, ())
instance Make (Unit a) (AsTuple '[a]) where
instance Make (AsTuple '[a]) (Unit a) where


decs =
  [d|
    instance Explain (a, b) (AsTuple '[a,b]) where
        explain (AsTuple (a, (b, ()))) = (a, b)
    instance Explain (AsTuple '[a,b]) (a, b) where
        explain (a, b) = (AsTuple (a, (b, ())))
    instance Make (a, b) (AsTuple '[a,b]) where
    instance Make (AsTuple '[a,b]) (a, b) where
    |]

data Tpl = MkTpl
data AsTpl = MkAsTpl
decs' =
  [d|
    instance Explain Tpl AsTpl where
        explain MkAsTpl = MkTpl
        --explain _ = undefined
    instance Explain AsTpl Tpl where
        explain MkTpl = MkAsTpl
        --explain _ = undefined
    instance Make Tpl AsTpl where
    instance Make AsTpl Tpl where
    |]



makeAsTupleTyEx :: [Type] -> Type
makeAsTupleTy :: String -> Natural -> Type
makeAsTupleTy_fromLinkedTupleTy :: Type -> Type
makeAsTupleTy_fromLinkedTupleTy = AppT (ConT ''AsTuple)
    . getLinkedTupleTyArg
makeAsTupleTyEx = makeAsTupleTy_fromLinkedTupleTy . makeLinkedTupleTyEx
makeAsTupleTy prefix u = makeAsTupleTy_fromLinkedTupleTy
    $ makeLinkedTupleTy prefix u
------

makeAsTuplePatEx :: [Pat] -> Pat
makeAsTuplePatEx ps = ConP 'AsTuple [makeLinkedTuplePatEx ps]
makeAsTuplePat :: String -> Natural -> Pat
makeAsTuplePat prefix = makeAsTuplePatEx . makePatterns prefix

-------
makeAsTupleExpEx :: [Exp] -> Exp
makeAsTupleExp :: String -> Natural -> Exp
makeAsTupleExpEx = AppE (ConE 'AsTuple) . makeLinkedTupleExpEx
makeAsTupleExp prefix = makeAsTupleExpEx . makeExprs prefix


max_tuple_length :: Num a => a
max_tuple_length = 62
--concatTFM :: Monad m => [m [a]] -> m [a]
--concatTFM = fmap concat . sequence
defs__instExplain_Tuple_AsTuple = concatTFM . map def__instExplain_Tuple_AsTuple
def__instExplain_Tuple_AsTuple i = r6 where
    r0 = decs'
    r2 = fmap (post_modify f2 . post_modify f1) r0
    r4 = fmap (post_modify f4 . post_modify f3) r2
    r6 = fmap (post_modify f6 . post_modify f5) r4

    prefix = "a"
    f1 = modify_if pred1 modify1
    pred1 = findConTName "Tpl"
    modify1 = const $ makeTupleTy prefix i
    f2 = modify_if pred2 modify2
    pred2 = findConTName "AsTpl"
    modify2 = const $ makeAsTupleTy prefix i
    f3 = modify_if pred3 modify3
    pred3 = findConPName "MkTpl"
    modify3 = const $ makeTuplePat prefix i
    f4 = modify_if pred4 modify4
    pred4 = findConEName "MkAsTpl"
    modify4 = const $ makeAsTupleExp prefix i
    f5 = modify_if pred5 modify5
    pred5 = findConPName "MkAsTpl"
    modify5 = const $ makeAsTuplePat prefix i
    f6 = modify_if pred6 modify6
    pred6 = findConEName "MkTpl"
    modify6 = const $ makeTupleExp prefix i
    ----
    f = modify_if pred modify
    pred (ConT name) | nameBase name == "Tpl" = True
    pred _ = False
    modify _ = apps AppT (TupleT 2) -- (ConT ''(,))
        $ map (VarT . mkName . (prefix++) . show)
        $ [0..1]
        -- ConT ''()
pr = do
    let r = def__instExplain_Tuple_AsTuple 2
    pprintLnQ r
    pprintAsSrcLnQ r
    --pprintLnQ $ make_decs 2



