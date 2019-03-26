{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}


module TH.DefNew (def__instNewBoxed, def__instNewBoxed__byName)
where

import Seed.Boxed
import TH.SeedUtils
import TH.Transform (post_modify)

data XXX a = MkXXX { unXXX :: a }
mkXXX = MkXXX
decs = [d|
    instance New XXX where
        wrap = mkXXX
        unwrap = unXXX
    instance Boxed (XXX a) where
        type BoxedFrom (XXX a) = a
        box = wrap
        unbox = unwrap
    |]


def__instNewBoxed__byName :: Name -> Name -> Name -> DecsQ
def__instNewBoxed__byName t mk un = def__instNewBoxed
    (ConT t) (name2exp mk) (name2exp un)
def__instNewBoxed :: Type -> Exp -> Exp -> DecsQ
def__instNewBoxed _XXX mkXXX unXXX = do
    a <- newName "a"
    f $ VarT a
  where
    f a = fmap g decs where
        -- post_modify
        g = post_modify f3 . post_modify f2 . post_modify f1 . post_modify f0
        f0 = replace_if (findVarTName "a") (const a)
        f1 = replace_if (findConTName "XXX") (const _XXX)
        f2 = replace_if (findVarEName "mkXXX") (const mkXXX)
        f3 = replace_if (findVarEName "unXXX") (const unXXX)
{-
ConT (Name "XXX")
VarE (Name "mkXXX")
VarE (Name "unXXX")
VarT (Name "a")
-}

decs' = def__instNewBoxed__byName (mkName "YYY") (mkName "mk") (mkName "un")
pr = do
    pprintLnQ decs'
    pprintAsSrcLnQ decs'

