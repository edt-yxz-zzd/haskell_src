{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Lambda.Lambda where

import Control.Arrow
import Control.Category
import Prelude hiding((.), id)
import Container.IContainer
import Container.IDynSet
import Container.OpEmpty
import Container.BufferOps
import Container.Instances__Set
import Container.DynSetOps
import Seed.ArrowOps
import Seed.OpSwap
import Data.Semigroup
import Data.Set (Set)
import ADT.IArrowCount


data AAV n t = NotAAV | Abs n t | App t t | Var n
data ApplyView t = NotApply | ApplyView t t

class Show n => IName n where
    make_name :: String -> n


class ILambdaTermBase t where
    apply :: t -> t -> t
class (ILambdaTermBase t
    , IDynSet (Variables t)
    , Element (Variables t) ~ Name t
    , IName (Name t)
    ) => ILambdaTerm t where
    type Name t
    type Variables t
    variable :: Name t -> t
    abstract :: Name t -> t -> t
    frees_of :: t -> (Variables t)
    -- vars_of :: t -> (Variables t)
    is_closed :: t -> Bool
    is_closed = frees_of >>> is_empty

class LambdaTermViewBase t where
    view_apply :: t -> ApplyView t
class LambdaTermView t where
    view_aav :: t -> AAV (Name t) t


class (Arrow arr, ILambdaTermBase (Term arr)
    ) => ILambdaBuilderBase arr where
    type Term arr
class (Arrow arr, ILambdaBuilderBase arr, ILambdaTerm (Term arr)
    ) => IUntypedLambdaBuilder arr where
    new_nameA :: arr String (Name (Term arr))
    -- new_nameA = (id &&& known_namesA) >>^ \(str, set) -> 
    -- known_namesA :: arr x (Variables (Term arr))
    default new_nameA
        :: (IArrowCount arr, ArrowCount arr ~ i
            , Name (Term arr) ~ NameT i n, IName n, Show i
            )
        => arr String (NameT i n)
    new_nameA = (arr make_name &&& tickA) >>^ \(n, i) -> NameT i n

data NameT i n = NameT i n | PlainOldName n
    deriving (Show, Read, Eq, Ord)
instance (IName n, Show i) => IName (NameT i n) where
    make_name = PlainOldName . make_name
instance IName String where
    make_name = id



data UntypedLambdaExpr n
    = Variable n
    | Apply (UntypedLambdaExpr n) (UntypedLambdaExpr n)
    | Abstract n (UntypedLambdaExpr n)
data UntypedLambdaExprWithFV s n
    = UntypedLambdaExprWithFV (s n) (UntypedLambdaExpr n)

instance ILambdaTermBase (UntypedLambdaExpr n) where
    apply = Apply
instance Semigroup (s n) => ILambdaTermBase (UntypedLambdaExprWithFV s n) where
    apply (UntypedLambdaExprWithFV frees1 expr1)
          (UntypedLambdaExprWithFV frees2 expr2)
          = UntypedLambdaExprWithFV (frees1 <> frees2) (apply expr1 expr2)
instance (ILambdaTermBase t, IName n
    , IDynSet (s n), Element (s n) ~ n
    , t ~ UntypedLambdaExprWithFV s n
    ) => ILambdaTerm (UntypedLambdaExprWithFV s n) where
    type Name (UntypedLambdaExprWithFV s n) = n
    type Variables (UntypedLambdaExprWithFV s n) = s n
    variable n = UntypedLambdaExprWithFV (singleton n) (Variable n)
    abstract n (UntypedLambdaExprWithFV frees expr)
        = UntypedLambdaExprWithFV (discard_le1 n frees) (Abstract n expr)
    frees_of (UntypedLambdaExprWithFV frees _) = frees

data UntypedLambdaBuilder (s :: * -> *) c n i o
    = UntypedLambdaBuilder ((c, i) -> (c, o))

instance Category (UntypedLambdaBuilder s c n) where
    id = UntypedLambdaBuilder id
    UntypedLambdaBuilder x2o . UntypedLambdaBuilder i2x
        = UntypedLambdaBuilder (x2o . i2x)
instance Arrow (UntypedLambdaBuilder s c n) where
    arr = UntypedLambdaBuilder . second
    first (UntypedLambdaBuilder ci2co) = UntypedLambdaBuilder c_ix2c_ox where
        c_ix2c_ox = rotateL >>> first ci2co >>> rotateR

instance (Enum c) => IArrowCount (UntypedLambdaBuilder s c n) where
    type ArrowCount (UntypedLambdaBuilder s c n) = c
    tickA = UntypedLambdaBuilder $ \(c,i)->(succ c, c)

instance (Semigroup (s n), n ~ NameT c n'
    ) => ILambdaBuilderBase (UntypedLambdaBuilder s c n') where
    type Term (UntypedLambdaBuilder s c n') = UntypedLambdaExprWithFV s (NameT c n')
instance (Semigroup (s n), Enum c, Show c, IName n'
    , IDynSet (s n), Element (s n) ~ n
    , n ~ NameT c n'
    ) => IUntypedLambdaBuilder (UntypedLambdaBuilder s c n') where


type ArrUntypedLambdaBuilder = UntypedLambdaBuilder Set Integer String

f :: IUntypedLambdaBuilder arr => arr i i -> ()
f _ = ()
a = f (id :: ArrUntypedLambdaBuilder i i)

