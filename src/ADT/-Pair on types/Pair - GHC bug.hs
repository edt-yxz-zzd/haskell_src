{-
BUG??????????
ghc.exe: panic! (the 'impossible' happened)
  (GHC version 8.0.2 for x86_64-unknown-mingw32):
        kindPrimRep.go *

Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ConstraintKinds #-}
-- :set -freduction-depth=0
module ADT.Pair
where
import qualified Prelude as P
import Prelude hiding (fst, snd)
import Seed.Boxed
import Data.Proxy
import Language.Haskell.TH.Syntax (Lift)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Kind (type (*))
import GHC.Types (type (~~))


type family ToPair (t :: con) (f :: fst) (s :: snd) :: ktfs

--type family SwapK (a :: ka) :: *
class (ISwap (Swap a), Swap (Swap a) ~~ a) => ISwap (a :: ka) where
    --type Swap a :: SwapK a
    type Swap a :: ksa
class (ISwapT (SwapT a), SwapT (SwapT a) ~~ a) => ISwapT (a :: ka) where
    type SwapT a :: ksta

type ToPairConstraint (a :: ka) = ToPair (PairT a) (Fst a) (Snd a) ~~ a

type SwapConstraint a = Swap (Swap a) ~~ a
type SwapTConstraint a = SwapT (SwapT (PairT a)) ~~ PairT a
type FstSwapConstraint a = Fst a ~~ Snd (Swap a)
type SndSwapConstraint a = Snd a ~~ Fst (Swap a)
class (IPair (Swap a), ISwap a, ISwapT (PairT a)
    , Fst a ~~ Snd (Swap a), Snd a ~~ Fst (Swap a)
    , ToPair (PairT a) (Fst a) (Snd a) ~~ a
    , SwapT (PairT a) ~~ PairT (Swap a)
    ) => IPair (a :: ka) where
    type Fst a :: kfa
    type Snd a :: kfs
    type PairT a :: kpta -- sth like constructor

--class (ISwap a, IPair a, IPair (Swap a), Pair (Swap a)) => Pair a where
-- class (IPair a, Pair (Swap a)) => Pair a where
class (IPair a, Pair (Swap a)) => Pair (a :: *) where
    fst :: a -> Fst a
    fst = P.fst . to_pair
    snd :: a -> Snd a
    snd = P.snd . to_pair
    to_pair :: a -> (Fst a, Snd a)
    to_pair a = (fst a, snd a)
    from_pair :: (Fst a, Snd a) -> a
    from_pair (a,b) = mkPair a b
    mkPair :: Fst a -> Snd a -> a
    mkPair a b = from_pair (a, b)

    swap :: a -> Swap a
    swap p = let (a, b) = to_pair p in mkPair b a
    --swap :: (Fst (Swap a) ~~ Snd a, Snd (Swap a) ~~ Fst a, Pair (Swap a)) => a -> Swap a
    --swap = _swap

    {-# MINIMAL ((fst, snd) | to_pair), (from_pair | mkPair) #-}

{-
_swap
    :: (Fst (Swap a) ~~ Snd a, Snd (Swap a) ~~ Fst a, Pair a, Pair (Swap a))
    => a -> Swap a
_swap p = let (a, b) = to_pair p in mkPair b a


{-
map_pair :: (a->a', b->b') -> (a, b) -> (a', b')
map_pair (f, g) (a, b) = (f a, g b)
mapPair
    ::  ( Pair f, Pair a, Pair b
        , Fst f ~ (Fst a -> Fst b), Snd f ~ (Snd a -> Snd b)
        )
    => f -> a -> b
mapPair f a = from_pair $ map_pair (to_pair f) (to_pair a)
mapPair_
    ::  ( Pair a, Pair f
        , Fst f ~ (Fst a -> b), Snd f ~ (Snd a -> c)
        )
    => f -> a -> (b, c)
mapPair_ = mapPair















-- IPair
data a :**: b = ProductPair
type role (:*:) representational representational
data a :*: b = a :*: b
    deriving (Eq, Ord, Read, Show, Functor, Typeable, Data, Generic, Lift)
infixr 6 :*:, :**:
to_proxy_pairP :: IPair a => proxy a -> (Proxy (Fst a), Proxy (Snd a))
to_proxy_pairP _ = (Proxy, Proxy)







{-
type instance ToPair (Proxy (,)) a b = (a, b)
instance ISwap (a, b) where
    type Swap (a, b) = (b, a)
instance ISwapT (Proxy (,)) where
    type SwapT (Proxy (,)) = Proxy (,)
instance IPair (a,b) where
    type Fst (a, b) = a
    type Snd (a, b) = b
    type PairT (a, b) = Proxy (,)
instance Pair (a,b) where
    fst = fst
    snd = snd
    to_pair = id
    from_pair = id
    mkPair = (,)
    swap (a, b) = (b, a)

-------------------------

type instance ToPair ((:**:)) a b = (a :**: b)
instance ISwap (a :**: b) where
    type Swap (a :**: b) = (b :**: a)
instance ISwapT ((:**:)) where
    type SwapT ((:**:)) = (:**:)
instance IPair (a :**: b) where
    type Fst (a :**: b) = a
    type Snd (a :**: b) = b
    type PairT (a :**: b) = (:**:)

---------------------------


type instance ToPair (Proxy (:*:)) a b = (a :*: b)
instance ISwap (a :*: b) where
    type Swap (a :*: b) = (b :*: a)
instance ISwapT (Proxy (:*:)) where
    type SwapT (Proxy (:*:)) = Proxy (:*:)
instance IPair (a :*: b) where
    type Fst (a :*: b) = a
    type Snd (a :*: b) = b
    type PairT (a :*: b) = Proxy (:*:)
instance Pair (a :*: b) where
    fst (a :*: b) = a
    snd (a :*: b) = b
    to_pair (a :*: b) = (a, b)
    from_pair (a, b) = a :*: b
    mkPair = (:*:)
    swap (a :*: b) = (b :*: a)







-------------------------
type instance ToPair (Label pa pb) (Label fa fb) (Label sa sb)
    = Label (ToPair pa fa sa) (ToPair pb fb sb)
instance (ISwap a, ISwap b) => ISwap (Label a b) where
    type Swap (Label a b) = Label (Swap a) (Swap b)
instance (ISwapT a, ISwapT b) => ISwapT (Label a b) where
    type SwapT (Label a b) = Label (SwapT a) (SwapT b)
instance (IPair a, IPair b, ToPairConstraint (Label a b)
    ) => IPair (Label a b) where
    type Fst (Label a b) = Label (Fst a) (Fst b)
    type Snd (Label a b) = Label (Snd a) (Snd b)
    type PairT (Label a b) = Label (PairT a) (PairT b)

instance (Pair a, Pair b, IPair (Label a b)) => Pair (Label a b) where
    fst = mapGBox fst
    snd = mapGBox snd
    to_pair p = (box a, box b)
        where
            (a, b) = unbox $ fmap to_pair p
    swap = mapGBox swap
    from_pair (la, lb) = box $ from_pair (unbox la, unbox lb)




{-
class PairF p where
    fst :: p a b -> a
    snd :: p a b -> b
    swap :: p a b -> p b a
    to_pair :: p a b -> (a, b)
    from_pair :: (a, b) -> p a b
-}
--}
--}
--}
--}
