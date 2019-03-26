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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}


module ADT.IPair
where
import qualified Prelude as P
import Prelude hiding (fst, snd)
import Seed.Boxed
import Data.Proxy
import Language.Haskell.TH.Syntax (Lift)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Kind (type (*), Constraint)
import GHC.Types (type (~~), Symbol)
import Seed.ProxyOps

type SwapConstraint a = Swap (Swap a) ~ a
type SwapTConstraint ca = SwapT (SwapT ca) ~ ca
type ToPairConstraint a = ToPair (PairT a) (Fst a) (Snd a) ~ a


class (SwapConstraint a) => ISwap a where
    type Swap a
class (SwapTConstraint ca) => ISwapT ca where
    type SwapT ca


type family ToPair _PairT_a _Fst_a _Snd_a -- = a
class (ISwap a, ISwapT (PairT a)
    , SwapTConstraint (PairT a)
    , SwapConstraint a
    , Fst a ~ Snd (Swap a), Snd a ~ Fst (Swap a)
    , ToPair (PairT a) (Fst a) (Snd a) ~ a
    , SwapT (PairT a) ~ PairT (Swap a)
    ) => IPairBase a where
    type Fst a :: *
    type Snd a :: *
    type PairT a :: * -- sth like constructor


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

    swap :: IPair a => a -> Swap a
    swap p = let (a, b) = to_pair p in mkPair b a

    {-# MINIMAL ((fst, snd) | to_pair), (from_pair | mkPair) #-}

class (IPairBase a, IPairBase (Swap a)) => IPair a where
instance (IPairBase a, IPairBase (Swap a)) => IPair a where
type FstP a = Proxy (Fst a)
type SndP a = Proxy (Snd a)
type PairTP a = Proxy (PairT a)
type SwapP a = Proxy (Swap a)

to_proxy_pairP :: IPair a => proxy a -> (FstP a, SndP a)
to_proxy_pairP _ = (Proxy, Proxy)









type ProxyPair a b = (a, b)
type ProxyPairT = (Proxy (,))
instance ISwap (ProxyPair a b) where
    type Swap (ProxyPair a b) = ProxyPair b a
instance ISwapT ProxyPairT where
    type SwapT ProxyPairT = ProxyPairT
type instance ToPair ProxyPairT (a) (b) = ProxyPair a b

instance IPairBase (a, b) where
    type Fst (a, b) = a
    type Snd (a, b) = b
    type PairT (a, b) = Proxy (,)
    fst = P.fst
    snd = P.snd
    to_pair = id
    from_pair = id
    mkPair = (,)
    swap (a, b) = (b, a)
_p = to_pair ('a', "a")


map_pair :: (a->a', b->b') -> (a, b) -> (a', b')
map_pair (f, g) (a, b) = (f a, g b)
mapPair
    ::  ( IPair f, IPair a, IPair b
        , Fst f ~ (Fst a -> Fst b), Snd f ~ (Snd a -> Snd b)
        )
    => f -> a -> b
mapPair f a = from_pair $ map_pair (to_pair f) (to_pair a)
mapPair_
    ::  ( IPair a, IPair f
        , Fst f ~ (Fst a -> b), Snd f ~ (Snd a -> c)
        )
    => f -> a -> (b, c)
mapPair_ = mapPair







-- IPair
--data a :**: b = ProductPair
--infixr 6 :*:, :**:
--type GetKind (a :: ka) = ka
type role (:*:) representational representational
data a :*: b = a :*: b
    deriving (Eq, Ord, Read, Show, Functor, Typeable, Data, Generic, Lift)
infixr 6 :*:













---------------------------

type UProductPairT = (Proxy (:*:))
type instance ToPair UProductPairT a b = (a :*: b)
instance ISwap (b :*: a) => ISwap (a :*: b) where
    type Swap (a :*: b) = (b :*: a)
instance ISwapT UProductPairT => ISwapT UProductPairT where
    type SwapT UProductPairT = UProductPairT
instance IPairBase (a :*: b) where
    type Fst (a :*: b) = a
    type Snd (a :*: b) = b
    type PairT (a :*: b) = UProductPairT
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
instance (IPairBase a, IPairBase b, ToPairConstraint (Label a b)
    ) => IPairBase (Label a b) where
    type Fst (Label a b) = Label (Fst a) (Fst b)
    type Snd (Label a b) = Label (Snd a) (Snd b)
    type PairT (Label a b) = Label (PairT a) (PairT b)

    fst = mapGBox fst
    snd = mapGBox snd
    to_pair p = (box a, box b)
        where
            (a, b) = to_pair $ unbox p
    from_pair (la, lb) = box $ from_pair (unbox la, unbox lb)





















