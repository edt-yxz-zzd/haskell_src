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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}


module ADT.Pair
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

type family ToPairP (con :: *) (fst :: *) (snd :: *) = (r :: *)
    --ToPairP (Proxy con) (Proxy fst) (Proxy snd) = Proxy (ToPair con fst snd)

{-
type family IsProxy (x :: *) = (r :: Constraint) where
    IsProxy (Proxy a) = Int ~ Int
    IsProxy a = Int ~ Char
-}

type family ToAny (a :: ka) = (r :: kr) where
    ToAny a = a
type family CastProxy (a :: *) = (r :: *) where
    CastProxy (Proxy _) = ()
    CastProxy _ = Int
-- Swap (Swap a) ~~ a
-- SwapT (SwapT ca) ~~ ca
--      ca -- pair constructor of a
type SwapConstraintP (pa :: *) = FMapSwap (FMapSwap pa) ~~ pa
type SwapTConstraintP (pca :: *) = FMapSwapT (FMapSwapT pca) ~~ pca
--type IsProxy (a :: *) = Proxy (ProxyArg a) ~~ a
--type IsProxy (a :: *) = Proxy _x ~~ a
--type IsProxy (a :: *) = Proxy _ ~~ a
--type IsProxy (a :: *) = Proxy (MayProxyArg a) ~~ a
type IsProxy (a :: *) = Proxy (TrueProxyArg a) ~~ a
--type IsProxy (a :: *) = ToAny (Proxy (TrueProxyArg a)) ~~ ToAny a
--type IsProxy (a :: *) = () ~~ CastProxy a
{-
class (pa ~ Proxy (TrueProxyArg pa)) => IsProxy (pa :: *) where
    type TrueProxyArg pa :: k
instance IsProxy (Proxy a) where
    type TrueProxyArg (Proxy a) = a
-}
{-
class (pa ~ Proxy a) => IsProxyEx (a :: k) (pa :: *) | pa -> a where
instance IsProxyEx a (Proxy a) where
class IsProxyEx a pa => IsProxy pa
instance IsProxyEx a pa => IsProxy pa
-}

class (IsProxy (FMapSwap _Proxy_a), SwapConstraintP _Proxy_a, IsProxy _Proxy_a)
    => ISwapP (_Proxy_a :: *) where
    type FMapSwap _Proxy_a :: * -- Proxy (Swap a)
class (IsProxy (FMapSwapT _Proxy_ca), SwapTConstraintP _Proxy_ca
        , IsProxy _Proxy_ca)
    => ISwapTP (_Proxy_ca :: *) where
    type FMapSwapT _Proxy_ca :: * -- Proxy (SwapT ca)


class (IPairP (Proxy a), IPairP (FMapSwap (Proxy a))) => IPair (a :: ka)
instance (IPairP (Proxy a), IPairP (FMapSwap (Proxy a))) => IPair a
class (ISwapP pa, ISwapTP (FMapPairT pa)
    , SwapTConstraintP (FMapPairT pa)
    , SwapConstraintP pa
    , FMapFst pa ~~ FMapSnd (FMapSwap pa), FMapSnd pa ~~ FMapFst (FMapSwap pa)
    , ToPairP (FMapPairT pa) (FMapFst pa) (FMapSnd pa) ~~ pa
    , FMapSwapT (FMapPairT pa) ~~ FMapPairT (FMapSwap pa)
    , IsProxy (FMapFst pa), IsProxy (FMapSnd pa), IsProxy (FMapPairT pa)
    , IsProxy pa
    ) => IPairP (pa :: *) where
    type FMapFst pa :: *
    type FMapSnd pa :: *
    type FMapPairT pa :: * -- sth like constructor

    type FMapFst pa = Proxy (Fst (ProxyArg pa))
    type FMapSnd pa = Proxy (Snd (ProxyArg pa))
    type FMapPairT pa = Proxy (PairT (ProxyArg pa))
    {-
    type FMapFst pa = Proxy (Fst (Last1 pa))
    type FMapSnd pa = Proxy (Snd (Last1 pa))
    type FMapPairT pa = Proxy (PairT (Last1 pa))
    -}


class (PairBase a, PairBase (Swap a)) => Pair a where
instance (PairBase a, PairBase (Swap a)) => Pair a where

type family MayProxyArg (x :: *) = (r :: *) where
    MayProxyArg (Proxy (a :: *)) = a
    MayProxyArg a = ()
type family ProxyArg (x :: *) = (r :: *) where
    ProxyArg (Proxy a) = a
type family TrueProxyArg (x :: *) where
    TrueProxyArg (Proxy a) = a

class (IPairP (Proxy a)
    , Swap (Swap a) ~~ a
    , Proxy (Fst a) ~~ FMapFst (Proxy a)
    , Proxy (Snd a) ~~ FMapSnd (Proxy a)
    , Proxy (PairT a) ~~ FMapPairT (Proxy a)
    , Proxy (Swap a) ~~ FMapSwap (Proxy a)
    )
    => PairBase (a :: *) where
    type Fst a :: *
    type Snd a :: *
    type PairT a :: * -- sth like constructor
    type Swap a :: *
    type Fst a = ProxyArg (FMapFst (Proxy a))
    type Snd a = ProxyArg (FMapSnd (Proxy a))
    type PairT a = ProxyArg (FMapPairT (Proxy a))
    type Swap a = ProxyArg (FMapSwap (Proxy a))
    {-
    type Fst a = Last1 (FMapFst a)
    type Snd a = Last1 (FMapSnd a)
    type PairT a = (FMapPairT a)
    type Swap a = Last1 (FMapSwap a)
    -}

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

    swap :: Pair a => a -> Swap a
    swap p = let (a, b) = to_pair p in mkPair b a

    {-# MINIMAL ((fst, snd) | to_pair), (from_pair | mkPair) #-}

type FstP (a :: ka) = FMapFst (Proxy a) -- Proxy (Fst a)
type SndP (a :: ka) = FMapSnd (Proxy a) -- Proxy (Snd a)
type PairTP a = FMapPairT (Proxy a) -- Proxy (PairT a)
type SwapP a = FMapSwap (Proxy a) -- Proxy (Swap a)










type ProxyPair a b = Proxy (a, b)
type ProxyPairT = Proxy (Proxy (,))
instance ISwapP (ProxyPair a b) where
    type FMapSwap (ProxyPair a b) = ProxyPair b a
instance ISwapTP ProxyPairT where
    type FMapSwapT ProxyPairT = ProxyPairT
type instance ToPairP ProxyPairT (Proxy a) (Proxy b) = ProxyPair a b
instance IPairP (ProxyPair a b) where
    type FMapFst (ProxyPair a b) = Proxy a
    type FMapSnd (ProxyPair a b) = Proxy b
    type FMapPairT (ProxyPair a b) = ProxyPairT
instance PairBase (a, b) where
    {-
    type Fst (a, b) = a
    type Snd (a, b) = b
    type PairT (a, b) = Proxy (,)
    type Swap (a, b) = (b, a)
    -}
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
    ::  ( Pair f, Pair a, Pair b
        , Fst f ~~ (Fst a -> Fst b), Snd f ~~ (Snd a -> Snd b)
        )
    => f -> a -> b
mapPair f a = from_pair $ map_pair (to_pair f) (to_pair a)
mapPair_
    ::  ( Pair a, Pair f
        , Fst f ~~ (Fst a -> b), Snd f ~~ (Snd a -> c)
        )
    => f -> a -> (b, c)
mapPair_ = mapPair







-- IPair
data a :**: b = ProductPair
type role (:*:) representational representational
data a :*: b = a :*: b
    deriving (Eq, Ord, Read, Show, Functor, Typeable, Data, Generic, Lift)
infixr 6 :*:, :**:
--to_proxy_pairP :: IPair a => proxy a -> (FstP a, SndP a)
to_proxy_pairP :: (IPair a, pa ~ Proxy a) => proxy a -> (FMapFst pa, FMapSnd pa)
to_proxy_pairP _ = (Proxy, Proxy)














type ProxyProductPair (a :: ka) (b :: kb) = Proxy (a :**: b)
instance ISwapP (ProxyProductPair a b) where
    type FMapSwap (ProxyProductPair a b) = ProxyProductPair b a
data ProductPairT
type ProxyProductPairT = Proxy ProductPairT
instance ISwapTP ProxyProductPairT where
    type FMapSwapT ProxyProductPairT = ProxyProductPairT
{- ? Illegal type synonym family application in instance:
 - why???????
type ProxyProductPairT = Proxy (Proxy (:**:))
instance ISwapTP ProxyProductPairT where
    type FMapSwapT ProxyProductPairT = ProxyProductPairT
type ProxyProductPairT = Proxy (Proxy (:**:))
instance ISwapTP (Proxy (Proxy (:**:))) where
    type FMapSwapT (Proxy (Proxy (:**:))) = (Proxy (Proxy (:**:)))
-}



type instance ToPairP ProxyProductPairT (Proxy a) (Proxy b)
    = ProxyProductPair a b
instance IPairP (ProxyProductPair a b) where
    type FMapFst (ProxyProductPair a b) = Proxy a
    type FMapSnd (ProxyProductPair a b) = Proxy b
    type FMapPairT (ProxyProductPair a b) = ProxyProductPairT
{-
instance PairBase (ProxyProductPair a b) where
    type Fst (ProxyProductPair a b) = a
    type Snd (ProxyProductPair a b) = b
    type PairT (ProxyProductPair a b) = ProxyProductPairT
    type Swap (ProxyProductPair a b) = ProxyProductPair b a
-}

{-
instance (IPair (b :**: a), Fst_ (a :**: b) ~~ a, Snd_ (a :**: b) ~~ b) => IPair (a :**: b) where
    type Fst_ (a :**: b) = a
    type Snd_ (a :**: b) = b
    type PairT_ (a :**: b) = ProductPairT

---------------------------

type UProductPairT = (Proxy (:*:))
type instance ToPair UProductPairT a b = (a :*: b)
instance ISwap (b :*: a) => ISwap (a :*: b) where
    type Swap (a :*: b) = (b :*: a)
instance ISwapT UProductPairT => ISwapT UProductPairT where
    type SwapT UProductPairT = UProductPairT
instance IPair (b :*: a) => IPair (a :*: b) where
    type Fst_ (a :*: b) = a
    type Snd_ (a :*: b) = b
    type PairT_ (a :*: b) = UProductPairT
instance Pair (b :*: a) => Pair (a :*: b) where
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
    type Fst_ (Label a b) = Label (Fst_ a) (Fst_ b)
    type Snd_ (Label a b) = Label (Snd_ a) (Snd_ b)
    type PairT_ (Label a b) = Label (PairT_ a) (PairT_ b)

instance (Pair a, Pair b, IPair (Label a b)) => Pair (Label a b) where
    fst = mapGBox fst
    snd = mapGBox snd
    to_pair p = (box a, box b)
        where
            (a, b) = unbox $ fmap to_pair p
    swap = mapGBox swap
    from_pair (la, lb) = box $ from_pair (unbox la, unbox lb)
















{-

type GetKind (a :: ka) = ka
--type family ToPair (t :: con) (f :: fst) (s :: snd) = (r :: ktfs)

--type family SwapK (a :: ka) :: kska
--class (ISwap (Swap a), Swap (Swap a) ~~ a) => ISwap (a :: ka) where
{-
class (ISwap (Swap a), Swap (Swap a) ~~ a
    , GetKind (Swap a) ~~ SwapK a
    , GetKind (Swap a) ~ SwapK a
    ) => ISwap (a :: ka) where
  -}
{-
--type Swap a = Last1 (SwapP a)
type Swap2 (a :: ka) = FMapSwap (SwapP a)
type SwapT2 (a :: ka) = FMapSwapT (SwapTP a)
type SwapConstraint (a :: ka) = Swap2 a ~~ Proxy a
type SwapTConstraint (a :: ka) = SwapT2 a ~~ Proxy a
type family FMapSwap (x :: *) where
    FMapSwap (Proxy a) = SwapP a
type family FMapSwapT (x :: *) where
    FMapSwapT (Proxy a) = SwapTP a
type family ProxyArg (x :: *) where
    ProxyArg (Proxy a) = a
-}
{-
class (IsProxy (SwapP a), SwapConstraint a) => ISwap (a :: ka) where
    type SwapP a :: * -- Proxy (Swap a)
class (IsProxy (SwapTP a), SwapTConstraint a) => ISwapT (a :: ka) where
    type SwapTP a :: * -- Proxy (SwapT a)
-}

--class (ISwapT (ProxyArg a), IsProxy a) => ISwapTP (a :: ka) where
--instance (ISwapT x, a ~ (Proxy x)) => ISwapTP x where
--class (ISwap (Swap a), Swap (Swap a) ~~ a) => ISwap (a :: ka) where
    --type Swap a :: ksa
    -- type Swap a :: SwapK a
    -- *** Exception: thread blocked indefinitely in an MVar operation
--class (ISwapT (SwapT a), SwapT (SwapT a) ~~ a) => ISwapT (a :: ka) where
--    type SwapT a :: ksta
--type SwapT a = Last1 (SwapTP a)





{--
type ToPairConstraint (a :: ka) = ToPair (PairT_ a) (Fst_ a) (Snd_ a) ~~ a

--type SwapConstraint a = Swap (Swap a) ~~ a
type SwapTConstraint a = SwapT (SwapT (PairT_ a)) ~~ PairT_ a
type FstSwapConstraint a = Fst_ a ~~ Snd_ (Swap a)
type SndSwapConstraint a = Snd_ a ~~ Fst_ (Swap a)
type Fst_ a = Last1 (FstP a)
type Snd_ a = Last1 (SndP a)
type PairT_ a = Last1 (PairTP a)
-}

{-
type family FMapFst (x :: *) where
    FMapFst (Proxy a) = FstP a
type family FMapSnd (x :: *) where
    FMapSnd (Proxy a) = SndP a
type family FMapPairT (x :: *) where
    FMapPairT (Proxy a) = PairTP a
-}

{-
class (ISwap a, ISwapT (PairT_ a)
    , Fst_ a ~~ Snd_ (Swap a), Snd_ a ~~ Fst_ (Swap a)
    , ToPair (PairT_ a) (Fst_ a) (Snd_ a) ~~ a
    , SwapT (PairT_ a) ~~ PairT_ (Swap a)
    ) => IPairBase (a :: ka) where
    type Fst_ a :: kfa
    type Snd_ a :: kfs
    type PairT_ a :: kpta -- sth like constructor
-}
    {-
    type FstK a :: kf
    type SndK a :: ks
    type PairTK a :: kp
    type FstK a = GetKind (Fst_ a)
    type SndK a = GetKind (Snd_ a)
    type PairTK a = GetKind (PairT_ a)
    -}

{-
type family FstK a :: kf
type family SndK a :: ks
type family PairTK a :: kp
type instance FstK a = GetKind (Fst_ a)
type instance SndK a = GetKind (Snd_ a)
type instance PairTK a = GetKind (PairT_ a)
-}



{--
class (PairBase a, PairBase (Swap a)) => Pair a where
instance (PairBase a, PairBase (Swap a)) => Pair a where
--class (ISwap a, IPair a, IPair (Swap a), Pair (Swap a)) => Pair a where
-- class (IPair a, Pair (Swap a)) => Pair a where
class (IPair a
    , Proxy (Fst a) ~~ FstP a, Proxy (Snd a) ~~ SndP a
    , Proxy (PairT a) ~~ PairTP a, Proxy (Swap a) ~~ SwapP a
    , Swap (Swap a) ~~ a
    --, Fst a ~ Fst_ a, Snd a ~ Snd_ a, PairT a ~ PairT_ a, Swap (Swap a) ~ a
    )
    => PairBase (a :: *) where
    type Fst a :: *
    type Snd a :: *
    type PairT a :: * -- sth like constructor
    type Swap a :: *
    type Fst a = Last1 (FstP a)
    type Snd a = Last1 (SndP a)
    type PairT a = Last1 (PairTP a)
    type Swap a = Last1 (SwapP a)

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

    swap :: Pair a => a -> Swap a
    swap p = let (a, b) = to_pair p in mkPair b a

    {-# MINIMAL ((fst, snd) | to_pair), (from_pair | mkPair) #-}

{-
_swap
    :: (Fst (Swap a) ~~ Snd a, Snd (Swap a) ~~ Fst a, Pair a, Pair (Swap a))
    => a -> Swap a
_swap p = let (a, b) = to_pair p in mkPair b a
-}


















_p = to_pair ('a', "a")
type ProxyPairT = Proxy (,)
--type instance ToPair ProxyPairT a b = (a, b)
--type instance SwapK (a, b) = 1
type instance ToPairP ProxyPairT (Proxy a) (Proxy b) = Proxy (a, b)
instance ISwap (a, b) where
    --type Swap (a, b) = (b, a)
    type SwapP (a, b) = Proxy (b, a)
instance ISwapT ProxyPairT where
    --type SwapT ProxyPairT = ProxyPairT
    type SwapTP ProxyPairT = Proxy ProxyPairT
instance IPairBase (a, b) where
    {-
    type Fst_ (a, b) = a
    type Snd_ (a, b) = b
    type PairT_ (a, b) = ProxyPairT
    -}
    type FstP (a, b) = Proxy a
    type SndP (a, b) = Proxy b
    type PairTP (a, b) = Proxy ProxyPairT
instance PairBase (a, b) where
    fst = P.fst
    snd = P.snd
    to_pair = id
    from_pair = id
    mkPair = (,)
    swap (a, b) = (b, a)

{------------------------
--type ProductPairT = Proxy (:**:)
data ProductPairT
type instance ToPair ProductPairT a b = (a :**: b)
--type instance ToPair (Proxy (:**:)) a b = (a :**: b)
instance ISwap (b :**: a) => ISwap (a :**: b) where
    type Swap (a :**: b) = (b :**: a)
instance ISwapT ProductPairT => ISwapT ProductPairT where
    type SwapT ProductPairT = ProductPairT
{-
instance ISwapT (Proxy (:**:)) => ISwapT (Proxy (:**:)) where
    type SwapT (Proxy (:**:)) = (Proxy (:**:))
instance ISwapT (ProductPairT :: k2 -> k1 -> k2 :**: k1) => ISwapT (ProductPairT :: k1 -> k2 -> k1 :**: k2) where
    type SwapT (ProductPairT :: k1 -> k2 -> k1 :**: k2) = (ProductPairT :: (k2 -> k1 -> k2 :**: k1))
-}
--instance IPair (b :**: a) => IPair (a :**: b) where
instance (IPair (b :**: a), Fst_ (a :**: b) ~~ a, Snd_ (a :**: b) ~~ b) => IPair (a :**: b) where
    type Fst_ (a :**: b) = a
    type Snd_ (a :**: b) = b
    type PairT_ (a :**: b) = ProductPairT

---------------------------

type UProductPairT = (Proxy (:*:))
type instance ToPair UProductPairT a b = (a :*: b)
instance ISwap (b :*: a) => ISwap (a :*: b) where
    type Swap (a :*: b) = (b :*: a)
instance ISwapT UProductPairT => ISwapT UProductPairT where
    type SwapT UProductPairT = UProductPairT
instance IPair (b :*: a) => IPair (a :*: b) where
    type Fst_ (a :*: b) = a
    type Snd_ (a :*: b) = b
    type PairT_ (a :*: b) = UProductPairT
instance Pair (b :*: a) => Pair (a :*: b) where
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
    type Fst_ (Label a b) = Label (Fst_ a) (Fst_ b)
    type Snd_ (Label a b) = Label (Snd_ a) (Snd_ b)
    type PairT_ (Label a b) = Label (PairT_ a) (PairT_ b)

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
