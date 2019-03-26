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
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}

module Seed.ProxyOps
    ( module Seed.ProxyOps
    , module Data.Proxy
    )
where
import Data.Proxy
toProxy :: a -> Proxy a
toProxy _ = Proxy



type family Last1 (x :: kx) where
    Last1 (f (a :: ka) :: kfa) = a
type family Func1 (x :: kx) where
    Func1 (f (a :: ka) :: kfa) = f
type family Last1P x where
    Last1P (proxy (f (a :: k) :: kfa) :: kpfa) = Proxy a
type family Func1P x where
    Func1P (proxy (f (a :: k) :: kfa) :: kpfa) = Proxy f
type Last2 a = Last1 (Func1 a)
type Func2 a = Func1 (Func1 a)
type Last2P a = Last1P (Func1P a)
type Func2P a = Func1P (Func1P a)


{-
type family Last2 x where
    Last2 (f (b :: kb) (a :: ka) :: kfba) = b
type family Last2P x where
    Last2P (proxy (f (b :: kb) (a :: ka) :: kfba) :: kpfba) = Proxy b
-}


appP, (^$) :: proxy f -> proxy' a -> Proxy (f (a :: ka) :: kfa)
appP _ _ = Proxy
(^$) = appP
infixl 9 ^$ -- left
last1P :: proxy (f (a :: ka) :: kfa) -> Proxy a
last1P _ = Proxy
func1P :: proxy (f (a :: ka) :: kfa) -> Proxy f
func1P _ = Proxy
-- last2P :: proxy (f b (a :: ka)) -> Proxy (b :: kb)
--last2P _ = Proxy
last2P = last1P . func1P
func2P = func1P . func1P

fstP :: proxy '(a, b) -> Proxy (a :: ka)
fstP _ = Proxy
sndP :: proxy '(a, b) -> Proxy (b :: kb)
sndP _ = Proxy


withBy :: ((?by :: proxy by)=> a) -> (proxy by -> a)
withBy a by = let ?by = by in a
withBy_ :: (?by :: proxy by) => (proxy by -> a) -> a
withBy_ f = f ?by




