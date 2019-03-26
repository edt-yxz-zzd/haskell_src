{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

--  See http://www.haskell.org/haskellwiki/Research_papers/Generics#Scrap_your_boilerplate.21
module TH.TryTransform
    ( module TH.TryTransform
    , module TH.Transform
    )
where
import Data.Typeable
import Data.Data
import Language.Haskell.TH
import Language.Haskell.Syntax
import Data.Proxy
import TH.Transform

isNothing Nothing = True
isNothing _ = False


_d :: Either [Maybe (Either [Int] [Int])] ()
_d = Left [Just (Right []), Just (Left [1,2])]
--main = undefined


class Data a => DataT cxt a where
    direct_transform :: proxy cxt -> a -> a
instance {-# OVERLAPPABLE #-} Data a => DataT cxt a where
    direct_transform _ = id
_direct_transform :: proxy cxt -> (forall a. DataT cxt a => a -> a)
_direct_transform = direct_transform
direct_transform' :: proxy cxt -> (forall a. Data a => a -> a)
direct_transform' proxy = _direct_transform proxy



data Pop1T
instance {-# INCOHERENT #-} Data [a] => DataT Pop1T ([] a) where
    direct_transform _ = f where
        f [] = []
        f (_:ts) = ts

proxy :: Proxy Pop1T
proxy = undefined

gmapT_ex__Pop1 :: Data a => a -> a
gmapT_ex__Pop1 = gmapT (direct_transform proxy)
_d' = gmapT_ex__Pop1 _d
_d'' = everywhere_bottomup gmapT_ex__Pop1 _d
main = do
    print _d'
    print $ _d /= _d'
    print _d''


{-
class IPop1 a where
    pop1 :: a -> a

data Pop1D a = Pop1D {pop1D :: a -> a}
instance IPop1 a => VTable Pop1D a where
    vtable = Pop1D {pop1D = pop1}
instance IPop1 ([] a) where
    pop1 = f where
        f [] = []
        f (_:ts) = ts



_proxy :: Proxy Pop1D
_proxy = undefined
_d' = gmapT_ex direct_transform' _proxy _d



{-
data Pop1 a = Pop1 { runPop1 :: a->a }
instance {-# INCOHERENT #-} {- OVERLAPS #-} VTable Pop1 (Maybe a) where
    --type VTableType Pop1 (Maybe a) = Pop1 (Maybe a)
    vtable = Just . Pop1 $ const Nothing
instance {-# INCOHERENT #-} {- OVERLAPPING #-} VTable Pop1 ([] a) where
    vtable = Just . Pop1 $ f where
        f [] = []
        f (_:ts) = ts


instance (Data a, VTable Pop1 a) => DataGMapT Pop1 a where
    direct_transform proxy a = case get_vtable proxy of
        Nothing -> a
        Just pop1 -> runPop1 pop1 a

_proxy :: Proxy Pop1
_proxy = get_cxt . Pop1 $ const _d
_d' = gmapT_ex direct_transform' _proxy _d


main = do
    print _d'
    print $ _d /= _d'
    print $ isNothing (vtable :: Maybe (Pop1 [a]))
--}
--}
--}
--}
