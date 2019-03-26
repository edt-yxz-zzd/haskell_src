{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}


-- used in CFGinType, for generate sentence from gramma
module Seed.FoldAlt
where

import Control.Arrow

unit_alt :: Arrow arr => arr (a, ()) a
unit_alt = arr fst
class FoldAlt e a where
    fold_altA :: Arrow arr => arr a e
    fold_altA = arr fold_alt
    fold_alt :: a -> e
instance FoldAlt e e where
    fold_alt = id
instance (Monoid e, FoldAlt e b, FoldAlt e a) => FoldAlt e (a, b) where
    fold_alt (a, b) = fold_alt a `mappend` fold_alt b
instance Monoid e => FoldAlt e () where
    fold_alt _ = mempty
instance Monoid e => FoldAlt e [e] where
    fold_alt = foldr mappend mempty

class Linked2List e a where
    linked2list :: a -> [e]
    linked2listA :: Arrow arr => arr a [e]
    linked2listA = arr linked2list
instance Linked2List e () where
    linked2list _ = []
instance Linked2List e e where
    linked2list e = [e]
instance Linked2List e [e] where
    linked2list = id
instance (Linked2List e a, Linked2List e b) => Linked2List e (a,b) where
    linked2list (a,b) = linked2list a ++ linked2list b




