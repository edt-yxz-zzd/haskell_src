

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}


{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Seed.Kind
    ( module Seed.Kind
    , module Data.Kind
    , module Data.Proxy
    )
where

import Data.Proxy
import Data.Kind
import GHC.TypeLits hiding (type (*))
import Seed.Boxed



type family PairFst (p :: (a, b)) = (r :: a) where
    PairFst '(a, b) = a
type family PairSnd (p :: (a, b)) = (r :: b) where
    PairSnd '(a, b) = b
type family ListAdd (lhs :: [a]) (rhs :: [a]) = (r :: [a]) where
    ListAdd '[] rhs = rhs
    ListAdd (h ': ts) rhs = h ': ListAdd ts rhs
type family List2Linked (ls :: [*]) = (r :: *) | r -> ls where
    List2Linked '[] = ()
    List2Linked (h ': ts) = (h, List2Linked ts)
type EitherX a = Either a a



-------- Dict
data lhs := right_part = lhs := [right_part]
type lhs ::= right_part = EitherX lhs := EitherX right_part
data symbol :> result = symbol :> result
type symbol ::> result = EitherX symbol :> result
infix 2 :=, ::=
infixr 9 :>, ::>
type Dict key val = [key :> val]
type family LookupE (k :: key) (dict :: Dict key val) = (r :: val) where
    --LookupE k '[] = error
    LookupE k (k ':> r ': ts) = r
    LookupE k (a ':> r ': ts) = LookupE k ts
    LookupE k '[] = TypeError
        (Text "LookupE fail, cannot find key:" :<>: ShowType k)
type family LookupLsE (ks :: [key]) (dict :: Dict key val) = (r :: [val]) where
    LookupLsE '[] d = '[]
    LookupLsE (k ': ks) d = LookupE k d ': LookupLsE ks d
{-
type family LookupLs2Linked (ks :: [key]) (dict :: Dict key *) = (r :: *) where
    LookupLs2Linked ks dict = List2Linked (LookupLsE ks dict)
-}
type LookupLs2Linked ks dict = List2Linked (LookupLsE ks dict)
type family RemoveFirstItem (k :: key) (d :: Dict key val) = (r :: Dict key val) where
    -- RemoveItem k '[] = error
    RemoveFirstItem k (k ':> v ': ts) = ts
    RemoveFirstItem k (x ':> v ': ts) = RemoveFirstItem k ts
    RemoveFirstItem k '[] = TypeError
        (Text "RemoveFirstItem fail, cannot find key:" :<>: ShowType k)





type IsNotInDict k d = Not (IsInDict k d)
type IsNotInList k d = Not (IsInList k d)
type IsInDict k d = IsInList k (Dict2Keys d)

type family IsInList (e :: a) (ls :: [a]) = (r :: Bool) where
    IsInList k '[] = 'False
    IsInList k (k ': ts) = 'True
    IsInList k (a ': ts) = IsInList k ts
type family HasNoDuplicateElem (ls :: [a]) = (r :: Bool) where
    HasNoDuplicateElem '[] = 'True
    HasNoDuplicateElem (h ': ts) =
        And (IsNotInList h ts) (HasNoDuplicateElem ts)
type HasNoDuplicateKey d = HasNoDuplicateElem (Dict2Keys d)
type family Dict2Keys (dict :: Dict k v) = (r :: [k]) where
    Dict2Keys '[] = '[]
    Dict2Keys (k ':> v ': ts) = k ': Dict2Keys ts
type family Dict2Vals (dict :: Dict k v) = (r :: [v]) where
    Dict2Vals '[] = '[]
    Dict2Vals (k ':> v ': ts) = v ': Dict2Vals ts
type Dict2LinkedVals d = List2Linked (Dict2Vals d)
type family Assert (b :: Bool) = (r :: Constraint) where
    Assert 'True = Int ~ Int
    Assert 'False = Int ~ Char

class Assert (HasNoDuplicateKey dict)
    => NoDuplicateKey (dict :: Dict k v) where
instance Assert (HasNoDuplicateKey dict)
    => NoDuplicateKey (dict :: Dict k v) where
class Assert (IsInDict key dict)
    => HasKey (key :: k) (dict :: Dict k v) where
instance Assert (IsInDict key dict)
    => HasKey (key :: k) (dict :: Dict k v) where

type family GetHead (ls :: [a]) = (r :: a) where
    GetHead (h ': ts) = h
type family IsEmpty (ls :: [a]) = (r :: Bool) where
    IsEmpty '[] = 'True
    IsEmpty a = 'False
type family Equal (lhs :: a) (rhs :: a) = (r :: Bool) where
    Equal a a = 'True
    Equal a b = 'False
type NotEqual a b = Not (Equal a b)


type family IsFirst (e :: a) (ls :: [a]) = (r :: Bool) where
    IsFirst e (e ': ts) = 'True
    IsFirst e ls = 'False
type IsFirstKey k d = IsFirst k (Dict2Keys d)

---------- Bool

type family Not (a :: Bool) = (r :: Bool) where
    Not 'True = 'False
    Not 'False = 'True
type Nxor a b = Not (Xor a b)
type family Xor (lhs :: Bool) (rhs :: Bool) = (r :: Bool) where
    Xor a a = 'False
    Xor a b = 'True
type family Or (lhs :: Bool) (rhs :: Bool) = (r :: Bool) where
    Or 'False 'False = 'False
    Or a b = 'True
type family And (lhs :: Bool) (rhs :: Bool) = (r :: Bool) where
    And 'True 'True = 'True
    And a b = 'False

