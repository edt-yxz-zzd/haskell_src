{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}



module Container2__base
where

import ExplainEx
import OpDynTheOnlyValue
import XInt
import SeedUtils (unjust)
import Prelude as P
import Value_Just
import Language.Haskell.TH
import Language.Haskell.Syntax




class Container a where
    type Element a :: *

class (e~Element a, Container a) => ContainerEx e a | a -> e
instance (e~Element a, Container a) => ContainerEx e a

class Container a => CountableContainer a
class CountableContainer a => FiniteContainer a
class Container a => SetConcept a
    -- no OpMember a
    -- no Eq (Element a)
class CountableContainer a => SequenceConcept a
class MappingConcept a where
    -- not a Container
    -- Seq v -> Map Int v -> Container v ??
    -- Set k -> Map k () -> Container k ??
    type KeyType a :: *
    type ValueType a :: *

{-
    Empty
    Singleton / Boxed / ID ??
    Optional
-}


{-
    Set
    Sequence
    Mapping
-}

data Value_Ignore_Just x a = Value_Ignore_Just a









class (Container a, Boolean (NullExResult a)) => OpNull a where
    null :: a -> Bool
    null = explain . null_ex
    type NullExResult a :: *
    null_ex :: a -> NullExResult a
    default null_ex :: OpDynTheOnlyValue (NullExResult a)
        => a -> NullExResult a
    null_ex _ = the_only_value
class (Container a) => OpUnsafeLen a where
    -- not OpNull
    unsafe_len :: a -> UInt -- error or dead loop
    default unsafe_len :: Sized a => a -> UInt
    unsafe_len = len
class (OpUnsafeLen a, OpNull a) => OpLenIfLe a where
    len_if_le :: Integer -> a -> Maybe UInt
    default len_if_le :: Sized a => Integer -> a -> Maybe UInt
    len_if_le i a = safe_from i >>= \u ->
        let n = len a in if n <= u then Just n else Nothing
    len_le :: Integer -> a -> Bool
    len_le i = maybe False (const True) . len_if_le i
    len_lt :: Integer -> a -> Bool
    len_lt i = len_le (i-1)
class   ( FiniteContainer a, IntegralU (LenExResult a)
        , OpLenIfLe a
        )
    => Sized a where
    len :: a -> UInt
    len = explain . len_ex
    type LenExResult a :: *
    len_ex :: a -> LenExResult a
class (Sized a, OpDynTheOnlyValue (LenExResult a))
    => StaticSized a where
    static_len :: Value_Ignore_Just a UInt
    static_len = Value_Ignore_Just $
        explain (the_only_value :: LenExResult a)



class (Container a, Boolean (HasDuplicatesExResult a))
    => OpHasDuplicates a where
    has_duplicates :: a -> Bool
    has_duplicates = explain . has_duplicates_ex
    type HasDuplicatesExResult a :: *
    has_duplicates_ex :: a -> HasDuplicatesExResult a
    default has_duplicates_ex :: a -> Value_False
    has_duplicates_ex _ = the_only_value
class (OpHasDuplicates a, Boolean_False (HasDuplicatesExResult a))
    => NoDuplicates a

class   ( SequenceConcept a, SetConcept a, NoDuplicates a
        , Sized a, NaturalLe1 (LenExResult a)
        , Explain (Maybe (Element a)) a
        )
    => Optional a where
    unoptional :: a -> Maybe (Element a)
    unoptional = explain
    unsafe_unoptional :: a -> Element a
    unsafe_unoptional = unjust . unoptional
class   ( Optional a
        , StaticSized a, NaturalEq1 (LenExResult a)
        , Explain (Value_Just (Element a)) a
        , OpNull a
        , Boolean_False (NullExResult a)
        )
    => Singleton a where
    unsingleton :: a -> Element a
    unsingleton a = let Value_Just e = explain a in e


class (Container a, Boolean (MemberExResult a))
    => OpMember a where
    member :: Element a -> a -> Bool
    member e = explain . member_ex e
    type MemberExResult a :: *
    member_ex :: Element a -> a -> MemberExResult a
    default member_ex :: OpDynTheOnlyValue (MemberExResult a)
        => Element a -> a -> MemberExResult a
    member_ex _ _ = the_only_value
class   ( SetConcept a, SequenceConcept a
        , StaticSized a, NaturalEq0 (LenExResult a)
        , Optional a
        , Explain (Value_EmptySet (Element a)) a
        , OpNull a, OpMember a
        , Boolean_True (NullExResult a)
        , Boolean_False (MemberExResult a)
        )
    => Empty a where




-----------------------------------
-- [a], Maybe a, Value_Just a, Value_EmptySet a
instance Container [a] where
    type Element [a] = a
instance CountableContainer [a] where
instance OpNull [a] where
    type NullExResult [a] = Bool
    null = P.null
    null_ex = P.null
instance OpUnsafeLen [a] where
    unsafe_len = unsafe_from . P.toInteger . length
instance OpLenIfLe [a] where
    len_if_le maxL ls
        | maxL < 0 = Nothing
        | otherwise = fmap (unsafe_from . (maxL-)) $ f maxL ls where
            f n (a:t) = if n == 0 then Nothing else f (n-1) t
            f n [] = Just n


instance Container (Maybe a) where
    type Element (Maybe a) = a
instance CountableContainer (Maybe a)
instance FiniteContainer (Maybe a)
instance OpNull (Maybe a) where
    type NullExResult (Maybe a) = Bool
    null_ex Nothing = True
    null_ex _ = False
    null = null_ex
instance OpUnsafeLen (Maybe a)
instance OpLenIfLe (Maybe a)
instance Sized (Maybe a) where
    type LenExResult (Maybe a) = Value_ZeroOrOne
    len_ex Nothing = Value_ZeroOrOne False
    len_ex _ = Value_ZeroOrOne True
    len Nothing = 0
    len _ = 1
instance SetConcept (Maybe a)
instance OpHasDuplicates (Maybe a) where
    type HasDuplicatesExResult (Maybe a) = Value_False
    has_duplicates_ex _ = the_only_value
    has_duplicates _ = False
instance NoDuplicates (Maybe a)
instance SequenceConcept (Maybe a)
instance Optional (Maybe a) where
instance Eq a => OpMember (Maybe a) where
    type MemberExResult (Maybe a) = Bool
    member_ex e a = Just e == a




{-
type Pair_ a = (a, a)

instance Container (Pair_ a) where
    type Element (Pair_ a) = a
instance CountableContainer (Pair_ a)
instance FiniteContainer (Pair_ a)
instance OpNull (Pair_ a) where
    type NullExResult (Pair_ a) = Value_False
    null = null_ex
    null_ex = const the_only_value
instance OpUnsafeLen (Pair_ a)
instance OpLenIfLe (Pair_ a)
data Value_Two
instance Sized (Pair_ a) where
    type LenExResult (Pair_ a) = Value_Two
    len_ex _ = the_only_value
    len _ = 2
instance StaticSized (Pair_ a) where

instance SetConcept (Pair_ a)
instance SequenceConcept (Pair_ a)
instance Eq a => OpMember (Pair_ a) where
    type MemberExResult (Pair_ a) = Bool
    member_ex a (b, c) = a == b || a == c
-}


instance Container (Value_Just a) where
    type Element (Value_Just a) = a
instance CountableContainer (Value_Just a)
instance FiniteContainer (Value_Just a)
instance OpNull (Value_Just a) where
    type NullExResult (Value_Just a) = Value_False
    null_ex _ = the_only_value
    null _ = False
instance OpUnsafeLen (Value_Just a)
instance OpLenIfLe (Value_Just a)
instance Sized (Value_Just a) where
    type LenExResult (Value_Just a) = Value_One
    len_ex _ = the_only_value
    len _ = 1
instance StaticSized (Value_Just a) where
instance SetConcept (Value_Just a)
instance OpHasDuplicates (Value_Just a) where
    type HasDuplicatesExResult (Value_Just a) = Value_False
    has_duplicates_ex _ = the_only_value
    has_duplicates _ = False
instance NoDuplicates (Value_Just a)
instance SequenceConcept (Value_Just a)
instance Optional (Value_Just a)
instance Singleton (Value_Just a) where
    unsingleton (Value_Just a) = a
instance Eq a => OpMember (Value_Just a) where
    type MemberExResult (Value_Just a) = Bool
    member_ex e a = e == unsingleton a




instance Container (Value_EmptySet a) where
    type Element (Value_EmptySet a) = a
instance CountableContainer (Value_EmptySet a)
instance FiniteContainer (Value_EmptySet a)
instance OpNull (Value_EmptySet a) where
    type NullExResult (Value_EmptySet a) = Value_True
    null_ex _ = the_only_value
    null _ = True
instance OpUnsafeLen (Value_EmptySet a)
instance OpLenIfLe (Value_EmptySet a)
instance Sized (Value_EmptySet a) where
    type LenExResult (Value_EmptySet a) = Value_Zero
    len_ex _ = the_only_value
    len _ = 0
instance StaticSized (Value_EmptySet a) where
instance SetConcept (Value_EmptySet a)
instance OpHasDuplicates (Value_EmptySet a) where
    type HasDuplicatesExResult (Value_EmptySet a) = Value_False
    has_duplicates_ex _ = the_only_value
    has_duplicates _ = False
instance NoDuplicates (Value_EmptySet a)
instance SequenceConcept (Value_EmptySet a)
instance Optional (Value_EmptySet a)
instance Empty (Value_EmptySet a) where
instance OpMember (Value_EmptySet a) where
    type MemberExResult (Value_EmptySet a) = Value_False
    member_ex e a = the_only_value
    member e a = False












--------------------------------






--}
--}
--}
--}
--}
--}
