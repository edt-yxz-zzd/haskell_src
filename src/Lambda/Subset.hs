{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

{- LANGUAGE ExistentialQuantification #-}
{- LANGUAGE GADTs #-}
{- LANGUAGE DefaultSignatures #-}




module Lambda.Subset
    ( Subset (..), Constraint(..)
    --, UnsafeSubsetConstructor()
    , makeUnsafeSubsetConstructor
    , default_make_subset_element
    , NoConstraint, WholeSetOf
    , no_constraint
    )
where

import Seed.Boxed
import Data.Typeable




newtype UnsafeSubsetConstructor a = UnsafeSubsetConstructor__private
    {runUnsafeSubsetConstructor__private
        :: SubsetConstraint a -> WholeSetType a -> a
    }
makeUnsafeSubsetConstructor
    :: (SubsetConstraint a -> WholeSetType a -> a)
    -> UnsafeSubsetConstructor a
makeUnsafeSubsetConstructor = UnsafeSubsetConstructor__private


default_make_subset_element
    :: (Subset a, Monad m) => SubsetConstraint a -> WholeSetType a -> m a
default_make_subset_element c f =
    if unlabel a $ satisfy_subset_constraint c f
    then return a
    else fail $
        "not satisfy constraint: " ++
        unlabel a subset_type_description ++
        " : " ++
        unlabel c constraint_type_description ++
        " : " ++
        to_constraint_object_description c
    where
        a = make c f
        make = runUnsafeSubsetConstructor__private
                __private__unsafe_subset_constructor

class Eq a => Constraint a where
    constraint_type_description :: Label a String
    to_constraint_object_description :: a -> String

type LS a = Label a String
instance (Constraint a, Constraint b) => Constraint (a,b) where
    constraint_type_description :: forall . Label (a,b) String
    constraint_type_description = box . show $
        ( unbox @(Label a String) constraint_type_description
        , unbox @(Label b String) constraint_type_description
        )
    to_constraint_object_description (a,b) = show
        ( to_constraint_object_description a
        , to_constraint_object_description b
        )
_paren :: String -> String
_paren a = "(" ++ a ++ ")"
_wrap :: String -> [String] -> String
_wrap a ls = concat $ a : map _paren ls
instance (Constraint a, Constraint b) => Constraint (Either a b) where
    constraint_type_description = box . _wrap "Either " $
        [ unbox @(LS a) constraint_type_description
        , unbox @(LS b) constraint_type_description
        ]
    to_constraint_object_description (Left a) = _wrap "Left " $
        [to_constraint_object_description a]
    to_constraint_object_description (Right b) = _wrap "Right " $
        [to_constraint_object_description b]



class Constraint (SubsetConstraint a) => Subset a where
    -- rename it "DynamicSubset"
    -- why?
    --  we can use "type" to represent a subset
    --      e.g. (a, [a]) <==> 1[a]
    --           ModUInt256 <==> Word8
    --           ModUInt5 <==> ??
    --  except when there are lots of / infinite types we have to define
    --  this is a dynamic typing solution for Bijection
    -- if static type system is power enough,
    --      the constraint should expression as a constainted type
    --          Integral i /\ i > 0 => UInt i
    type WholeSetType a :: *
    type SubsetConstraint a :: *
    -- fail if not satisfy constaint
    subset_type_description :: Label a String
    to_subset_object_description :: a -> String
    __private__unsafe_subset_constructor :: UnsafeSubsetConstructor a
    __private__unsafe_subset_constructor = undefined
    make_subset_element
        :: Monad m => SubsetConstraint a -> WholeSetType a -> m a
    make_subset_element = default_make_subset_element

    satisfy_subset_constraint
        :: SubsetConstraint a -> WholeSetType a -> Label a Bool
    satisfy_subset_constraint c t = case make_subset_element c t of
        Just a -> label a True
        -- Just _ -> box True
        _ -> box False

    verify_subset_constraint
        :: SubsetConstraint a -> a -> Bool
    verify_subset_constraint c a = c == get_subset_constraint a
    get_subset_constraint :: a -> SubsetConstraint a
    to_whole_set_element :: a -> WholeSetType a
    view_subset_element :: a -> (SubsetConstraint a, WholeSetType a)
    get_subset_constraint = fst . view_subset_element
    to_whole_set_element = snd . view_subset_element
    view_subset_element a = (get_subset_constraint a, to_whole_set_element a)

    {-# MINIMAL subset_type_description, to_subset_object_description
        , (make_subset_element
          | __private__unsafe_subset_constructor, satisfy_subset_constraint
          )
        , ( view_subset_element
          | (get_subset_constraint, to_whole_set_element)
          )
     #-}

data NoConstraint
no_constraint = undefined
type WholeSetOf = Label NoConstraint

instance Eq NoConstraint where
    a == b = True
instance Ord NoConstraint where
    compare a b = EQ
    a < b = False
    a > b = False
    a <= b = True
    a >= b = True
    min = const
    max = const


instance Constraint NoConstraint where
    constraint_type_description = box "NoConstraint"
    to_constraint_object_description = const "NoConstraint"
instance Subset (WholeSetOf a) where
    type SubsetConstraint (WholeSetOf a) = NoConstraint
    type WholeSetType (WholeSetOf a) = a
    subset_type_description = box "WholeSet"
    to_subset_object_description a = "WholeSet"
    make_subset_element _ = return . box
    to_whole_set_element = unbox
    get_subset_constraint = const no_constraint


instance (Subset a, Subset b) => Subset (a, b) where
    type SubsetConstraint (a,b) = (SubsetConstraint a, SubsetConstraint b)
    type WholeSetType (a,b) = (WholeSetType a, WholeSetType b)
    to_subset_object_description (a, b) = show $
        (to_subset_object_description a, to_subset_object_description b)
    subset_type_description = box . show $
        ( unbox @(LS a) subset_type_description
        , unbox @(LS b) subset_type_description
        )

    make_subset_element (ca, cb) (wa, wb) = do
        a <- make_subset_element ca wa
        b <- make_subset_element cb wb
        return (a,b)

    to_whole_set_element (a, b) =
        ( to_whole_set_element a
        , to_whole_set_element b
        )
    get_subset_constraint (a, b) =
        ( get_subset_constraint a
        , get_subset_constraint b
        )



data Value2Constraint a ca wa



{-
data LiftedValue a b = LiftedValue a b
    deriving (Show, Eq, Ord, Read)
data LiftedConstraint a c = LiftedConstraint a c
    deriving (Show, Eq, Ord, Read)
instance (Subset a, Eq a, Constraint c)
    => Constraint (LiftedConstraint a c) where
    constraint_type_description = box . _wrap "LiftedConstraint " $
        [ unbox @(LS a) subset_type_description
        , unbox @(LS c) constraint_type_description
        ]
    to_constraint_object_description (LiftedConstraint a c) =
        _wrap "LiftedConstraint " $
        [to_subset_object_description a, to_constraint_object_description c]

instance (Subset a, Eq a, Constraint c)
    => Constraint (LiftedConstraint a c) where
    constraint_type_description = box . _wrap "LiftedConstraint " $
        [ unbox @(LS a) subset_type_description
        , unbox @(LS c) constraint_type_description
        ]
    to_constraint_object_description (LiftedConstraint a c) =
        _wrap "LiftedConstraint " $
        [to_subset_object_description a, to_constraint_object_description c]
-}

data LiftedValue a b = LiftedValue__private (a, b)
    deriving (Show, Eq, Ord, Read)
data LiftedConstraint a b = LiftedConstraint__private (a, b)
    deriving (Show, Eq, Ord, Read)
instance (Constraint a, Constraint c)
    => Constraint (LiftedConstraint a c) where
    constraint_type_description = box . _wrap "LiftedConstraint " $
        [ unbox @(LS a) constraint_type_description
        , unbox @(LS c) constraint_type_description
        ]
    to_constraint_object_description (LiftedConstraint__private c) =
        _wrap "LiftedConstraint " $
        [to_constraint_object_description c]

instance (Subset a, Subset b) => Subset (LiftedValue a b) where
    type SubsetConstraint (LiftedValue a b) =
                        LiftedConstraint a (SubsetConstraint b)
    type WholeSetType (LiftedValue a b) = WholeSetType (a, b)
    subset_type_description = box . _wrap "LiftedValue " $
        [ unbox @(LS a) subset_type_description
        , unbox @(LS b) subset_type_description
        ]

    to_subset_object_description (LiftedValue__private ab) = _wrap "LiftedValue " $
        [ to_subset_object_description ab ]

    get_subset_constraint (LiftedValue__private ab) =
        LiftedConstraint__private (get_subset_constraint ab)
    to_whole_set_element (LiftedValue__private ab) = to_whole_set_element ab
    make_subset_element (LiftedConstraint__private c) wab = do
        b <- make_subset_element c wab
        return $ LiftedValue__private wab

unLiftedValue :: LiftedValue a b -> (a, b)
unLiftedValue (LiftedValue__private ab) = ab
unLiftedConstraint :: LiftedConstraint a b -> (a, b)
unLiftedConstraint (LiftedConstraint__private ab) = ab
mkLiftedConstraint :: (a, b) -> LiftedConstraint a b
mkLiftedConstraint = LiftedConstraint__private


data SubsetList c a = SubsetList c [a]
    deriving (Eq, Ord, Show, Read)
newtype ListConstraint c = ListConstraint c
    deriving (Eq, Ord, Show, Read)

instance Constraint c => Constraint (ListConstraint c) where
    constraint_type_description = box . _wrap "ListConstraint " $
        [ unbox @(LS c) constraint_type_description]
    to_constraint_object_description (ListConstraint c) =
        _wrap "ListConstraint " $
        [to_constraint_object_description c]
--instance (Subset a) => Subset (SubsetList (SubsetConstraint a) a) where
instance (Subset a, c ~ SubsetConstraint a)
    => Subset (SubsetList c a) where
    type SubsetConstraint (SubsetList c a) = ListConstraint (SubsetConstraint a)
    type WholeSetType (SubsetList c a) = [WholeSetType a]
    make_subset_element (ListConstraint c) ls = do
        ls' <- mapM (make_subset_element c) ls
        return $ SubsetList c ls'

    subset_type_description = box . _wrap "SubsetList " $
        [ unbox @(LS (SubsetConstraint a)) constraint_type_description
        , unbox @(LS a) subset_type_description
        ]
    to_subset_object_description (SubsetList c ls) = _wrap "SubsetList " $
        [ to_constraint_object_description c
        , show $ map to_subset_object_description ls
        ]
    get_subset_constraint (SubsetList c ls) = ListConstraint c
    to_whole_set_element (SubsetList c ls) = map to_whole_set_element ls


--constraint

