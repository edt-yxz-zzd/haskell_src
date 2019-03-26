{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}


module Regex where
import SeedUtils (compareLen, eqLen, leLen)
import Explain
import Container
import Language
-- import GenerativeGrammar



{-
data Regex m a
    = T (m a)
    | Con (m [Regex m a])
    | Alt (m [Regex m a])
    | Star (m (Regex m a))
-}


data Regex' t
    = Sym' t
    | Con' [Regex' t]
    | Alt' [Regex' t]
    | Star' (Regex' t)
    deriving (Eq, Ord, Show, Read)
data Regex e t
    = Sym t
    | Con [e t]
    | Alt [e t]
    | Star (e t)
    deriving (Eq, Ord, Show, Read)

instance Functor Regex' where
    fmap f (Sym' t) = Sym' $ f t
    fmap f (Con' es) = Con' $ map (fmap f) es
    fmap f (Alt' es) = Alt' $ map (fmap f) es
    fmap f (Star' e) = Star' $ fmap f e
instance Functor e => Functor (Regex e) where
    fmap f (Sym t) = Sym $ f t
    fmap f (Con es) = Con $ map (fmap f) es
    fmap f (Alt es) = Alt $ map (fmap f) es
    fmap f (Star e) = Star $ fmap f e
instance Explain (Regex Regex' t) (Regex' t) where
    explain (Sym' t) = Sym t
    explain (Con' es) = Con es
    explain (Alt' es) = Alt es
    explain (Star' e) = Star e
instance Make (Regex Regex' t) (Regex' t) where
    make (Sym t) = Sym' t
    make (Con es) = Con' es
    make (Alt es) = Alt' es
    make (Star e) = Star' e

instance Eq t => Re t Regex' where
    fromRegex' = id
    toRegex' = id
{-
class (Explain (Regex e t) e, Make (Regex e t) e)
    => ReMake t e where
    before_con :: [e] -> [e]
    before_alt :: [e] -> e
    before_star :: e -> e
    before_sym :: t -> e
-}

class (Re t e, DynSet [t] (e t), Language [t] (e t)) => RealRe t e where
    -- in concept, impratical set op

class   ( Eq t, Explain (Regex e t) (e t), Make (Regex e t) (e t)
        , Functor e)
    => Re t e where
    -- constructors
    con :: [e t] -> e t
    alt :: [e t] -> e t
    star :: e t -> e t
    sym :: t -> e t
    con = make . Con
    alt = make . Alt
    star = make . Star
    sym = make . Sym
    alt2 :: e t -> e t -> e t
    alt2 e1 e2 = case (explain e1, explain e2) of
        (Alt es1, Alt es2) -> alt $ es1++es2
        (Alt es1, _) -> alt $ e2:es1
        (_, Alt es2) -> alt $ e1:es2
        (_, _) -> alt [e1, e2]


    re_eq_best_effort :: Eq t => e t -> e t -> Bool
        -- True ==>> eq
        -- False ==>> eq or not eq
    re_eq_best_effort = re_eq_with_struct
    re_maynot_eq :: Eq t => e t -> e t -> Bool
        -- False ==>> eq ==>> {True, False}
        -- not eq ==>> True ==>> {eq, not eq}
    re_maynot_eq e = not . re_eq_with_struct e



    re_eq_with_struct :: Eq t => e t -> e t -> Bool
    re_eq_with_struct e1 e2 = case (explain e1, explain e2) of
        (Sym t1, Sym t2) -> t1 == t2
        (Con es1, Con es2) -> eq_res es1 es2
        (Alt es1, Alt es2) -> eq_res es1 es2
        (Star e1, Star e2) -> re_eq_with_struct e1 e2
        _ -> False
      where
        eq_res es1 es2 = (eqLen es1 es2 &&) .
                        all id $ zipWith re_eq_with_struct es1 es2



    toRegex' :: Re t e => e t -> Regex' t
    toRegex' e = case explain e of
        Sym t -> Sym' t
        Con es -> Con' $ map toRegex' es
        Alt es -> Alt' $ map toRegex' es
        Star e -> Star' $ toRegex' e
    fromRegex' :: Re t e => Regex' t -> e t
    fromRegex' (Sym' t) = sym t
    fromRegex' (Con' es) = con $ map fromRegex' es
    fromRegex' (Alt' es) = alt $ map fromRegex' es
    fromRegex' (Star' e) = star $ fromRegex' e


    re_null, re_dead :: e t
    re_null = con [] -- one
    re_dead = alt [] -- zero | {} - con*direct_product | alt+union

    re_nullable :: e t -> Bool
    re_nullable e = case explain e of
        Sym _ -> False
        Con es -> all re_nullable es
        Alt es -> any re_nullable es
        Star _ -> True

    re_productive :: e t -> Bool
    re_productive e = case explain e of
        Sym _ -> True
        Con es -> all re_productive es
        Alt es -> any re_productive es
        Star _ -> True



-- -}
-- -}
-- -}
-- -}
-- -}
-- -}


