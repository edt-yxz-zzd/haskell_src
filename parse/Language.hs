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

module Language where
import Container
-- import Data.Sequence


-- type CountableContainer = []
-- type FiniteContainer = Seq

class   ( Eq sym, Set (Sentence lang) lang
        , FiniteContainer sym (Sentence lang), Seq sym (Sentence lang)
        , Iterable (Sentence lang) (Sentences lang)
        )
    => Language sym lang | lang -> sym where
    type Sentences lang :: *
    type Sentence lang :: *
    equivalent_with_respect_to_language
        :: lang -> Sentence lang -> Sentence lang -> Bool
        -- see [page 94]
        --  Elemnts Of The Theory Of Computation (2ed)(1998)(Harry Lewis)
        --  x ~=[L]=~ y ::= "forall z: xz in L == yz in L"

    all_sentences :: lang -> Sentences lang -- countable[finite[sym]]
        -- countable since each sentence is finite long
        -- allow duplicates


