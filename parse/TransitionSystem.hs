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


{-
    Concurrency Theory -- Calculi and Automata for Modelling Untimed and Timed Concurrent Systems (2006) (Howard Bowman)
    3.3 Labelled Transition Systems
-}



module TransitionSystem
where

import Container2
import Explain

class   ( ContainerEx (State a) (StatesT a)
        , ContainerEx (State a) (StatesM a)
        , ContainerEx (State a) (States0 a)
        )
    => TransitionSystemConcept a where
    type State a
    type Label a -- Symbol a
    type StatesT a
    type StatesM a
    type States0 a
    -- 1
    --      Stmt = Assign val expr
    --           | If bool_expr [Stmt] [Stmt]
    --           | While bool_expr [Stmt]
    --           | Call proc
    --      st : (memory, [Stmt]);
    --      sym: Stmt
    --      transition1s sys i' (memory, i:ls) = case i of
    --          Assign val expr -> if i==i'
    --              then singleton (memory[val:=expr], ls) else empty
    --          If bool_expr stmts1 stmts2 -> singleton (memory, news++ls)
    --            where
    --              news = if eval sys memory bool_expr
    --                      then stmts1 else stmts2
    --          While bool_expr stmts -> singleton (memory, news++ls)
    --            where
    --              news = if eval sys memory bool_expr
    --                      then stmts++[i] else []
    --          Call proc -> singleton (memory, getDef sys proc ++ ls)
    --      transition1s _ _ (_, []) = empty
    --
    -- 2
    --      Symbol = Either Terminal Nonterminal
    --      st : ([Terminal], [Symbol])
    --      sym: Maybe Terminal
    --          -- Nothing is internal action/null transition
    --      transition1s sys (Just t') (prefix, Left t:ls) =
    --          if t==t' then singleton (prefix ++ [t], ls) else empty
    --      transition1s sys Nothing (prefix, Right n:ls) =
    --          fromList . map (\news -> (prefix, news++ls)) $
    --          getRuleRightParts sys n
    --      transition1s _ _ _ = empty
class   ( TransitionSystemConcept a
        , OpDynMerge (StatesT a) (StatesM a)
        , OpFrom (States0 a) (StatesM a)
        )
    => TransitionSystem a where
    transition1s :: a -> Label a -> State a -> StatesT a
    initial_states :: a -> States0 a

class   ( TransitionSystem a
        , Optional (StatesT a), Optional (States0 a)
        )
    => OptionalTransitionSystem a where
    transition1x :: a -> Label a -> State a -> Maybe (State a)
    transition1x a sym st = unoptional $ transition1s a sym st
    initial_statex :: a -> Maybe (State a)
    initial_statex = unoptional . initial_states
class   ( OptionalTransitionSystem a
        , Singleton (StatesT a), Singleton (States0 a)
        )
    => DeterministicTransitionSystem a where
    transition11 :: a -> Label a -> State a -> State a
    transition11 a sym st = unsingleton $ transition1s a sym st
    initial_state :: a -> State a
    initial_state = unsingleton . initial_states

