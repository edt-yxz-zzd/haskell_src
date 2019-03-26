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


module GenerativeGrammar where
import SeedUtils (compareLen, eqLen, leLen)
import Container (iter)
import NonNullList
import Data.Either
import Regex


type Symbol = Either
type Syms n t = [Symbol n t] -- sentential form, rhs

-- left hand side may be all terminals
-- see [page 18] Parsing Techniques - A Practical Guide (2ed)
--                  (by Dick Grune, Ceriel J.H. Jacobs)
--      Fig. 2.5.Grammar for the movements of a Manhattan turtle
type PS_Rule n t= (NonNullList (Symbol n t), Syms n t)
-- see [page 14] Parsing Techniques
--      Definition 2.1: generative grammar
-- see [page 42]
--      1) allowing a set of start symbols
--          e.g. DFA cannot transform to one final state
--               parse CFG on DFA will get output CFG with start symbols:
--                  {(st0, S, final_st)}
--      2) left-hand side is empty
--          model noise
--          e.g. comments and spaces that we ignores
class (Eq n, Eq t) => PhraseStructureGrammar n t g | g -> n t where
    -- Type0; phrase structure
    -- sym+ ::= sym*
    all_start_nonterminals :: g -> [n] -- or one symbol??
    all_nonterminals :: g -> [n]
    all_terminals :: g -> [t]
    all_ps_rules :: g -> [PS_Rule n t] -- countable
    default all_ps_rules
        :: ContextSensitiveGrammar n t g => g -> [PS_Rule n t]
    all_ps_rules = map cs_rule2ps_rule . all_cs_rules


class PhraseStructureGrammar n t g => AllowEmptyLHS n t g where
    all_null_lhs_rules :: g -> [Syms n t]
    -- let them be [syms]
    -- given them a name, e.g. Noise ::= Star (Alt $ map enter [syms])
    --      where enter (h:syms) = Left Noise : h : enter syms
    --            enter [] = [Left Noise]
    -- then insert Noise everywhere
class PhraseStructureGrammar n t g => FiniteRules n t g where
    -- all_rules are finite




is_monotonic_rule :: PS_Rule n t -> Bool
is_monotonic_rule (lhs, rhs) = leLen (iter lhs) rhs

class PhraseStructureGrammar n t g => MonotonicGrammar n t g where
    -- Type1 monotonic
    -- forall rule: len lhs <= len rhs
    -- all is_monotonic_rule $ all_ps_rules g

cs_rule2ps_rule :: CS_Rule n t -> PS_Rule n t
cs_rule2ps_rule (ss1, (n, ss), ss2) =
    ( unsafe_list2nonnull (ss1 ++ Left n : ss2)
    , ss1 ++ ss ++ ss2)
type CS_Rule n t = (Syms n t, CF_Rule n t, Syms n t)
class PhraseStructureGrammar n t g => ContextSensitiveGrammar n t g where
    -- Type0 non-monotonic context-sensitive
    -- Type1 monotonic context-sensitive
    -- ~a~ N ~b~ = ~a~ ~c~ ~b~ -- len(~c~) == 0??
    all_cs_rules :: g -> [CS_Rule n t]
    default all_cs_rules
        :: ContextFreeGrammar n t g => g -> [CS_Rule n t]
    all_cs_rules = map cf_rule2cs_rule . all_cf_rules



cf_rule2ps_rule :: CF_Rule n t -> PS_Rule n t
cf_rule2cs_rule :: CF_Rule n t -> CS_Rule n t
cf_rule2ps_rule (n, ss) = (return (Left n), ss)
cf_rule2cs_rule rule = ([], rule, [])
type CF_Rule n t = (n, Syms n t)
class ContextSensitiveGrammar n t g => ContextFreeGrammar n t g where
    -- Type2; context-free
    all_cf_rules :: g -> [CF_Rule n t]  -- countable
    default all_cf_rules
        :: LinearGrammar n t g => g -> [CF_Rule n t]
    all_cf_rules = map linear_rule2cf_rule . all_linear_rules

    identification_mechanism :: g -> n -> [Syms n t]
    identification_mechanism g n =
        [rhs | (n', rhs) <- all_cf_rules g, n'==n]
    nullable :: g -> n -> Bool
    {- should handle recursion
    nullable g n = elem [] rhss || any (all nullable) rhss where
        rhss = identification_mechanism g n
    -}
    nullable g n = nullable_ [] n where
        nullable_ avoid m =
            if elem m avoid then False
            else (elem [] rhss ||) $
                flip any rhss $ all (nullable_ avoid') . lefts
            where
                rhss = identification_mechanism g m
                avoid' = m : avoid

    directly_recursive :: g -> n -> Bool
    directly_recursive g n = any (elem $ Left n) $ identification_mechanism g n

    recursive :: g -> n -> Bool
    left_recursive :: g -> n -> Bool
    right_recursive :: g -> n -> Bool
    recursive g n = recursive_find [] n where
        recursive_find avoid m =
            if elem m avoid then False
            else elem n reaches ||
                any (recursive_find $ m:avoid) reaches
            where
                rhss = identification_mechanism g m
                reaches = [m | rhs <- rhss, Left m <- rhs]
    left_recursive g n = recursive_find [] n where
        recursive_find avoid m =
            if elem m avoid then False
            else elem n heads ||
                any (recursive_find $ m:avoid) heads
            where
                rhss = identification_mechanism g m
                heads = [h | Left h:_ <- rhss]
    right_recursive g n = recursive_find [] n where
        recursive_find avoid m =
            if elem m avoid then False
            else elem n lasts ||
                any (recursive_find $ m:avoid) lasts
            where
                rhss = identification_mechanism g m
                lasts = lefts $ map last [rhs | rhs@(_:_) <- rhss]



    productive :: g -> n -> Bool
    productive g n = productive_ [] n where
        productive_ avoid m =
            if elem m avoid then False
            else ([] `elem` rhss ||) $
                any (all (productive_ $ m:avoid) . lefts) rhss
            where
                rhss = identification_mechanism g m
    self_embedding :: g -> n -> Bool
        -- X ->+ ~a+~ X ~b+~ where ~x+~ means not nullable

    reachables_from :: g -> n -> [n]

    -- style of notation for CFGs
    -- BNF - Backus-Naur Form
    --      <n> ::= <n> t | t | <n>
    --      n = re"<.*>"; t = re"[^<>\s|]+";
    --      rule = n "::=" (n|t)* ("|" (n|t)*)*
    --
    -- CF van Wijngaarden
    --      n : n, t symbol; t symbol; n.
    --      n = t = re"(?!symbol)\w+"
    --      rule = n ":" (n | t "symbol")* (";" (n | t "symbol")*)* "."

    -- Extended BNF(EBNF)
    --      ? * +
    -- regular right part grammars RRP grammars


type RRP_Rule n t = (n, Regex' (Symbol n t))
class ContextFreeGrammar n t g => RegularRightPartGrammar n t g where
    -- Type 2 CF RRP
    -- regular right part grammars RRP grammars
    -- now all_cf_rules are indeed countable infinite
    --      iterative interpretation
    -- we offer right recursive interpretation below

    all_rrp_rules :: g -> [RRP_Rule n t]


linear_rule2cf_rule :: L_Rule n t -> CF_Rule n t
linear_rule2cf_rule (n, Left ts) = (n, map Right ts)
linear_rule2cf_rule (n, Right (ts1, n', ts2)) =
    (n, map Right ts1 ++ [Left n'] ++ map Right ts2)
type L_Rule n t = (n, Either [t] ([t], n, [t]))
class ContextFreeGrammar n t g => LinearGrammar n t g where
    all_linear_rules :: g -> [L_Rule n t]  -- countable
    default all_linear_rules
        :: RightLinearGrammar n t g => g -> [L_Rule n t]
    all_linear_rules = map right_linear_rule2linear_rule . all_right_linear_rules

right_linear_rule2linear_rule (n, (ts, Nothing)) = (n, Left ts)
right_linear_rule2linear_rule (n, (ts, Just n')) = (n, Right (ts, n',[]))
type RL_Rule n t = (n, ([t], Maybe n))
-- regular grammars(RE grammars)
-- or finite-state grammars(FS grammars)
class LinearGrammar n t g => RightLinearGrammar n t g where
    -- Type3; right-regular
    all_right_linear_rules :: g -> [RL_Rule n t]
    default all_right_linear_rules
        :: FiniteChoiceGrammar n t g => g -> [RL_Rule n t]
    all_right_linear_rules = map fc_rule2right_linear_rule . all_fc_rules

type LL_Rule n t = (n, (Maybe n, [t]))
class LinearGrammar n t g => LeftLinearGrammar n t g where
    -- Type3; left-regular
    all_left_linear_rules :: g -> [LL_Rule n t]
    default all_left_linear_rules
        :: FiniteChoiceGrammar n t g => g -> [LL_Rule n t]
    all_left_linear_rules = map fc_rule2left_linear_rule . all_fc_rules



fc_rule2right_linear_rule (n, ts) = (n, (ts, Nothing))
fc_rule2left_linear_rule (n, ts) = (n, (Nothing, ts))

type FC_Rule n t = (n, [t])
class (RightLinearGrammar n t g, LeftLinearGrammar n t g)
    => FiniteChoiceGrammar n t g where
    -- Type4 FC; finite-choice
    all_fc_rules :: g -> [FC_Rule n t]



-- see "Elemnts Of The Theory Of Computation (2ed)(1998)(Harry Lewis)"
--      2.3: Finite Automata and Regular Expressions
--      [page 79 below Figure 2-14 - page 83]
data FiniteStateEx n = FS n | Init | Final -- n < Init < Final
    deriving (Eq, Ord, Show, Read)
fs_grammar2regex :: (Ord n, FiniteRules n t g, RightLinearGrammar n t g)
                    g -> Regex' t
fs_grammar2regex = where
    m0 = foldl' M.empty init $ all_start_nonterminals g
    init m n = add m (Init, sym [], FS n)
    m1 = foldl' m0 f $ all_right_linear_rules g
    f m (n, (ts, Nothing)) = add m (FS n, sym ts, Final)
    f m (n, (ts, Just n')) = add m (FS n, sym ts, FS n')
    add m (s0, re, s1) = M.insertWith alt2 (s0, s1) re m


