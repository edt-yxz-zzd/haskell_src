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



{-

|-
    Elemnts Of The Theory Of Computation 2ed - Harry Lewis
    [page 58] Chapter 2: FINITE AUTOMATA

    Verification of Sequential and Concurrent Programs
    [page 130] 4 Recursive Programs :: Example 4.2.
-}

module Formal
    ( Formal (..)
    , withSelf
    , withFormal
    , withAutoma
    , Language (..)
    , Sentence, Reversed, GSt, GConf
    , SentenceGenerator (..)
    , InitialsFormal (..) -- with "s"
    , InitialStates (..) -- with "s"
    , StartNonterminals (..)

    , Automaton (..)
    , FinalAutomaton (..)
    , InitialState (..) -- without "s"
    , InitialAutomaton (..) -- without "s"
    , Automa (..)
    , FormalAutomaton (..)
    , FinalFormalAutomaton (..)
    , InitialConfigurations (..)
    , InitialConfigurationsFormal (..)
    )
where
import SeedUtils 
    ( just, merge_lsls, gen_list, may2ls
    , followButFinal, follow_closure
    , filter_not, direct_product2
    , Test (..))

-----
import CleanGrammar (Grammar(..), Symbol, GrammarNT, mkGrammar)
import Boxed
import Data.List (replicate, findIndices, foldl')
import Tree
import Container
import Prelude hiding (null, take, length)
import Data.Maybe (catMaybes)
import Data.Monoid
import ToDoc

type Sentence = []
class Language t lang | lang -> t where
    -- allow duplicates
    -- countable
    sentences :: (?self :: lang) => [Sentence t]
    sentences = sentencesWith ?self

    sentencesWith :: lang -> [Sentence t]
    default sentencesWith :: SentenceGenerator a n t lang
        => lang -> [Sentence t]
    sentencesWith = generateWith

    {- if False, may not stop!!! move to Parse
    acceptWith :: Eq t => lang -> Sentence t -> Bool
    acceptWith lang = (`elem` sentencesWith lang)
    accept :: (?self :: lang, Eq t) => Sentence t -> Bool
    accept = acceptWith ?self
    -}



type PSt o m t = (o, m, [t])
type PConf o m n t = (PSt o m t, [Symbol n t])
data AcceptState = NotAcceptSince | NotAccept | Accept | AcceptSince
instance Test AcceptState where
    test NotAccept = False
    test NotAcceptSince = False
    test Accept = True
    test AcceptSince = True

class   ( Language t lang
        --, InitialConfigurationsFormal (PSt o m t) (Symbol n t) lang
        --, StartNonterminals n lang
        , Monoid o, Eq t
        --, FinalFormalAutomaton o m (Symbol n t) t lang
        , Automa [(o, m, [Symbol n t])] t lang
        )
    => SentenceParser o m n t lang | lang -> o m n t where
    accept :: (?automa :: lang) => Sentence t -> Bool
    -- accept = acceptWith ?automa
    {-
    accept ts = withFormal ?automa $ any is_final_memory
        [m | ((o,m, []), []) <- filter is_halting $ yieldss conf0s] where
        conf0s = map f initial_configurations
        f ((o,m, ts'), syms) = ((o,m, ts'++ts), syms)
    -- -}
    accept = last . accepts

    acceptsDetail :: (?automa :: lang)
        => Sentence t -> [[(o,m, [Symbol n t])]]
    acceptsDetail ts = sts_ls where
        sts0 = withSelf ?automa initial_state
        -- sts_ls :: forall . (?automa :: lang) => [[(o,m, [Symbol n t])]]
        sts_ls = scanl (flip transition) sts0 ts
    acceptsEx :: (?automa :: lang) => Sentence t -> [AcceptState]
    acceptsEx = map toAcceptState . acceptsDetail
    accepts :: (?automa :: lang) => Sentence t -> [Bool]
        -- len result = len sentence + 1
    accepts = map any_final . acceptsDetail

    raw_parse :: (?automa :: lang) => Sentence t -> [o]
    raw_parses :: (?automa :: lang) => Sentence t -> [[o]]
    raw_parse = last . raw_parses
    raw_parses = map (\ls->[o|(o,_,[])<-ls]) . acceptsDetail

    acceptWith :: lang -> Sentence t -> Bool
    acceptWith lang = withAutoma lang accept
    acceptsWith :: lang -> Sentence t -> [Bool]
    acceptsWith lang = withAutoma lang accepts
    raw_parseWith :: lang -> Sentence t -> [o]
    raw_parseWith lang = withAutoma lang raw_parse
    raw_parsesWith :: lang -> Sentence t -> [[o]]
    raw_parsesWith lang = withAutoma lang raw_parses

type Reversed = []
type GSt a t = (a, Reversed t)
type GConf a n t = (GSt a t, [Symbol n t])
class InitialConfigurations cnfg self | self -> cnfg where
    initial_configurations :: (?self :: self) => [cnfg]
    -- initial_configurations = initial_configurationsWith ?self
    initial_configurationsWith :: self -> [cnfg]
    initial_configurationsWith self = withSelf self initial_configurations
class (InitialsFormal st tk sys, InitialConfigurations (st, [tk]) sys)
    => InitialConfigurationsFormal st tk sys
instance (InitialsFormal st tk sys, InitialConfigurations (st, [tk]) sys)
    => InitialConfigurationsFormal st tk sys
class   ( Language t lang
        , InitialConfigurationsFormal (GSt a t) (Symbol n t) lang
        , StartNonterminals n lang
        )
    => SentenceGenerator a n t lang | lang -> a n t where
    -- start_nonterminals :: (?sys :: lang) => [n]
    {-
    initial_configurations :: (?self :: lang) => [GConf a n t]
    initial_configurations = init_confs initial_states $
        map (\n -> [Left n]) start_nonterminals
    -}
    generateFromEx :: (?sys :: lang) => [GConf a n t] -> [(a, Sentence t)]
    generateFromEx = map (\((a, ls), _) -> (a, reverse ls)) .
        filter is_halting . yieldss

    generateEx :: (?sys :: lang) => [(a, Sentence t)]
    generateEx = generateFromEx $ withSelf ?sys initial_configurations
    generate :: (?sys :: lang) => [Sentence t]
    generate = map snd generateEx
    generateWith :: lang -> [Sentence t]
    generateWith lang = withFormal lang generate

init_confs :: [st] -> [[tk]] -> [(st, [tk])]
init_confs = direct_product2 (,)

withSelf :: self -> ((?self :: self) => a) -> a
withSelf self a = let ?self = self in a
class StartNonterminals n self | self -> n where
    start_nonterminalsWith :: self -> [n]
    start_nonterminals :: (?self :: self) => [n]
    start_nonterminals = start_nonterminalsWith ?self
class (Formal st tk sys, InitialStates st sys)
    => InitialsFormal st tk sys where
class InitialStates st self | self -> st where
    initial_statesWith :: self -> [st]
    initial_states :: (?self :: self) => [st]
    initial_states = initial_statesWith ?self
class InitialState st self | self -> st where
    initial_stateWith :: self -> st
    initial_state :: (?self :: self) => st
    initial_state = initial_stateWith ?self





class Automaton sts t a | a -> sts t where
    -- st = (output, memory, instructions); state
    -- sts = [st]; nondeterministic states
    -- [t] - input = [symbol]
    -- a special Formal:
    --    always consume input
    --    i.e. input decrease strictly
    --    while normal Formal the input may grow
    --  partial_nulltransition1 v.s. yield  :: a -> [a]
    --  nulltransition1 v.s. merge_lsls . map yield :: [a] -> [a]
    --  partial_transition1 v.s. wrap step  :: b -> a -> [a]
    --  transition1 v.s. step1s :: b -> [a] -> [a]
    --          step1s _1 = merge_lsls . map (wrap step _1)
    --  transition v.s. yieldss . step1s _1 :: b -> [a] -> [a]
    transition :: (?automa :: a) => t -> sts -> sts
        -- transition is complete version
        -- i.e. transition = nulltransitions . transition1 _1
        --                 = complete_transition :: t -> [st] -> [st]
        --      where nulltransitions = complete :: [st] -> [st]

class Automaton sts t a => FinalAutomaton sts t a where
    any_final :: (?automa :: a) => sts -> Bool
        -- if any st is final
    default any_final
        :: ((?automa :: a), FinalFormalAutomaton o m i input_t a)
        => [(o,m, [i])] -> Bool
    any_final sts = withSelf ?automa $
        any is_final_memory [m | (_,m, ins@[]) <- sts]

    fixed_any_final :: (?automa :: a) => sts -> Bool
        -- later input cannot alter any_final
        -- if True ==>>
        --   any sts' if sts ->* sts':
        --      any_final sts' == any_final sts
    fixed_total_state :: (?automa :: a) => sts -> Bool
        -- ignore later input; trapped
        -- if True ==>>
        --   {sts' | sts ->* sts'} == {sts}
        --   i.e. any t: transition t sts == sts
        -- fixed_total_state ==>> fixed_any_final
        --                              =xx=>> fixed_total_state

    fixed_any_final = fixed_total_state
    fixed_total_state = const False
    toAcceptState :: (?automa :: a) => sts -> AcceptState
    toAcceptState sts | any_final sts =
        if fixed_any_final sts then AcceptSince else Accept
    toAcceptState sts | otherwise =
        if fixed_any_final sts then NotAcceptSince else NotAccept
class (Automaton sts t a, InitialState sts a)
    => InitialAutomaton sts t a where
class (FinalAutomaton sts t a, InitialAutomaton sts t a)
    => Automa sts t a where
withAutoma :: Automaton sts t au => au -> ((?automa :: au) => x) -> x
withAutoma au x = let ?automa = au in x



class   ( Formal (output, memory, [input_t]) instruction self
        , Automaton [(output, memory, [instruction])] input_t self
        , Monoid output)
    => FormalAutomaton output memory instruction input_t self where
    -- Formal (output, memory, unconsumed_input_buffer) instruction
    -- Automaton [(output, memory, unexecuted_instructions])] input_t
    --
    -- properties:
    -- Automaton: sts <<= t `set_eq` merge_lsls [[c]<<=t | c<-sts]
    -- Automaton: [(o,m, [])] <<= t == []
    -- Formal: yields (st@_, ins@[]) == []
    -- Formal: yields conf@(st@(o,m,ts), ins) `set_eq` conf:_
    --          def filter before `set_eq`:
    --              if input ts': (st@(o,m,ts++ts'), ins) |- (_, [])
    -- Formal:
    --    yields (st@(o,m, ts++ts'), ins@[i]) `set_eq`
    --      yieldss . map f $ yields (st@(o,m, ts), ins@[i])
    --      f ((o,m,ts), ins) = ((o,m,ts++ts'), ins)
    --
    -- Formal: step st@(o,m, ts) i == map f $ step (mempty, m, ts) i
    --      f ((o',m,ts), ins) = ((o `mappend` o', m,ts), ins)
    exec :: (?self :: self) => memory -> [input_t] -> instruction
        -> [((output, memory, [input_t]), [instruction])]
        -- if ts may not enough for i, then not execute i
        --   i.e. if exist ts'
        --          s.t. ((_,_,[]),_) in exec m (ts++ts') i
        --              (i.e. may not enough)
        --        then (not exec):
        --          old:???((mempty, m, ts), [i]) occurs in result
        --          new: ((mempty, m', []), [i]) occurs in result
        --            or ((mempty, m', []), [i']) occurs in result
        --              where m' (and i') contains info to continue parse
    exec m ts i = withFormal ?self $ step (mempty, m, ts) i
    {-
    may_exec :: (?self :: self) => memory -> [input_t] -> instruction
        -> Maybe [((output, memory, [input_t]), [instruction])]
        -- like above but if not enough input, then Nothing (not execute)
    -}

    is_waiting_instruction :: (?self :: self) => instruction -> Bool
    -- is_fixed_configuration ((o,m,[]), i:_) = is_waiting_instruction i
class   ( FormalAutomaton output memory instruction input_t self
        , FinalAutomaton [(output, memory, [instruction])] input_t self
        )
    => FinalFormalAutomaton output memory instruction input_t self where
    is_final_memory :: (?self :: self) => memory -> Bool
    -- any_final sts = any is_final_memory [m | (_,_, ins@[]) <- sts]


class Formal st tk sys | sys -> st tk where
    -- st : state of system;
    -- [tk] : input tokens or [instruction]
    -- (st, [tk]) : configuration of system
    -- v.s. Automaton:
    --      Formal.st = Automaton.(input, st.memory, st.output)
    --      Formal.[tk] = Automaton.st.instructions


    -- yieldMay, yieldsMay :: (?sys :: sys) => (st, [tk]) -> [Maybe (st, [tk])]
    -- stepMay :: (?sys :: sys) => st -> tk -> [Maybe (st, [tk])]
    -- stepMay st = map Just . step st
    step :: (?sys :: sys) => st -> tk -> [(st, [tk])]
        -- 1) since input in st, if not enough input
        --   then we should output (st', [i'])
        --   i' is a place holder to indices further input are requred
        --   and will not be treated as a halting state
        --   property
        --      (st', [i']) ->* [(st', i')], i.e. a fixed point
        --
        -- 2) long instruction i.e. several instructions
        --      (multibytes encoding of a char)
        --   e.g. parser with lookahead LR1 or parser of CS grammar
        --       when a nonternimal A comes, may:
        --         if st.left_part_st == 0
        --          1) A ::= rule | ...
        --          2) A _ ::= rule ...
        --              st'.left_part_st := st.left_part_st + 1
        --              put (st', [])
        --          3) both 1) and 2)
        --         else: shift st ...
    -- step st = catMaybes . stepMay st
    yield, yields :: (?sys :: sys) => (st, [tk]) -> [(st, [tk])]
        -- yields ::= yield*
        -- output tokens may be longer than input's
        -- for deterministic system, len(output) == 1
        -- for nondeterministic system, len(output) may be infinite
        -- yields (st, []) == [(st, [])] ??? yes
        --        let (_, []) be halting configuration ???
        -- yield (st, []) == [(st, [])] ???
    yield (st, []) = [] -- [(st, [])]
        -- since yield means execute one instruction
        -- if there is no instruction, then return nothing
    yield (st, t:ls) = fmap f $ step st t where
        f (st, outs) = (st, outs++ls)
    yields conf = yieldss [conf]

    -- yieldss : first "s"="*"=closure; second "s"=pl.=list
    yieldss :: (?sys :: sys) => [(st, [tk])] -> [(st, [tk])]
    yieldss = followButFinal is_final follow_closure yield where
        is_final conf = is_halting conf || is_fixed_configuration conf
            -- why not is_halting??
            --    in new version of yield
            --        halting configuraion ->* []
            -- is_halting -- null . snd
    {-
    yieldss = yields_ where
        -- use merge_lsls instead of concat
        -- since lsls : countable countable
        yield_ = merge_lsls . map yield . filter (not . is_final)
            -- step :: ls -> ls
        yield_m = just . yield_ -- :: ls -> Maybe ls
        yields_ = merge_lsls . gen_list yield_m -- step* :: ls -> ls
        is_final = null . snd
        -- -}

    (|-), (|-*) :: (?sys :: sys, Eq st, Eq tk)
        => (st, [tk]) -> (st, [tk]) -> Bool
    a |- b = elem b $ yield a
    a |-* b = elem b $ yields a




    is_fixed_configuration :: (?sys :: sys) => (st, [tk]) -> Bool
        -- True <==> yield conf == [conf]
    -- is_fixed_configuration = const False
    default is_fixed_configuration
        :: ((?sys :: sys), FormalAutomaton o m i input_t sys)
        => ((o,m, [input_t]), [i]) -> Bool
    is_fixed_configuration ((o,m,[]), i:_) =
        withSelf ?sys $ is_waiting_instruction i


    is_halting :: (?sys :: sys) => (st, [tk]) -> Bool
        -- True ==>> yield conf == [] =xx=>> True
    is_halting (_, []) = True
    is_halting _ = False

    partial_deduce_chain :: (?sys :: sys, Eq st, Eq tk) => [(st, [tk])] -> Bool
    partial_deduce_chain ls = all (uncurry (|-)) $ zip ls $ tail ls

    total_deduce_chain :: (?sys :: sys, Eq st, Eq tk) => [(st, [tk])] -> Bool
    total_deduce_chain [] = False
    total_deduce_chain ls = is_halting (last ls) && partial_deduce_chain ls


    stepWith :: sys -> st -> tk -> [(st, [tk])]
    stepWith sys = withFormal sys step
    yieldWith, yieldsWith :: sys -> (st, [tk]) -> [(st, [tk])]
    yieldWith sys = withFormal sys yield
    yieldsWith sys = withFormal sys yields
    yieldssWith :: sys -> [(st, [tk])] -> [(st, [tk])]
    yieldssWith sys = withFormal sys yieldss

    is_haltingWith :: sys -> (st, [tk]) -> Bool
    is_haltingWith sys = withFormal sys is_halting
    partial_deduce_chainWith :: (Eq st, Eq tk) => sys -> [(st, [tk])] -> Bool
    partial_deduce_chainWith sys = withFormal sys partial_deduce_chain
    total_deduce_chainWith :: (Eq st, Eq tk) => sys -> [(st, [tk])] -> Bool
    total_deduce_chainWith sys = withFormal sys total_deduce_chain
    --}
withFormal :: (Formal st tk sys) => sys -> ((?sys::sys)=>a) -> a
withFormal sys a = let ?sys = sys in a





-- CFG:
--  parse:
--      formal input is the only start symbol ["S"] :: [tk] == [sym]
--      st contains parser input :: [t] == [terminal]
--      when a rule of topmost (i.e. leftmost) applied, we push it
--          st contains rules in RPN :: [(n, [sym])]
--      pitfalls:
--          left recur: cannot reject certain sentences
--          lookup [(k,v)]: may update to Map k v
--  generate:
--      formal input is the only start symbol ["S"] :: [tk]
--      st contains reversed output :: [t]
newtype CFG n t = CFG { unCFG :: GrammarNT n t }
    deriving (Show, Read, Eq, Ord)
newtype ParseCFG n t = ParseCFG { unParseCFG :: CFG n t }
    deriving (Show, Read, Eq, Ord)
type P = ParseCFG
instance Boxed (GrammarNT n t) (P n t) where
    box = mkP
    unbox = unCFG . unParseCFG
mkP = ParseCFG . CFG

newtype GenerateCFG n t = GenerateCFG { unGenerateCFG :: CFG n t }
    deriving (Show, Read, Eq, Ord)
type G = GenerateCFG
instance Boxed (GrammarNT n t) (G n t) where
    box = mkG
    unbox = unCFG . unGenerateCFG
mkG = GenerateCFG . CFG




type Rule n t = (n, [Symbol n t])
type Tk n t = Symbol n t
type St n t = ([Rule n t], [t])
type Conf n t = (St n t, [Tk n t])
instance (Eq t, Eq n) => Formal (St n t) (Tk n t) (G n t) where
    step (reversed_rules, reversed_ts) (Left n) =
        [ ((rule:reversed_rules, reversed_ts), rights)
        | rule@(n', rights) <- rules $ unbox ?sys, n' == n]
    step (rs, ts) (Right t) = [((rs, t:ts), [])]
    is_fixed_configuration = const False

instance (Eq t, Eq n) => Language t (G n t) where
instance InitialConfigurations (St n t, [Tk n t]) (G n t) where
    initial_configurations = init_confs initial_states $
        map (\n -> [Left n]) start_nonterminals
instance (Eq t, Eq n) => SentenceGenerator ([Rule n t]) n t (G n t) where
instance StartNonterminals n (G n t) where
    start_nonterminalsWith = starts . unbox
instance (Eq t, Eq n) => InitialsFormal (St n t) (Tk n t) (G n t) where
instance InitialStates (St n t) (G n t) where
    initial_statesWith _ = [([], [])]
{-
generate grammar = let g = mkG grammar in
    map (reverse . snd . fst) $ withFormal g $
        filter is_halting $ yieldss $
            map (\s-> (to_init_st [], [Left s])) $ starts grammar
-}
instance (Eq t, Eq n) => Formal (St n t) (Tk n t) (P n t) where
    -- RPN - Reverse Polish Notation
    -- st = (rpn_rules, parse_input) :: ([rule], [t])
    -- conf = (st, [sym]) where sym ~ tk
    -- tk v.s. t : tk is Formal token, t is Parser terminal

    -- bug: not [0..1] rules but [0..] rules
    -- step ts (Left n) = may2ls . fmap ((,) ts) $
    --    lookup n . rules $ unbox ?sys
    step (reversed_rules, ts) (Left n) =
        [ ((rule:reversed_rules, ts), rights)
        | rule@(n', rights) <- rules $ unbox ?sys, n' == n]
    step (rs, t0:ts) (Right t1) =
        if t0 == t1 then [((rs, ts), [])] else []
    step _ _ = []
    is_fixed_configuration = const False
instance (Eq t, Eq n, o~Reversed (Rule n t))
    => Automaton [(o,m, [Tk n t])] t (P n t) where
    transition t sts = transition' [] t sts where
        transition' ns t = merge_lsls . map (f ns t)
        f ns t (o,m, (Left n):syms) =
            if member n ns then error "left recur"
            else transition' (n:ns) t
                [ (rule:o, m, rights++syms)
                | rule@(_, rights) <- rulesAt n]
        f _ t (o,m, (Right t'):syms) | t == t' =
            -- apply null rule
            -- bug: forgot update o
                -- [(o,m, syms') | syms' <- rm_null syms] where
            [(rpn++o,m, syms')
            | (rpn, syms') <- rm_null_ex [] syms] where
        f _ _ _ = []
        {-
    transition t sts = merge_lsls $ map (f t) sts where
        f t (o,m, (Left n):syms) = transition t
            -- what if left recur??
            [ (rule:o, m, rights++syms)
            | rule@(_, rights) <- rulesAt n]
        f t (o,m, (Right t'):syms) | t == t' =
            -- apply null rule
            -- bug: forgot update o
                -- [(o,m, syms') | syms' <- rm_null syms] where
            [(rpn++o,m, syms')
            | (rpn, syms') <- rm_null_ex [] syms] where
        f _ _ = []
        --}
        rm_null syms@(Left n : syms') | nullable n = syms : rm_null syms'
        rm_null syms = [syms]
        rm_null_ex rpn syms@(Left n : syms')
            | nullable n = (rpn, syms) : rm_null_ex ((n,[]):rpn) syms'
        rm_null_ex rpn syms = [(rpn, syms)]
        nullable n = any null $ map snd $ rulesAt n
        rulesAt n = [rule
                | rule@(n', rights) <- rules $ unbox ?automa, n' == n]
instance (Eq n, Eq t, o~[Rule n t])
    => SentenceParser o m n t (P n t) where
instance (Eq n, Eq t) => Language t (P n t) where
    sentencesWith = sentencesWith . mkG . unbox
instance (Eq n, Eq t, o~[Rule n t])
    => FinalAutomaton [(o, m, [Symbol n t])] t (P n t) where
    fixed_total_state = null
    any_final sts = any id [True | (_, _, []) <- sts]
instance (Eq n, Eq t, o~[Rule n t])
    => InitialAutomaton [(o, m, [Symbol n t])] t (P n t) where
instance InitialState [([rule], m, [Symbol n t])] (P n t) where
    initial_stateWith self =
        [([], undefined, [Left n]) | n <- starts $ unbox self]
instance (Eq n, Eq t, o~[Rule n t])
    => Automa [(o, m, [Symbol n t])] t (P n t) where






is_halt_conf :: (st, [tk]) -> Bool
is_halt_conf = null . snd
get_st = fst
get_tks = snd

is_succ_st :: St n t -> Bool
is_succ_st = null . snd
is_succ_conf :: Conf n t -> Bool
is_succ_conf conf = is_halt_conf conf && is_succ_st (fst conf)
to_init_st :: [t] -> St n t
to_init_st ts = ([], ts)

acceptWith' :: forall n t. (Eq t, Eq n) => GrammarNT n t -> [t] -> Bool
acceptWith' g ts = let ?sys = box g :: forall. P n t in accept' ts
accept' :: (Eq t, Eq n, ?sys :: P n t) => [t] -> Bool
accept' = any is_succ_conf . list_middle_confs
list_middle_confs :: (Eq t, Eq n, ?sys :: P n t) => [t] -> [Conf n t]
list_middle_confs ts = yieldss . fmap (\n->(to_init_st ts, [Left n]))
        . starts $ unbox ?sys
parse_to_RPNs :: (Eq t, Eq n, ?sys :: P n t) => [t] -> [[Rule n t]]
parse_to_RPNs = map (fst . fst) . filter is_succ_conf . list_middle_confs
parse :: (Eq t, Eq n, ?sys :: P n t) => [t] -> Forest n t
parse = map (unsingleton . rpn2forest) . parse_to_RPNs
parseWith g = with g parse


-- below with == above withFormal . mkP
with :: (Eq n, Eq t) => GrammarNT n t -> ((?sys :: P n t) => a) -> a
-- with g a = let ?sys = mkP g in a
with = withFormal . mkP

-- below yieldWith == above yieldWith . mkP
--yieldWith g conf = with g $ yield conf
--yieldsWith g conf = with g $ yields conf
-- weired: "with g ." fail but "with g $" work!!!!!!!!!!
nextsWith g confs = with g $ concat . map yield $
                    filter (not . is_halt_conf) confs
nextssWith g = gen_list (just . nextsWith g)
-- conf_succ = ((_, []), []) :: ([String], [Either String String])

nexts = nextsWith grammar
nextss = nextssWith grammar


conf0 = (to_init_st $ lsls "aaa", [Left "S"])
grammar = mkGrammar (words "a b z") (words "S") $ fmap words
    [ "S    a A"
    , "S    D b"
    , "A    B C"
    -- , "B    B C" -- left recur
    , "B    a a"
    , "C    A S"
    , "C"
    , "C    D"
    ]
grammarLeftRecur = mkGrammar (words "a b z") (words "S") $ fmap words
    [ "S    a A"
    , "S    D b"
    , "A    B C"
    , "B    B C" -- left recur
    , "B    a a"
    , "C    A S"
    , "C"
    , "C    D"
    ]

-- singleton a = [a]
lsls = map singleton
accept_str' = acceptWith' grammar . lsls
-- accept_str'' = acceptWith (mkG grammar) . lsls
accept_str''' = acceptWith (mkP grammar) . lsls
bTrue = bTrue1' && bTrue1''' && bTrue2 && bTrue3 && bTrue4 && bTrue5
test_strs = ["aaa", "aaaaaaaa"]
bTrue1' = all accept_str' test_strs -- by parse
-- bTrue1'' = all accept_str'' test_strs
    -- by generator.accept, yes - return, no - noreturn
    -- remove to parser.accept
bTrue1''' = all accept_str''' test_strs
    -- by parser.accept
bTrue2 = ([3, 8] ==) . findIndices (accept_str') $
        fmap (flip replicate 'a') [0..8]
bTrue3 = ([3, 8] ==) . findIndices (accept_str''') $
        fmap (flip replicate 'a') [0..8] -- no return
ls = with grammar $ yields conf0
ls1 = yieldWith (mkP grammar) conf0
ls2 = nexts ls1
ls3 = nexts ls2
lss = nextss [conf0]

---
parse_str = parseWith grammar . lsls
forest = parse_str "aaaaaaaa"
bTrue4 = length forest == 1

lens = take 100 . map length $ generateWith $ mkG grammar
lens' = [3,8,13,13,18,18,18,18,18,23,23,23,23,23,23,23,28]
bTrue5 = take (length lens') lens == lens'

accepts_g = acceptsWith ppp . lsls
ppp = mkP grammar
main = do
    print bTrue
    print $ accepts_g "aaaaaaaaa"
    print $ to_doc $ raw_parsesWith ppp $ lsls "aaaaa"
    print $ to_doc $ raw_parsesWith (mkP grammarLeftRecur) $ lsls "aaaaa"
{- output:
True
[False,False,False,True,False,False,False,False,True,False]
[[],
 [],
 [],
 [[("C", []),
   ("B", [Right("a"), Right("a")]),
   ("A", [Left("B"), Left("C")]),
   ("S", [Right("a"), Left("A")])]],
 [],
 []]
*** Exception: left recur

-}
