{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , DatatypeContexts #-}


{-
    donot distinguish terminal and nonterminal in the CFG
    we treat each symbol nonterminal
    but we use token as terminal
    if we input (s0, t, s1) and t.symbol == sym, 
    then we add rule: <s0, sym, s1> = <s0, t, s1>

    original rules:
    -- s :: input nfsm partial state
    X = ; 
    ==>> (s0?, X, s1?> = <s0?, s1?>;
    ==>> <s0?, X, s0?> = <s0?, s0?>;
    Y = A1 A2 ... An ; 
    ==>> (s0?, Y, sn?> = 
                (s0?, A1, s1?> 
                (s1?, A2, s2?> ... (s[n-1]?, An, sn?>
    ==>> <s0?, Y, sn?> = 
                <s0?, A1, s1?> 
                (s1?, A2, s2?> ... (s[n-1]?, An, sn?>
    (s0?, Z, s1?> = <s0?, s?> <s?, Z, s1?>
    ==>> (s0?, X, s1?> = <s0?, s1?> <s1?, X, s1?>

    input (s0, t, s1)
    ==>> <s0, t.sym, s1> = Token<s0, t, s1>
    input (s0, s1)
    ==>> <s0, s1> = Token<s0, s1> = ;
    any s
    ==>> <s, s> = ;
    ==>> <s0, s1> = <s0, s> <s, s1>


    useful rules:
    (s0, X, sn?> = ??

    X = ;
    ==>> (s0, X, sn?> = <s0, s0>
    X = A1 .. An
    ==>> (s0, X, sn?> = (s0, A1, s1?> .. (s[n-1]?, An, sn?>
    ==>> a) (s0, X, sn?> = (s0, NFSM[X, name][init_ts], ?>
        = (s0, {nfsm[init_ps]}, ?>
        b) (s0, nfsm[ps], ?> = (s0, next, s1?> (s1?, {nfsm[ps][next]}, ?>
        c) if final ps: (s0, nfsm[ps], s0> = <s0, s0>
    if exists Token<s, t, s1> and <s0, s> = ;
    ==>> d) (s0, t.sym, s1> = <s0, s> Token<s, t, s1>
    e) <s, s> = ; 
    e') if exists Token<s0, s1> ==>> <s0, s1> = Token<s0, s1> = ;
    f) if <s0, s>=; and <s, s1>=; ==>> <s0, s1>=;


    --------------------
    main rule
    g) (s0, Main, s1?) = (s0, Main, s?> <s?, s1?>

    naming:
    (s, t, ?) - main_wanted
    (s, t, ?> - left_wanted
    (s, nfsm[ps], ?> - right_wanted
    <s0,s1>=; - null_instance
    Token<s0,t,s1> - token_instance
    (s0, Main, s1) - main_instance
    (s0, X, s1> - left_instance
        by a) - topdown_instance
        by d) - buttomup_instance
    (s0, nfsm[ps], s1> - right_instance
        by b) - concate_instance
        by c) - tail_instance


    input:
        rule = (symbol, name, nfsm)
        input_state = s
        main_wanted = (s, symbol)
        null = (s, s)
        token = (s, t, s)

    output grammar:
        -- topdown
        -- if X = A: X wait A; 
        --    i.e. d11[(s0,A,?>].add((s0,X,?>)
        -- if X = A B: X wait A B; X A wait B; 
        --    i.e. d12[(s0,A,?>].add((s0,X,?>, B)
        --         d21[(s1,B,?>].add((s0,X,?>, (s0,A,s1>)
        (s0, main, s2) = (s0, main, s1> <s1,s2>
        (s0, X, s1> = (s0, NFSM[X][name][init_ps], s1>
        (s0, NFSM[X][name][final_ps], s0> = ;
        (s0, NFSM[X][name][ps], s2> = (s0, next, s1> (s1, NFSM[X][name][ps][next], s2>
        (s1, NFSM[X][name][ps][next], s2> = (s1, NFSM[X][name][ps'=ps+next], s2>


        -- bottomup
        (s0, T.sym, s2> = <s0, s1> Token<s1, T, s2>
        <s0, s1> = ;


        instance type
        (s, sym, s)
        (s, a, s> where a = sym | (sym, name, ps) | (sym, name, ps, sym)
        <s, s>
        <s, t, s>

        rule type
        ((s, a, s>, <s,s>)
        ()
        ((s, a, >,)
        ((s, a, >, (s, a, >)
        (<s,s>, <s,t,s>)



    member:
    name - alternative name
    left2name2rp_nfsm :: Map left $ Map name rp_nfsm
        -- why not use a big nfsm?
        -- 1) we allow dead rule, i.e. size name2nfsm == 0
        -- 2) we allow dynamic add nfsms
    terminals :: Set terminal
    token :: Token symbol
        -- why use token instead of terminal in input?
        --    to allow different data even (s0, terminal, s1) were same
        -- why Token symbol instead of Token terminal?
        --    to allow input nonterminal directly
        get_symbol :: token -> symbol
    s = input_state - position of input token; 
    -- treat input tokens as regex or in_nfsm




    method:
    maybe_new_XXX xxx p = if is_new_XXX xxx p then put_new_XXX xxx p else p
    put_new_XXX xxx p = p { priority_queue = put (make_new_XXX xxx) $ priority_queue p }
    process p = case get q of
            Nothing -> p
            Maybe (boxed_xxx, q') -> let p' = p {priority_queue = q'} in
                process $ on_new boxed_xxx p'
        where
            q = priority_queue p
    on_new boxed_xxx p = case boxed_xxx of
        XXX xxx -> on_new_XXX xxx p
        YYY yyy -> on_new_YYY yyy p
    on_new_XXX xxx p = ...

    input_XXX xxx p = ... maybe_new_YYY yyy p ... process p

    framework:
    _put_new new p = p { priority_queue = put new $ priority_queue p }
    put_new new p = if new in queue then p else _put_new new p
    input maybe_new xxx p = process $ maybe_new xxx p
    maybe_new is_new put_new xxx p = if is_new xxx p then p else put_new xxx p


    concrete methods:
    Boxed sym name nfsm s t = Rule (sym, name, nfsm) 
        | State s 
        | Wanted (s, sym)
        | Null (s, s) 
        | Token (s, t, s)
    input_rule rule rule p = input maybe_new_rule
    maybe_new_rule (sym, name, nfsm)@rule p =
        if exists rules[sym][name] 
        then error "DuplicateRuleNameError"
        else put_new (Rule rule) p
    on_new_rule (sym, name, nfsm)@rule p =
        rules[sym][name] = nfsm
        -- a)
        for each left_wanted (s, sym, ?>:
            for each initial ps of nfsm:
                new right_wanted (s, nfsm[ps], ?>
                (s, sym, ?> wait (s, nfsm[ps], ?>
    input_null = input maybe_new_null
    maybe_new_null (h, t)@ht p = maybe_new is_new_null put_new_null ht p' where
        p' = maybe_new_input_state h . maybe_new_input_state t $ p

    on_new_null (h, t) p =
        assert h /= t
        -- f)
        for each <s, h> in p:
            p = maybe_new_null (s, t) p
        for each <t, s> in p:
            p = maybe_new_null (h, s) p

        -- d)
        for each Token<t,T,s> in p:
            put_new_bottomup_instance (h, T.sym, s> = <h,t> Token<t,T,s>
        -- g)
        for each (s0, Main, ?) in p:
            for each (s0, Main, h> in p:
                put_new_main_instance (s0, Main, t) = (s0, Main, h> <h,t>
    input_token = input maybe_new_token
    maybe_new_token (h, _, t)@token p = maybe_new is_new_token put_new_token token p' where
        p' = maybe_new_input_state h . maybe_new_input_state t $ p
    on_new_token (s0, t, s1)@token =
        -- d)
        for each <s, s0> in p:
            put_new_bottomup_instance (s, t.sym, s1> = <s,s0> token

    input_main_wanted = input maybe_new_main_wanted
    maybe_new_main_wanted (s, main)@main_wanted p =
        maybe_new is_new_main_wanted put_new_main_wanted main_wanted p'
        where p' = maybe_new_input_state s p
    on_new_main_wanted (s, main) p =
        -- g)
        maybe_new_left_wanted (s, main, ?>
        (s, main, ?) wait (s, main, ?>

-}







{-
    left - nonterminal symbol
    right - symbol
    symbol = Either left terminal
    name - alternative name
    left2name2rp_nfsm :: Map left $ Map name rp_nfsm
        -- why not use a big nfsm?
        -- 1) we allow dead rule, i.e. size name2nfsm == 0
        -- 2) we allow dynamic add nfsms
    terminals :: Set terminal
    token :: Token symbol
        -- why use token instead of terminal in input?
        --    to allow different data even (s0, terminal, s1) were same
        -- why Token symbol instead of Token terminal?
        --    to allow input nonterminal directly
        get_symbol :: token -> symbol
    s = input_state - position of input token; 
    -- treat input tokens as regex or in_nfsm
    input - (s, s) | (s, token, s)
    s0 -[null]-> s1 ::=
        s0==s1
        exists (s0, s1)
        exists s2, s.t. s0-[null]->s2-[null]->s1
    s0 =[null]= s1 ::= s0-[null]->s1-[null]->s0
    std s = [=s=] -- std input_state in equivalence class

    rp_ps = right_part_partial_state of rp_nfsm
    rp_ts = right_part_total_state of rp_nfsm
    next :: symbol -- some a next symbol after rp_ps
    output_mains :: Map left_instance $ Set [right_instance]
    -- if (s01, t0, s02) -> {[(st1, tt, st2), ... (s11, t1, s12)]}
    -- ==>> s01-[null]->s11, st2-[null]->s02
    -- if (s01, t0, s02) -> {[]}
    -- ==>> s01-[null]->s02
    output_grammar :: Map right_instance $ Set [right_instance]
    -- if (s01, t0, s02) -> {[(st1, tt, st2), ... (s11, t1, s12)]}
    -- ==>> s01==s11, st2==s02
    -- if (s01, t0, s02) -> {[]}
    -- ==>> s01==s02
    -- if _ -> {[..., (s21, t2, s22), (s11, t1, s12), ...]}
    -- ==>> s12-[null]->s21
    -- if (s, t, s) -> {[]}, _->{[..., (s21, t2, s22), (s,t,s), (s11, t1, s12), ...]}
    -- ==>> s==s12
    output_null_transition :: Map s $ Set s


    main_wanteds :: Set left_wanted -- left_wanted -[back]-> root
    left_wanted = (std s, left)
        -- dict back -> right_wanted where left == right_wanted.next
    left_name = (left, name)
    left_name_wanted = (std s, left_name) -- -[back]-> left_name
        -- care null production/rule
    unpacked_left_name_wanted = (std any >= std s, left_name)
        -- dict back -> left_name_wanted
        -- to search input_instances for the first next_symbol
    total_right_wanted = (std s, left_name, rp_ts)
        -- -[back]-> (if exists)left_name_wanted 
        --           if rp_ts == init rp_nfsm
        -- dict back -> right_wanted
        -- i.e. {total_right_wanted:Set right_wanted}
    partial_right_wanted = (std s, left_name, rp_ps)
        -- dict back -> total_right_wanted
        -- is_final rp_ps ==>> right_instance
    right_wanted = (std s, left_name, rp_ps, next)
        -- -[back]-> partial_right_wanted
        -- ==>> left_wanted[next] + 
        --      total_right_wanted[std right_instance[next].end_s, left_name, transition rp_ps next]
    std2next2right_wanteds :: Map (std s) $ Map next $ Set right_wanted
        -- right_instance ~~> {right_wanted}
    std2next2right_instances :: Map (std s) $ Map next $ Set right_instance
        -- right_wanted ~~> {right_instance}
    left_wanted


    input_instance = (std s0, token, std s1)
    list_instance = (std s0, left_name, std s1)
    right_instance = Either input_instance list_instance
    list_instance2list = {list_instance:Set [right_instance]}
    std2next2input_instances
        -- unpacked_left_name_wanted ~~> {input_instance}
    on input (s0, s1)
        new input_state s0, s1
        null_transition_digraph.add(s0,s1)
        ??new big [=s0, s1=]??
            merge keys: Map (std s) (container a)
            update (std s)
        ??new std s<-[null]-s0??
            new unpacked_left_name_wanted...
    on input (s0, t, s1)
        new input_instance = (std s0, token, std s1)
    on add left_wanted -- main wanted
        main_wanted.add(left_wanted) -- dict back -> root
        new left_wanted
    on new left_wanted
        (std s, left) -- left may be terminal!!!!!!!!!!!
        if left is terminal:
            ...
            return


        new left_name_wanted for all names of left
        -- left_name back-> left
    on new left_name_wanted
        (std s, left_name)
        left_name -> rp_nfsm

        if is_final init_rp_ts ==>> new list_instance (std s, left_name, std s) -> []
        new total_right_wanted
        -- back -> left_name_wanted if rp_ts is init and exists ...wanted

        new unpacked_left_name_wanted for all std x >= std s
        back__unpacked_left_name_wanted2left_name_wanteds -- dict back
    on new unpacked_left_name_wanted
        (std u, left_name)
        (std u, left_name, init_rp_ts, nexts)
        for each next: if std2next2input_instances
    on new total_right_wanted
        new partial_right_wanted for each rp_ps in rp_ts
        back__partial_right_wanted2total_right_wanteds -- dict back
    on new partial_right_wanted
        new right_wanted for each next of nfsm[rp_ps]
        -- back->partial_right_wanted
        if is_final rp_ps ==>> new right_instance
            using back dict to find out left_wanteds
    on new right_wanted
        
    on new right_instance
        std2next2right_instances.add
        right_wanteds = std2next2right_wanteds[std_s][next]
        for each: new right_wanted+right_instance
    on new right_wanted
        std2next2right_wanteds.add
        right_instances = std2next2right_instances[std_s][next]
        for each: new right_wanted+right_instance
    on new right_wanted+right_instance
        
    on new input_instance|list_instance
        new right_instance




-}
{-
 - input: [token] -->> FSM input_state token -->> [(s, t, s)|(s, s)]
 -          token = Token terminal -- i.e. (terminal, data)
 -        [left_wanted] where left_wanted = (input_state, nonterminal)
 - output: Map (input_state, symbol, input_state) [token]
 -      Map right_wanted (Set (s,t,s)) where 
 -      packed_right_wanted = (std s, left_symbol, rp_total_state)
 -      right_wanted = (std_input_state, left_symbol, rp_partial_state, next_right_symbol)
 -      partial_right_wanted = (std s, next_sym)
 -      right_instance = (std s, t, std s)
 -      partial_right_wanted2instances
 -      new_token :: (nonterminal, rp_nfsm, [token]) -> token 
 -      -- [token] in reversed order
 -      token t1, t2 ==>> t1 == t2 -->> t1.symbol == t2.symbol
 -          t1 == t2 -->> t1 issubforest t2
 -      merge right_instance (s0, t0, s1) into (s2, t2, s3) if 
 -          std s0 -[null]-> std s2
 -          t0 issubforest t2
 -          std s3 -[null]-> std s1
 -      left_instance = (lefted_wanted, right_instance) 
 -          = ((s0, sym), (s1, t, s3)) where s0 -[null]-> s1
 - on new_left_wanted:
 -      left_wanted=(s0, sym)
 -      new_packed_right_wanted = (std s0, sym, rp_init_ts)
 -      partial_packed_right_wanted = (std s0, sym)
 -      partial_packed_right_wanted2left_wanted 
 -      = {(std s, left):(s, left)}
 - on new_packed_right_wanted:
 -      // note all std right_wanted are generated
 -      packed_right_wanted = (std s0, left, rp_ts)
 -      new_right_wanteds += {(std s1, left, rp_ps, next)
 -          for rp_ps in rp_ts
 -          for any std s1 s.t. std s0-[null]->std s1}
 -      right_wanted2packed = {(std s1, left, rp_ps):(std s0, left, rp_ts)}
 -      if is_final rp_ts then new_right_instance...
 - on new_right_wanted:
 -      right_wanted = (std_s, left_sym, rp_s, next_sym)
 -      on new_packed_right_wanted: 
 -      -- nexts = next_symbols rp_s
 -      let instances = partial_right_wanted2instances[(std_s, next_sym)]
 -      right_wanted+right_instance -> new_packed_right_wanted
-}

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List -- for foldl'

-- for trc :: DynGraph gr => gr a b -> gr a ()
import Data.Graph.Inductive.Query.TransClos
import Data.Graph.Inductive.PatriciaTree -- for Gr, UGr
-- for mkGraph :: [LNode a] -> [LEdge b] -> gr a b
import Data.Graph.Inductive.Graph


--
-- import Sized

--


class (Eq token, Eq symbol) 
    => TokenOf symbol token | token -> symbol where
    get_symbol :: token -> symbol

class NFSM symbol partial_state total_state nfsm where

type Map k v = Map.Map k v
type Set k = Set.Set k
data (TokenOf symbol token, Ord symbol, Ord name, Ord partial_state,
      NFSM symbol partial_state total_state rp_nfsm, 
      Ord input_state, DynGraph gr)
    => ParserState symbol token name 
        partial_state total_state rp_nfsm input_state gr = P {
        symbol2name2rp_nfsm :: Map symbol (Map name rp_nfsm)
        ,input_tokens :: [(input_state, token, input_state)]
        ,input_nulls :: [(input_state, input_state)]
        ,grammar_news :: [(symbol, name, rp_nfsm)]

        -- equivalences :: Map input_state (Set input_state)
        ,backward_null_instances :: Map input_state (Set input_state)
        -- <s0, s1> -> {s1:{s0}}
        ,input_state2node :: Map input_state Node
        ,reversed_null_digraph :: gr input_state ()

    }
{- 
data (TokenOf sym t, Ord sym, Ord n, Ord ps,
      NFSM sym ps ts rp, 
      Ord s) 
    => P sym t n ps ts rp s = P sym t n ps ts rp s

-}

type P sym t n ps ts rp s gr = ParserState sym t n ps ts rp s gr
{-
add_new_input_state :: P sym t n ps ts rp s -> s -> P sym t n ps ts rp s
add_new_input_state p s = if Map.member s s2node then p else
    node = Map.size s2node
    p {input_state2node = insert s node s2node}
    where s2node = input_state2node p

add_new_input_states :: P sym t n ps ts rp s -> [s] -> P sym t n ps ts rp s
-}

feed_tokens :: P sym t n ps ts rp s gr -> [(s, t, s)] -> P sym t n ps ts rp s gr
feed_tokens = undefined
feed_nulls :: P sym t n ps ts rp s gr -> [(s, s)] -> P sym t n ps ts rp s gr
feed_nulls = undefined
add_rules :: P sym t n ps ts rp s gr -> [(sym, n, rp)] -> P sym t n ps ts rp s gr
add_rules = undefined


add_new_input_states :: Ord s => Map s Node -> [s] -> Map s Node
add_new_input_states = foldl' add_new_input_state
add_new_input_state :: Ord s => Map s Node -> s -> Map s Node
add_new_input_state s2node s = if Map.member s s2node then s2node else
    Map.insert s node s2node where
        node = Map.size s2node

_eat_null :: (Ord s, DynGraph gr) => (Map s Node, gr s ()) -> (s, s) -> (Map s Node, gr s ())
_eat_null (s2node, rg) (h, t) = (s2node', rg')
  where
    s2node' = add_new_input_states s2node [h, t]
    -- s2lnode s = ((s2node' Map.! s), s)
    to_node s = s2node' Map.! s
    rg' = insEdge (to_node t, to_node h, ()) rg
_eat_nulls :: (Ord s, DynGraph gr) => (Map s Node, gr s ()) -> [(s, s)] -> (Map s Node, gr s ())
_eat_nulls = foldl' _eat_null
{- -}
--x_get_null_data :: P sym t n ps ts rp s gr -> (Map s Node, gr s ())
_get_null_data p = (input_state2node p, reversed_null_digraph p)
_put_null_data p (s2node, rg) = p {input_state2node = s2node, reversed_null_digraph = rg}



-- see Data.Graph.Inductive.NodeMap -- NodeMapM

eat_nulls :: P sym t n ps ts rp s gr -> P sym t n ps ts rp s gr
--aeat_nulls = undefined
eat_nulls p = p'' where
    null_data = _get_null_data p
    (s2node0, rg0) = null_data
    (s2node, rg) = _eat_nulls null_data (input_nulls p)
    
    rg' = trc rg
    null_data' = (s2node, rg')
    p' = _put_null_data p null_data'
    p'' = p' {input_nulls = []}
    -- _new_buttomup_instances -}
eat_tokens :: P sym t n ps ts rp s gr -> P sym t n ps ts rp s gr
eat_tokens = undefined
update_grammar :: P sym t n ps ts rp s gr -> P sym t n ps ts rp s gr
update_grammar = undefined
parse :: P sym t n ps ts rp s gr -> P sym t n ps ts rp s gr
parse = eat_tokens . eat_nulls . update_grammar

-- -}





{-
class MapOf key dict | dict -> key where
    key2value :: dict value -> key -> Maybe value
    unsafe_key2value :: dict value -> key -> value
instance Ord key => MapOf key (Map.Map key) where
    key2value = flip Map.lookup
    unsafe_key2value = (Map.!)

class SetOf elem set | set -> elem where
    union :: set -> set -> set
class SetClass set where
    union_sc :: set a -> set a -> set a
instance SetClass set => SetOf elem (set elem) where
    union = union_sc
-- instance SetClass Set.Set where
--    union_sc = Set.union -- Ord a !!

instance Ord elem => SetOf elem (Set.Set elem) where
    union = Set.union

class SetOf partial_state total_state =>
    CFG_RightPart_NFSM symbol partial_state total_state nfsm 
    | nfsm -> symbol total_state where
    transition :: nfsm -> total_state -> symbol -> total_state
    next_symbols :: SetOf symbol symbol_set => nfsm -> total_state -> symbol_set
    -- 
    -- is_empty s ==>> s == transition nfsm s _
    -- is_empty s ==>> is_empty (next_symbols nfsm s)
    -- not (is_empty s) ==>> all (not . is_empty . transition nfsm s) $ next_symbols nfsm s


-}


-- partial_state "<=" is a partial_ordering
-- a <= b iff null_transition (a, b) or a == b
-- a <= b && b <= a =xx=>> a == b; but they are equivalent states
-- DAG forest

-- wanted : (left_symbol, rp_total_state, next_right_symbols)
-- wanted : (left_symbol, rp_partial_state, input_state, next_right_symbol, [(input_state, token, input_state)], initial_input_state) 
-- -- (_, _, initial_input_state, _, [], initial_input_state)
--  | (_, _, latest_input_state, _, (_, _, latest_input_state):_ & _++(>=initial_input_state, _, _):[], initial_input_state)
-- -- (s1, _, _):(_, _, s0):_ ==>> s1 >= s0
--  merge: exists a, b, s.t. 
--         a.left_symbol == b.left_symbol
--         a.rp_partial_state <= b.rp_partial_state
--         a.input_state <= b.input_state
--         a.initial_input_state <= b.initial_input_state
--         a.tokens == b.tokens
--  then we merge b into a, i.e. discard b

{-
type ParserState input_state token = ([(input_state, token, input_state)], a)
class (MapOf symbol dict, 
       SetOf symbol symbol_set, 
       CFG_RightPart_NFSM symbol rp_partial_state rp_total_state rp_nfsm,
       ) => EarleyParser parser where
    get_rules :: parser -> dict rp_nfsm
    get_terminals :: parser -> symbol_set
    feed :: TokenOf symbol token => parser -> parser_state -> (input_state, token, input_state) -> parser_state

-}



