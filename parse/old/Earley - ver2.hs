{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts #-}


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

    -- new wanting ==>> wanting to wanted - topdown
    --            ==>> new wanting->wanted
    -- new wanted ==>> match a instance (or by rule)
    --            ==>> new wanted + instance
    -- new wanted ==>> new wanting (self)
    -- input ==>> new instance ==>> match a wanted ==>> new wanted+instance
    --                         ==>> new input_state; new sym
    -- input ==>> new rp_nfsm ==>> new sym; new (sym, name)
    -- input ==>> main()->wanted ==>> new wanting->wanted; new input_state
    -- // input ==>> initial_state/final_state??
    -- // main_wanted = (s0, sym, ?) | (?, sym, ?) | (s0, ?, ?)
    -- // since we can expand "?" in outside, we just consider (s, sym, ?)
    -- new wanted+instance ==>> back to wanting - bottomup
    --            ==>> new wanting+wanted+instance
    -- new wanting->wanted ==>> new wanted
    -- new wanting->wanted ==>> down to find instance - topdown
    --            ==>> new wanting+wanted+instance
    -- new wanting+wanted+instance 
    --          ==>> new wanting(self)->wanted(next)
    --          ==>> new instance (for wanting)
    -- new input_state
    -- new symbol
    -- new (symbol, name) ==>> new wanting->wanted;
    --
    --
    -- like javafx property listen, we make a dependency digraph
    -- wanting <<== wanted
    -- wanted <<== wanting->wanted;
    -- instance <<== input; wanting+wanted+instance'
    -- wanted+instance <<== wanted!*!instance
    -- wanting->wanted <<== sym_name; wanting; wanting+wanted+instance
    -- wanting+wanted+instance <<== wanting->wanted!*!wanted+instance
    -- sym_name <<== input
    --
    -- NOTE: CARE: wanted+instance, wanting+wanted+instance
    --      they both base on two msgs at same time
    wanted = wanting = (s0, pos) 
        -- pos - position = Null | Main sym | parent_sym[, name[, ps[ child_next_sym]]]
        -- parent_sym = symbol | Main symbol | (symbol, name)
    instance = (wanted, s1) = (s0, pos, s1)

    input:
        rule = (symbol, name, nfsm)
            NFSM[symbol] = NFSM[symbol][name] =  nfsm
                for each name come with symbol
            nfsm = nfsm[ps0] for each init ps
            per rp_nfsm rule:
                nfsm[ps] = [ps-next_sym->ps' in nfsm] NFSM[next_sym] nfsm[ps']
                nfsm[ps] = [ps->ps' in nfsm] nfsm[ps']
                nfsm[ps] = [final ps in nfsm];
        input_state/initial_state/final_state = s
        main_wanted = (s, symbol)
        null = (s, s)
        token = (s, t, s)

    process:
        news_buffer + queue + olds_buffer
        news+olds = all msgs
        news == queue but offer a fast query
        queue - stable priority i.e. min first; if eq, FIFO
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


import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.Trans.State.Strict


-- type Set = Set.Set
type Map = Map.Map

-- type MinPriorityQueue = Set.Set -- min first
type MinHeap = Set.Set


qput :: Ord a => a -> MinHeap a -> MinHeap a
qput = Set.insert

qget :: Ord a => MinHeap a -> Maybe(a, MinHeap a)
qget = Set.minView













-- mget :: (Ord a, Monad m) => m (MinHeap

-- ParserState
data PS a sym name ps nfsm s t = PS 
    {minq :: MinHeap a
    ,rules :: Map sym (Map name nfsm)
    }

minq_ heap p = p {minq = heap}
minq_with f p = minq_ (f $ minq p) p
rules_with f p = p {rules = f $ rules p}


type M a sym name ps nfsm s t m = StateT (PS a sym name ps nfsm s t) m

pput :: (Monad m, Ord a) => a -> M a sym name ps nfsm s t m ()
pput a = modify . minq_with $ qput a

pget :: (Monad m, Ord a) => M a sym name ps nfsm s t m (Maybe a)
pget = do
    p <- get
    let q = minq p
    case qget q of
        Nothing -> return Nothing
        Just (a, q') -> do
            put (minq_ q' p)
            return (Just a)


newtype NFSM_ nfsm = NFSM_ nfsm
    deriving (Show, Read)
instance Eq (NFSM_ nfsm) where
    _ == _ = True
instance Ord (NFSM_ nfsm) where
    _ `compare` _ = EQ

data Action sym name nfsm s t 
    = Rule sym name (NFSM_ nfsm)
    | InputState s
    | MainWanted s sym
    | Token t
    | Null (s, s)
    deriving (Eq, Show, Ord, Read)

process_step :: (Monad m, Ord sym, Ord name, Ord (Action sym name nfsm s t)) => 
    Action sym name nfsm s t -> M (Action sym name nfsm s t) sym name ps nfsm s t m ()
process_step (Rule sym name (NFSM_ nfsm)) = modify $ \p ->
    rules_with f p
    where
      f = \sym2name2nfsm -> 
        let name2nfsm = Map.findWithDefault Map.empty sym sym2name2nfsm in
        case Map.lookup name name2nfsm of
            Nothing -> Map.insert sym (Map.insert name nfsm name2nfsm) sym2name2nfsm
            _ -> undefined -- error!!

process_step _ = undefined

process_ :: (Monad m, Ord a) => (a->M a sym name ps nfsm s t m ()) -> M a sym name ps nfsm s t m ()
process_ step =  m where 
    m = do
    maybe <- pget
    case maybe of
        Nothing -> return ()
        Just a -> do
            step a
            m -- process_ step

process :: (Monad m, Ord sym, Ord name, Ord (Action sym name nfsm s t)) => 
    M (Action sym name nfsm s t) sym name ps nfsm s t m ()
process = process_ process_step









