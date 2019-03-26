
{-

right linear
    St = a b c St'

Rule sym st = (st, [sym], Maybe st)

-}

import SeedUtils

class NDFA_Rules sym a where
    is_terminal :: a -> sym -> Bool
        -- we may mark some nonterminals to be terminal
        -- so do not distinguish st/sym types
    rules :: a -> [(sym, [sym])]

class NDFA_Rules sym a => DynNDFA_Rules sym a where
class NDFA_Rules sym a => OpMarkAsTerminal sym a where
    mark_as_terminal :: a -> sym -> a
class NDFA_Rules sym a => OpRulesAt sym a where
    rules_at :: a -> sym -> [[sym]]
class NDFA_Rules sym a => OpAllNonterminals sym a where
    all_nonterminals :: a -> [sym]




--          ndfa rules -> state in result -> state2regex
ndfa2regex :: ndfa -> [st] -> Map st regex
ndfa2regex rules target_sts =
    -- g = digraph from edges [(l->r) | (l->rp) <- rules, r <- nub rs]
    g = digraph_from_rules rules
    -- scc : strong connected component
    -- sts_ls = [[st]]
    -- [sts_ls == [..[=a=]..[=b=]..] -->> not (a->b)
    sts_ls = reversed_topological_ordering__scc g

    for sts in sts_ls:
        if not is_linear_grammar ndfa sts:
            error "bad format"
        -- st = ... st' ==> st = (init) st'  if st' in sts
        -- st = ...     ==> st = (init)      otherwise
        -- or: left linear
        --      st = st' ... ==> st = (tail) st'
        --      st = ... ==> st = (tail)
        (is_right, rlg) = to_right_linear_grammar_only ndfa sts
        st2re = right_linear_grammar2regex rlg
        if not is_right:
            st2re = M.map re_reverse st2re
        add st2re to result
    for st in rules:
        if st not in result:
            -- rules_at
            add {st : (rp[st][0] | rp[st][1] | ...)} to result
to_right_linear_grammar_only :: ndfa -> [st] -> (Bool, ndfa)
    -- keys result <= target
right_linear_grammar2regex :: ndfa -> Map st regex
right_linear_grammar2regex rlg =
    g = digraph_from_rules rlg
    -- first one will be the first removed
    -- i.e. the result regex of it will not contain others
    sts = find_out_sts_removed_to_be_DAG g -- will be output keys
    if null sts:
        return to_dict ndfa
    sts' = take 1 sts -- at most one; other will be process when recur
    -- st in sts ==>> I[st] = st
    rlg' = add_init_rules rlg sts'
    stst_init_ls = [((st,st'), init) | (st, init, st') <- rlg']
    stst2inits = group stst_init_ls :: Map (st, st') [init]
    stst2regex = M.map Alt stst2inits
    -- all nonterminals
    for mid in keys ndfa:
        stst2regex' = {}
        -- R(mid->mid)*
        mid_mid_star = Star $ get re_dead (mid, mid) stst2regex
        beg_mid2re = M.filter_key (_, mid) stst2regex -- R(beg->mid)
        mid_end2re = M.filter_key (mid, _) stst2regex -- R(mid->end)
        for (beg, _), first in beg_mid2re.items():
            for (_, end), last in mid_end2re.items():
                -- R(beg->end)
                beg_end_re = get re_dead (beg, end) stst2regex
                add stst2regex' (beg, end)
                    $ Alt [beg_end_re, Con[first, mid_mid_star, last]]

    [st] = sts'
    re = get (I[st], F) last_stst2regex'
    -- mark_as_terminal
    st2re = ndfa2regex (ndfa\\st) (sts\\st)
    add st2re st re
    return st2re






type RuleR sym st = (st, [sym], Maybe st)

data Regex' t
    = Sym' t
    | Con' [Regex' t]
    | Alt' [Regex' t]
    | Star' (Regex' t)
    deriving (Eq, Ord, Show, Read)

type Out sym st = RuleR sym st

rules2dict :: Ord st => [RuleR sym st] -> Map st [RuleR sym st]
rules2dict rules = group $ map uncurry rules
rule2regex rule@(st, syms, st') = Sym' rule
ndfa2regex :: Ord st => [RuleR sym st] -> Set st -> Regex' (Out sym st)
ndfa2regex rules outs =
    keys' = keys outs
    mapKeys Left rules2dict
    -- R(Ist -> st, 0) = ""; F = Nothing
    update [(Right st, (Right st, [], Just $ Left st)) | st <- S.toList outs]

    remove keys' rules
    remove key rules


