{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- ~ can not be used in "class" decl???
{-# LANGUAGE TypeFamilies #-} -- for ~ Equality constraints
{-# LANGUAGE GADTs #-} -- for ~ Equality constraints

--- 
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}




{-

Grammar ::= [Rule] -- countable [Rule]
Rule = ([n], [sym]) -- not null [n] -- finite [n] [sym]

sep Grammar into LeftPart and RightPart (two automa)

-}


import SeedUtils 
    ( just, merge_lsls, gen_list, may2ls, direct_product2
    , followButFinal, follow_closure, follow_closureBut
    , filter_not, justif)
-----
import Container
import NonNullList
import Formal
import Boxed



type Symbol = Either -- sym = Either n t
type Rule n t = (NonNullList n, [Symbol n t])
f :: ( Eq n, Eq t, Iterable rule g
      -- , sym~Symbol n t
      , rule~Rule n t
      ) => (n, t, g)
f = undefined
class ( Eq n, Eq t
      --, Iterable (Rule n t) g
      -- l_t == n -- left_part -> n+
      , SentenceGenerator l_a l_n n lp
      -- r_t == Symbol n t -- right_part -> sym*
      , SentenceGenerator r_a r_n (Symbol n t) rp
      , StartNonterminals n g
      ) -- left_part right_part be automa
    => Grammar l_a l_n lp r_a r_n rp n t g
    | g -> n t lp rp where
    -- rules = iter
    left_part :: (?grammar :: g) => lp
    right_part :: (?grammar :: g) => rp
    left2right :: (?grammar :: g) => l_a -> [GConf r_a r_n (Symbol n t)]

    -- rulesEx :: (?grammar :: g) => [Maybe (Rule n t)]
    rules :: (?grammar :: g) => [Rule n t]
    rules = merge_lsls lsls where
        lefts_ex_ls = withFormal left_part generateEx
        lsls = [ [ (mkNN n ns, syms) | (_, syms) <- withFormal right_part
                    $ generateFromEx $ left2right l_a]
               | (l_a, n:ns) <- lefts_ex_ls]
withGrammar :: g -> ((?grammar :: g) => a) -> a
withGrammar g a = let ?grammar = g in a



{-
x :: lp_conf -> n -> [lp_conf]
y :: (reversed_ns, lp_conf) -> n -> [(rv_ns, lp_conf)]
z :: (rv_ns, lp_conf) -> Maybe (rv_ns, l_a)
z' :: (rv_ns, lp_conf) -> [(rv_ns, syms)]
-}


{-
class SGrammar n t g => T g | g -> n t
class -- Grammar l_a l_n lp r_a r_n rp n t g => 
    SGrammar l_a l_conf n t g
instance Grammar l_a l_n lp r_a r_n rp n t g => SGrammar l_a () n t g

-}

newtype ParseGrammar g n t = ParseGrammar { unParseGrammar :: g }
    deriving (Show, Read, Eq, Ord)
type P = ParseGrammar
instance Boxed g (P g n t) where
    box = mkP
    unbox = unParseGrammar
mkP = ParseGrammar



type Tk n t = Symbol n t
type St n t = ([n], [Rule n t], [t])
type Conf n t = (St n t, [Tk n t])
instance ( Eq t, Eq n, Grammar l_a l_n lp r_a r_n rp n t g
         -- , l_conf~GConf l_a l_n n
         )
    => Formal (St n t) (Tk n t) (P g n t) where
    -- RPN - Reverse Polish Notation
    -- st = (rpn_rules, parse_input) :: ([rule], [t])
    -- conf = (st, [sym]) where sym ~ tk
    -- tk v.s. t : tk is Formal token, t is Parser terminal

    -- bug: not [0..1] rules but [0..] rules
    -- step ts (Left n) = may2ls . fmap ((,) ts) $
    --    lookup n . rules $ unbox ?sys
    {-
    stepMay ( rv_ns, reversed_rules, ts) (Left n)
        = Just ((n:rv_ns, reversed_rules, ts), []) :
        [ justif (iter ns' == ns)
            (([], rule:reversed_rules, ts), rights)
        -- what if infinite mismatch??
        --  should use [Maybe a] instead of [a]??
        | rule@(ns', rights) <- withGrammar (unbox ?sys) rules
        ] where
        ns = reverse rv_ns

    stepMay ([], rs, t0:ts) (Right t1) | t0 == t1 =
        [Just (([], rs, ts), [])]
    stepMay _ _ = []
    -}
    step (-- l_conf@(l_st@(l_a, l_rv_ns), l_sym:l_syms),
           rv_ns, reversed_rules, ts) (Left n) -- | l_sym == Right n
        = ((n:rv_ns, reversed_rules, ts), []) :
        [ (([], rule:reversed_rules, ts), rights)
        -- what if infinite mismatch??
        --  should use [Maybe a] instead of [a]??
        | rule@(ns', rights) <- withGrammar (unbox ?sys) rules, iter ns' == ns
        ] where
        ns = reverse rv_ns
        {-
        l_t = n
        l_tk = Right n
        l_sym = l_tk
        yields l_conf
        t = Right n -- Right n, n == l_t
        -}

    step ([], rs, t0:ts) (Right t1) | t0 == t1 =
        [(([], rs, ts), [])]
    step _ _ = []

    --}

--import SeedUtils (merge_lsls)

{-
-- finite -> countable -- impossible if len finite >= 2
products :: [a] -> [[a]]
products [] = []
products finite_a = countable_ls where
    -- bug: countable_ls = [a:ls | ls <- countable_ls, a <- finite_a]
    countable_ls = ?
    countable_lsls = map (\a -> map (a:) countable_ls) finite_a
-}

{-
-- left_part = undefined
-- right_part = undefined
initials = undefined
terminals = undefined
step = undefined

default_rules :: Grammar n t g lp rp => g -> [Rule n t]
default_rules g = undefined where
    lp = left_part g
    rp = right_part g
    ss = initials lp
    ns = terminals lp -- finite! -- left terminal is g nonterminal
    -- bug: nss = products ns

    -- left_step :: left_st -> n -> [(left_st, [n])]
    -- rpn == reversed [n]
    -- left_step :: (rpn, left_st, [n]) -> [(rpn, left_st, [n])]
    left_step (rpn, st, []) = merge_lsls
        [[(n:rpn, st', ns') | (st', ns') <- step st n] | n <- ns]
    left_step (rpn, st, n:ns) = [(n:rpn, st', ns'++ns) | (st', ns') <- step st n]
    left_stepss = follow_closure left_step where
        -- no! quit iff step->[]
        is_final (_, _, []) = True
        is_final _ = False
    confs0 = merge_lsls $ direct_product2 step ss ns





-- -}
-- -}
-- -}
-- -}


