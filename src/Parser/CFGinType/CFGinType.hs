

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}

{-
    Proxy (
        [ A := B :* C
        , C := E :F
        ]
    )
    class Build by (A := ...)
-}
module Parser.CFGinType.CFGinType
    ( module Seed.Kind
    , module Seed.FoldAlt
    , BuildTerminal(..)
    , BuildRP2Alt(..)
    , BuildCFG_Main(..)
    , BuildCFG(..)
    , BuildSymbol(..)
    , UsrCFG(..)
    , UsrCFG_N2R(..)
    , UsrCFG2CFG(..)
    , UsrCFG_N2R2N2R(..)
    , UsrCFG_withNoise(..)
    )
where

import Data.Proxy
import Data.Kind
import Control.Arrow
import Seed.ArrowOps (constA, voidArr)
import Seed.Kind
--import Text.ParserCombinators.ReadP
--import Text.ParserCombinators.ReadPrec
--import Text.Read

import Seed.FoldAlt



type family RenameSymbol (tokens :: [x]) (a :: x) = (r :: EitherX x) where
    RenameSymbol '[] a = Left a -- nonterminal
    RenameSymbol (h ': ts) h = Right h -- terminal
    RenameSymbol (h ': ts) a = RenameSymbol ts a
type family RenameSymbols (tokens :: [x]) (ls :: [x]) = (r :: [Either x x]) where
    RenameSymbols s '[] = '[]
    RenameSymbols s (h ': ts) = RenameSymbol s h ': RenameSymbols s ts
type family RenameAlt (tokens :: [x]) (a :: x := y) = (r :: x ::= y) where
    RenameAlt s (lhs ':= rp) = RenameSymbol s lhs ':= RenameSymbols s rp
type family RenameAlts (tokens :: [x]) (alts :: [x := y]) = (r :: [x ::= y]) where
    RenameAlts s '[] = '[]
    RenameAlts s (h ': ts) = RenameAlt s h ': RenameAlts s ts
type RenameCFG ts cfg = RenameAlts ts (CFG2Alts cfg)
type family RenameResultMap (tokens :: [x]) (dict :: [x :> y])
    = (r :: [x ::> y]) where
    RenameResultMap s '[] = '[]
    RenameResultMap s (h ':> r ': ts) =
        RenameSymbol s h ':> r ': RenameResultMap s ts


type Tokens symbol = [symbol]
type CFG symbol = (Tokens symbol, Alts symbol)
type Alt symbol = symbol ::= symbol
type Alts symbol = [Alt symbol]
type N2R symbol = [symbol ::> *]
type Symbol symbol = EitherX symbol
type RP symbol = [Symbol symbol]
type family Alt2NonTerminal (alt :: Alt n) = (r :: Symbol n) where
    Alt2NonTerminal (lhs ':= rp) = lhs
type family Alt2RP (alt :: Alt n) = (r :: RP n) where
    Alt2RP (lhs ':= rp) = rp
type family UsrAlt2LHS (alt :: UsrAlt n) = (r :: UsrSymbol n) where
    UsrAlt2LHS (lhs ':= rp) = lhs
type family UsrAlt2UsrRP (alt :: UsrAlt n) = (r :: UsrRP n) where
    UsrAlt2UsrRP (lhs ':= rp) = rp
{-
type family CFG2Alts (cfg :: CFG n) = (r :: Alts n) where
    CFG2Alts '(ts, alts) = alts
-}
type CFG2Tokens cfg = PairFst cfg
type CFG2Alts cfg = PairSnd cfg

{-
type family AltsOf (a :: Symbol n) (cfg :: CFG n) = (r :: Alts n) where
    AltsOf a (ts, alts) = AltsOf_ a alts
-}
type AltsOf a cfg = AltsOf_ a (CFG2Alts cfg)
type family AltsOf_ (a :: Symbol n) (alts :: Alts n) = (r :: Alts n) where
    AltsOf_ a '[] = '[]
    AltsOf_ a ((a ':= rp) ': ts) = (a ':= rp) ': AltsOf_ a ts
    AltsOf_ a ((x ':= rp) ': ts) = AltsOf_ a ts


type family Alt2AllSymbols (alt :: Alt n) = (r :: [Symbol n]) where
    Alt2AllSymbols (lhs ':= rp) = lhs ': rp
type family Alts2AllSymbols (alts :: Alts n) = (r :: [Symbol n]) where
    Alts2AllSymbols '[] = '[]
    Alts2AllSymbols (alt ': alts) =
        ListAdd (Alt2AllSymbols alt) (Alts2AllSymbols alts)
type CFG2AllSymbols cfg = Alts2AllSymbols (CFG2Alts cfg)



-------------
type UsrSymbol symbol = symbol
type UsrRP symbol = [UsrSymbol symbol]
type UsrAlt symbol = symbol := symbol
type UsrAlts symbol = [UsrAlt symbol]
type family Symbol2UsrSymbol (s :: Symbol n) = (r :: UsrSymbol n) where
    Symbol2UsrSymbol (Left a) = a
    Symbol2UsrSymbol (Right a) = a
type family RP2UsrRP (s :: RP n) = (r :: UsrRP n) where
    RP2UsrRP '[] = '[]
    RP2UsrRP (h ': ts) = Symbol2UsrSymbol h ': RP2UsrRP ts
type family Alt2UsrAlt (alt :: Alt n) = (r :: UsrAlt n) where
    Alt2UsrAlt (h ':= rp) = Symbol2UsrSymbol h ':= RP2UsrRP rp
--type Alt2UsrRP alt = RP2UsrRP (Alt2RP alt)


--------------------------
class ArrowPlus arr =>
    BuildTerminal by arr
        (cfg :: CFG n) (n2r :: N2R n) (a :: n) where
    build_terminal
        :: proxy '(by, arr, cfg, n2r, a)
        -> arr i (LookupE (Right a) n2r)



class (alt ~ (UsrAlt2LHS alt ':= UsrAlt2UsrRP alt))
    => IsUsrAlt (alt :: UsrAlt n) where
instance (alt ~ (UsrAlt2LHS alt ':= UsrAlt2UsrRP alt))
    => IsUsrAlt (alt :: UsrAlt n) where
class (alt ~ (Alt2NonTerminal alt ':= Alt2RP alt))
    => IsAlt (alt :: Alt n) where
instance (alt ~ (Alt2NonTerminal alt ':= Alt2RP alt))
    => IsAlt alt where

class (ArrowPlus arr, IsUsrAlt usr_alt)
    => BuildRP2Alt by arr
        (cfg :: CFG n) (n2r :: N2R n) (usr_alt :: UsrAlt n) where
    build_rp2alt
        :: (usr_alt ~ (lhs ':= usr_rp)
            , rp ~ RenameSymbols ts usr_rp, ts ~ CFG2Tokens cfg)
        => proxy '(by, arr, cfg, n2r, usr_alt)
        -> arr (LookupLs2Linked rp n2r) (LookupE (Left lhs) n2r)
class (ArrowPlus arr, BuildRP by arr cfg n2r (Alt2RP alt), IsAlt alt
    , BuildRP2Alt by arr cfg n2r (Alt2UsrAlt alt)
    , LookupLs2Linked
        (RenameSymbols (CFG2Tokens cfg) (UsrAlt2UsrRP (Alt2UsrAlt alt)))
        n2r
        ~ LookupLs2Linked (Alt2RP alt) n2r
    , Alt2NonTerminal alt ~ Left (UsrAlt2LHS (Alt2UsrAlt alt))
    ) =>
    BuildAlt by arr
        (cfg :: CFG n) (n2r :: N2R n) (alt :: Alt n) where
    build_alt
        :: (alt ~ (lhs ':= Alt2RP alt))
        => proxy '(by, arr, cfg, n2r, alt)
        -> arr i (LookupE lhs n2r)
    build_alt p = build_rp prp >>> build_rp2alt p2 where
        prp = build_alt__proxy_cast_rp p
        p2 = build_alt__proxy_cast_rp2alt p
    {-
    rp2alt
        :: (alt ~ (lhs ':= rp))
        => proxy '(by, arr, cfg, n2r, alt)
        -> arr (LookupLs2Linked rp n2r) (LookupE lhs n2r)
    {-# MINIMAL rp2alt #-}
    -}
instance (ArrowPlus arr, BuildRP by arr cfg n2r (Alt2RP alt), IsAlt alt
    , BuildRP2Alt by arr cfg n2r (Alt2UsrAlt alt)
    , LookupLs2Linked
        (RenameSymbols (CFG2Tokens cfg) (UsrAlt2UsrRP (Alt2UsrAlt alt)))
        n2r
        ~ LookupLs2Linked (Alt2RP alt) n2r
    , Alt2NonTerminal alt ~ Left (UsrAlt2LHS (Alt2UsrAlt alt))
    ) =>
    BuildAlt by arr cfg n2r alt

build_alt__proxy_cast_rp
    :: proxy '(by, arr, cfg, n2r, lhs ':= rp)
    -> Proxy '(by, arr, cfg, n2r, rp)
build_alt__proxy_cast_rp2alt
    :: (alt ~ (lhs ':= rp))
    => proxy '(by, arr, cfg, n2r, alt)
    -> Proxy '(by, arr, cfg, n2r, Alt2UsrAlt alt)
build_alt__proxy_cast_rp _ = Proxy
build_alt__proxy_cast_rp2alt _ = Proxy
class ArrowPlus arr =>
    BuildRP by arr
        (cfg :: CFG n) (n2r :: N2R n) (rp :: RP n) where
    build_rp
        :: proxy '(by, arr, cfg, n2r, rp)
        -> arr i (LookupLs2Linked rp n2r)
instance ArrowPlus arr => BuildRP by arr cfg n2r '[] where
    build_rp _ = constA ()
instance (ArrowPlus arr, BuildRP by arr cfg n2r rp
    , BuildSymbol by arr cfg n2r lhs
    )
    => BuildRP by arr cfg n2r (lhs ': rp) where
    build_rp p = build_symbol pl &&& build_rp prp where
        pl = build_rp__proxy_cast_symbol p
        prp = build_rp__proxy_cast_rp p

build_rp__proxy_cast_symbol
        :: proxy '(by, arr, cfg, n2r, lhs ': rp)
        -> Proxy '(by, arr, cfg, n2r, lhs)
build_rp__proxy_cast_rp
        :: proxy '(by, arr, cfg, n2r, lhs ': rp)
        -> Proxy '(by, arr, cfg, n2r, rp)
build_rp__proxy_cast_rp _ = Proxy
build_rp__proxy_cast_symbol _ = Proxy




class ArrowPlus arr =>
    BuildNonTerminal by arr
        (cfg :: CFG n) (n2r :: N2R n) (a :: n) where
    build_nonterminal
        :: proxy '(by, arr, cfg, n2r, a)
        -> arr i (LookupE (Left a) n2r)
instance (ArrowPlus arr, BuildNonTerminal_ by arr cfg n2r alts_of_a a
    , alts_of_a ~ AltsOf (Left a) cfg
    ) => BuildNonTerminal by arr cfg n2r a where
    build_nonterminal = build_nonterminal_ . build_nonterminal__proxy_cast_altsOf
build_nonterminal__proxy_cast_altsOf
        :: proxy '(by, arr, cfg, n2r, a)
        -> Proxy '(by, arr, cfg, n2r, AltsOf (Left a) cfg, a)
build_nonterminal__proxy_cast_altsOf _ = Proxy
class ArrowPlus arr =>
    BuildNonTerminal_ by arr
        (cfg :: CFG n) (n2r :: N2R n) (alts_of_a :: Alts n) (a :: n) where
    build_nonterminal_
        :: proxy '(by, arr, cfg, n2r, alts_of_a, a)
        -> arr i (LookupE (Left a) n2r)
instance ArrowPlus arr => BuildNonTerminal_ by arr cfg n2r '[] a where
    build_nonterminal_ _ = zeroArrow
instance (ArrowPlus arr, BuildAlt by arr cfg n2r alt
    , BuildNonTerminal_ by arr cfg n2r alts a
    , Alt2NonTerminal alt ~ Left a
    )
    => BuildNonTerminal_ by arr cfg n2r (alt ': alts) a where
    build_nonterminal_ pa = h <+> t where
        h = build_alt $ build_nonterminal__proxy_cast_alt pa
        t = build_nonterminal_ $ build_nonterminal__proxy_cast_alts pa
build_nonterminal__proxy_cast_alt
        :: proxy '(by, arr, cfg, n2r, alt ': alts, a)
        -> Proxy '(by, arr, cfg, n2r, alt)
build_nonterminal__proxy_cast_alts
        :: proxy '(by, arr, cfg, n2r, alt ': alts, a)
        -> Proxy '(by, arr, cfg, n2r, alts, a)
build_nonterminal__proxy_cast_alt _ = Proxy
build_nonterminal__proxy_cast_alts _ = Proxy




class ArrowPlus arr =>
    BuildSymbol by arr
        (cfg :: CFG n) (n2r :: N2R n) (a :: Symbol n) where
    build_symbol
        :: proxy '(by, arr, cfg, n2r, a)
        -> arr i (LookupE a n2r)

instance (ArrowPlus arr, BuildNonTerminal by arr cfg n2r a)
    => BuildSymbol by arr cfg n2r (Left a) where
    build_symbol = build_nonterminal . build_symbol__proxy_cast_left
instance (ArrowPlus arr, BuildTerminal by arr cfg n2r a)
    => BuildSymbol by arr cfg n2r (Right a) where
    build_symbol = build_terminal . build_symbol__proxy_cast_right
build_symbol__proxy_cast_left
        :: proxy '(by, arr, cfg, n2r, Left a)
        -> Proxy '(by, arr, cfg, n2r, a)
build_symbol__proxy_cast_right
        :: proxy '(by, arr, cfg, n2r, Right a)
        -> Proxy '(by, arr, cfg, n2r, a)
build_symbol__proxy_cast_left _ = Proxy
build_symbol__proxy_cast_right _ = Proxy





-----------------------
class (ArrowPlus arr, BuildCFG_ by arr cfg n2r (CFG2AllSymbols cfg)) =>
    BuildCFG by arr (cfg :: CFG n) (n2r :: N2R n)
instance (ArrowPlus arr, BuildCFG_ by arr cfg n2r (CFG2AllSymbols cfg)) =>
    BuildCFG by arr cfg n2r
class ArrowPlus arr =>
    BuildCFG_ by arr (cfg :: CFG n) (n2r :: N2R n) (symbols :: [Symbol n])
instance ArrowPlus arr => BuildCFG_ by arr cfg n2r '[]
instance (ArrowPlus arr, BuildCFG_ by arr cfg n2r ts
    , BuildSymbol by arr cfg n2r h
    ) => BuildCFG_ by arr cfg n2r (h ': ts)


class (ArrowPlus arr, BuildCFG by arr cfg n2r
    , BuildSymbol by arr cfg n2r (CFG_Main by arr cfg n2r)
    ) => BuildCFG_Main by arr (cfg :: CFG n) (n2r :: N2R n) where
    type CFG_Main by arr cfg n2r :: Symbol n
    build_main
        :: proxy '(by, arr, cfg, n2r)
        -> arr i (LookupE (CFG_Main by arr cfg n2r) n2r)
    build_main = build_symbol . build_main__proxy_cast_symbol
build_main__proxy_cast_symbol
        :: proxy '(by, arr, cfg, n2r)
        -> Proxy '(by, arr, cfg, n2r, CFG_Main by arr cfg n2r)
build_main__proxy_cast_symbol _ = Proxy





-----------------------
data UsrCFG n = UsrCFG { usrTokens :: [n], usrAlts :: [n := n]}
data UsrCFG_N2R n = UsrCFG_N2R {usrCFG :: UsrCFG n, usrN2R :: [n :> *]}
type family UsrCFG2CFG (usr :: UsrCFG n) = (cfg :: CFG n) where
    UsrCFG2CFG ('UsrCFG ts alts) = '(ts, RenameAlts ts alts)
type family UsrCFG_N2R2N2R (usr :: UsrCFG_N2R n) = (n2r :: N2R n) where
    UsrCFG_N2R2N2R ('UsrCFG_N2R ('UsrCFG ts alts) n2r) = RenameResultMap ts n2r

type family UsrCFG_withNoise (space :: n) (usr :: UsrCFG n) = (r :: UsrCFG n) where
    UsrCFG_withNoise space ('UsrCFG ts alts) = 'UsrCFG (space ': ts)
        (UsrAlts_withNoise space alts)
type family UsrAlts_withNoise (space :: n) (usr :: UsrAlts n) = (r :: UsrAlts n) where
    UsrAlts_withNoise s '[] = '[]
    UsrAlts_withNoise s (h ': ts) =
        UsrAlt_withNoise s h ': UsrAlts_withNoise s ts
type family UsrAlt_withNoise (space :: n) (usr :: UsrAlt n) = (r :: UsrAlt n) where
    UsrAlt_withNoise s (lhs ':= rp) = lhs ':= UsrRP_withNoise s rp
type family UsrRP_withNoise (space :: n) (usr :: UsrRP n) = (r :: UsrRP n) where
    UsrRP_withNoise s '[] = '[]
    UsrRP_withNoise s (h ': ts) =
        s ': h ': UsrRP_withNoise s ts


-----------------------

{-
-}


--}
--}
--}
--}
--}
--}
