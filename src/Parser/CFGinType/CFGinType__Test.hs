
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


module Parser.CFGinType.CFGinType__Test
    ( module Parser.CFGinType.CFGinType__Test
    , module Parser.CFGinType.CFGinType
    , module Text.ParserCombinators.ReadP
    --, module Text.Read
    , module Data.Proxy
    -- , module Control.Arrow
    )
where

import Parser.CFGinType.CFGinType

import Data.Proxy
import Data.Kind
import Control.Arrow
import Seed.ArrowOps (constA, voidArr)
import Text.ParserCombinators.ReadP
--import Text.ParserCombinators.ReadPrec
import Text.Read



type By = ()
type Arr = Kleisli ReadPrec

{-
    expr = sum
    sum = mul '+' sum
    sum = mul
    mul = atom '*' mul
    mul = atom
    atom = val
    atom = '(' expr ')'

-}
data Expr
data SumE
data MulE
data Atom
data TVal
data TAdd
data TMul
data TOpen
data TClose
type UG = 'UsrCFG [TVal, TAdd, TMul, TOpen, TClose]
    [ Expr ':= '[SumE]
    , SumE ':= [MulE, TAdd, SumE]
    , SumE ':= '[MulE]
    , MulE ':= [Atom, TMul, MulE]
    , MulE ':= '[Atom]
    , Atom ':= '[TVal]
    , Atom ':= [TOpen, Expr, TClose]
    ]
type UN2R = 'UsrCFG_N2R UG
    [ Expr ':> (Integer, String)
    , SumE ':> Integer
    , MulE ':> Integer
    , Atom ':> Integer
    , TVal ':> Integer
    , TAdd ':> ()
    , TMul ':> ()
    , TOpen ':> ()
    , TClose ':> ()
    ]
type G = UsrCFG2CFG UG
type D = UsrCFG_N2R2N2R UN2R

{-
class BuildTerminalH by arr cfg n2r a where
    build_terminalH
        :: (cfg ~ Proxy cfg', n2r ~ Proxy n2r')
        => proxy (by, Proxy arr, cfg, n2r, a)
        -> arr i (LookupE (Right a) n2r')
instance BuildTerminalH By Arr (Proxy G) (Proxy D) TVal where
    build_terminalH _ = Kleisli $ const readPrec
-}

mkKleisli :: m o -> Kleisli m i o
mkKleisli = Kleisli . const
stringA :: String -> Arr i String
stringA_ :: String -> Arr i ()
stringA = mkKleisli . lift . string
stringA_ = voidArr . stringA
instance (G ~ cfg, D ~ n2r) => BuildTerminal By Arr cfg n2r TVal where
    build_terminal _ = Kleisli $ const readPrec
instance (G ~ cfg, D ~ n2r) => BuildTerminal By Arr cfg n2r TAdd where
    build_terminal _ = stringA_ "+"
instance (G ~ cfg, D ~ n2r) => BuildTerminal By Arr cfg n2r TMul where
    build_terminal _ = stringA_ "*"
instance (G ~ cfg, D ~ n2r) => BuildTerminal By Arr cfg n2r TOpen where
    build_terminal _ = stringA_ "("
instance (G ~ cfg, D ~ n2r) => BuildTerminal By Arr cfg n2r TClose where
    build_terminal _ = stringA_ ")"


unit_alt :: Arrow arr => arr (a, ()) a
unit_alt = arr fst
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    Atom ':= '[TVal]
    ) where
    build_rp2alt _ = unit_alt
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    Atom ':= [TOpen, Expr, TClose]
    ) where
    build_rp2alt _ = arr $ fst . fst . snd
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    MulE ':= '[Atom]
    ) where
    build_rp2alt _ = unit_alt
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    MulE ':= [Atom, TMul, MulE]
    ) where
    build_rp2alt _ = arr f where
        f (lhs, ((), (rhs, ()))) = lhs * rhs
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    SumE ':= '[MulE]
    ) where
    build_rp2alt _ = unit_alt
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    SumE ':= [MulE, TAdd, SumE]
    ) where
    build_rp2alt _ = arr f where
        f (lhs, ((), (rhs, ()))) = lhs + rhs
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    Expr ':= '[SumE]
    ) where
    build_rp2alt _ = arr f where
        f (i, ()) = (i, show i)


instance (G ~ cfg, D ~ n2r) => BuildCFG_Main By Arr cfg n2r where
    type CFG_Main By Arr cfg n2r = Left Expr

_P :: Proxy '(By, Arr, G, D)
_P = Proxy

build :: ReadP (Integer, String)
build = flip readPrec_to_P 3 $ runKleisli (build_main _P) ()
parse = readP_to_S build

{-
*Main> parse "1+2*-3+3*(1+2)"
[((1,"1"),"+2*-3+3*(1+2)"),((3,"3"),"*-3+3*(1+2)"),((-5,"-5"),"+3*(1+2)"),((-2,"-2"),"*(1+2)"),((4,"4"),"")]
-}



