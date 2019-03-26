
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


module Parser.CFGinType__Test_generate
    ( module Parser.CFGinType__Test_generate
    , module Parser.CFGinType
    -- , module Control.Arrow
    )
where

import Parser.CFGinType

import Data.Proxy
import Data.Kind
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Seed.ArrowOps (constA, voidArr)
import ADT.ArrowRandom

data Arr i o
instance Category Arr where
    (.) = undefined
    id = undefined
instance Arrow Arr where
    arr = undefined
    first = undefined
instance ArrowZero Arr where
    zeroArrow = undefined
instance ArrowPlus Arr where
    (<+>) = undefined
instance ArrowRandomDefaultBy Arr where
    type ArrowRandom2By Arr = ()
instance ArrowRandomBy by Arr where
    type Arrow2RandomValueBy by Arr = Integer
    randomByA = undefined
type By = ()

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
    [ Expr ':> String
    , SumE ':> String
    , MulE ':> String
    , Atom ':> String
    , TVal ':> String
    , TAdd ':> String
    , TMul ':> String
    , TOpen ':> String
    , TClose ':> String
    ]
type G = UsrCFG2CFG UG
type D = UsrCFG_N2R2N2R UN2R



stringA :: Arrow arr => String -> arr i String
stringA = constA
{-
stringA_ :: String -> Arr i ()
stringA = mkKleisli . lift . string
stringA_ = voidArr . stringA
-}
instance (G ~ cfg, D ~ n2r) => BuildTerminal By Arr cfg n2r TVal where
    build_terminal _ = randomA >>> arr show
instance (G ~ cfg, D ~ n2r) => BuildTerminal By Arr cfg n2r TAdd where
    build_terminal _ = stringA "+"
instance (G ~ cfg, D ~ n2r) => BuildTerminal By Arr cfg n2r TMul where
    build_terminal _ = stringA "*"
instance (G ~ cfg, D ~ n2r) => BuildTerminal By Arr cfg n2r TOpen where
    build_terminal _ = stringA "("
instance (G ~ cfg, D ~ n2r) => BuildTerminal By Arr cfg n2r TClose where
    build_terminal _ = stringA ")"



--------------------
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    Atom ':= '[TVal]
    ) where
    build_rp2alt _ = unit_alt
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    Atom ':= [TOpen, Expr, TClose]
    ) where
    build_rp2alt _ = fold_altA
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    MulE ':= '[Atom]
    ) where
    build_rp2alt _ = unit_alt
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    MulE ':= [Atom, TMul, MulE]
    ) where
    build_rp2alt _ = fold_altA
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    SumE ':= '[MulE]
    ) where
    build_rp2alt _ = unit_alt
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    SumE ':= [MulE, TAdd, SumE]
    ) where
    build_rp2alt _ = fold_altA
instance (G ~ cfg, D ~ n2r) => BuildRP2Alt By Arr cfg n2r (
    Expr ':= '[SumE]
    ) where
    build_rp2alt _ = unit_alt


instance (G ~ cfg, D ~ n2r) => BuildCFG_Main By Arr cfg n2r where
    type CFG_Main By Arr cfg n2r = Left Expr

_P :: Proxy '(By, Arr, G, D)
_P = Proxy

build :: Arr (i :: *) String
build = build_main _P






--}
--
