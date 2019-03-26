{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}


module Explain.FromOrToXXX
where
import IntDefs.IntDefs (UInt, PInt, QInt)

import Seed.PairOps (a_pairs_ls2triples)
import Explain.FromOrToXXX_tpl (defs__easy, _ConTName2conty_str_pair)
import TH.SeedUtils (Type (ConT), def__instances)

$(def__instances . defs__easy $ a_pairs_ls2triples [
    (0, [
        --(''Bool, "Bool")
    ] ++ map _ConTName2conty_str_pair [
        ''Bool, ''Ordering, ''Integer, ''Int, ''UInt, ''PInt, ''QInt
    ]
    )
    , ( 1, map _ConTName2conty_str_pair [
        ''Maybe
    ] ++ [
        (ConT ''[], "List")
    ])
    , ( 2, map _ConTName2conty_str_pair [
        ''Either
    ] ++ [
        (ConT ''(,), "Pair")
    ])
 ])

