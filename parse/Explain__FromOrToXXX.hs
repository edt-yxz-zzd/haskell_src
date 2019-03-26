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

module Explain__FromOrToXXX where
import Explain
import SeedUtils__TH (decsQ_add, decQ2decsQ)
import Language.Haskell.TH
{-# LANGUAGE TemplateHaskell #-}
import UInt
import PInt
import DInt


fmap concat . mapM (uncurry def__Ops_FromOrToXXX_ThisOrList) .
    map (\name->(nameBase name, conT name)) $
    [''Bool, ''Ordering, ''Integer, ''Int, ''UInt, ''PInt, ''DInt]

{-
ls = [''Bool, ''Integer, ''Int, ''UInt, ''PInt, ''DInt]
str_typeQ_ls = map f ls where
    f name = (nameBase name, conT name)

fmap concat $ mapM (uncurry def__Ops_FromOrToXXX_ThisOrList) str_typeQ_ls
{-
decsQ_add
    (fmap concat $ mapM (uncurry def__Ops_FromOrToXXX) str_typeQ_ls)
    (fmap concat $ mapM (uncurry def__Ops_FromOrToXXX_List) str_typeQ_ls)
-}

-- def__Ops_FromOrToXXX "Bool" [t| Bool |]
-- def__Ops_FromOrToXXX "IntList" [t| [Int] |]
--def__Ops_FromOrToXXX "Maybe" [t| Maybe x |]



--}
--}
--}
--}
--}
--}
--}
