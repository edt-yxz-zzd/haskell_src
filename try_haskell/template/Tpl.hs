{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
-- importer needs {-# LANGUAGE TemplateHaskell #-}
-- derive 'x

module Tpl where
import Language.Haskell.TH


-- d2 :: Name -> Name -> DecsQ
d2 :: TypeQ -> TypeQ -> DecsQ
d2 clsq tvq = do
    cls <- clsq
    tv <- tvq
    return [InstanceD [] (AppT cls tv) []]
-- d2 cls tv = [InstanceD [] (AppT cls tv) []]

d0 cls tv = [ClassD [] cls [tv] [] []]

derive = [d| class C a where |]
class C a
d1 = [d| instance C a where |]
printQ q = runQ q >>= print
main = do
    printQ derive
    -- [ClassD [] C_0 [PlainTV a_1] [] []]
    printQ [d| instance C a|]
    -- [InstanceD [] (AppT (ConT Tpl.C) (VarT a_2)) []]

