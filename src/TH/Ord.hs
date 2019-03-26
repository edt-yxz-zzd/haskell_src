{-# LANGUAGE TemplateHaskell #-}
module TH.Ord where
import Language.Haskell.TH
import TH.SeedUtils (decsQ_add)

instanceEq__EQ :: TypeQ -> DecsQ
instanceEq__EQ tq = [d|
    instance Eq $tq where
        _ == _ = True
    |]
instanceOrd__EQ :: TypeQ -> DecsQ
instanceOrd__EQ tq = [d|
    instance Eq $tq => Ord $tq where
        compare a b = EQ
        a < b = False
        a > b = False
        a <= b = True
        a >= b = True
        min = const
        max = const
    |]

-- with Eq or not
instanceOrd__EQ_ex :: Bool -> TypeQ -> DecsQ
instanceOrd__EQ_ex False = instanceOrd__EQ
instanceOrd__EQ_ex True = f where
    f tq = decsQ_add (instanceEq__EQ tq) (instanceOrd__EQ tq)




