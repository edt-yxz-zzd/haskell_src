{-# LANGUAGE TemplateHaskell #-}

module NumConcept
    ( Concept
    , NumConcept
    , ComplexConcept
    , RealConcept
    , RationalConcept
    , IntegralConcept
    , UIntConcept
    , DIntConcept
    , PIntConcept
    , inst_num_concepts
    )
where

import Language.Haskell.TH
class Concept a
class Concept a => NumConcept a
class NumConcept a => ComplexConcept a
class ComplexConcept a => RealConcept a
class RealConcept a => RationalConcept a
class RationalConcept a => IntegralConcept a
class IntegralConcept a => UIntConcept a -- NaturalConcept
class IntegralConcept a => DIntConcept a
class (UIntConcept a, DIntConcept a) => PIntConcept a


inst00 :: TypeQ -> DecQ
inst00 tq = instanceD (return []) tq []
inst1__ls :: [Name] -> TypeQ -> DecsQ
-- inst1__ls names tq = sequence $ map (flip inst1 tq) names
inst1__ls names tq = mapM (flip inst1 tq) names
inst1 :: Name -> TypeQ -> DecQ
inst1 name tq = inst00 $ do
    t <- tq
    return $ flip AppT t $ ConT name

decQ2decsQ :: DecQ -> DecsQ
decQ2decsQ = fmap return
inst1_top :: Name -> TypeQ -> DecsQ
inst1_top cls_name = decQ2decsQ . inst1 cls_name


instConcept :: TypeQ -> DecsQ
instConcept = inst1_top ''Concept
[instNumConcept, instComplexConcept] =
    map inst1_top [''NumConcept, ''ComplexConcept]
-- [Concept, NumConcept, ComplexConcept, RealConcept, RationalConcept, IntegralConcept, UIntConcept, DIntConcept, PIntConcept]
concept_names = [''Concept, ''NumConcept, ''ComplexConcept, ''RealConcept, ''RationalConcept, ''IntegralConcept, ''UIntConcept, ''DIntConcept, ''PIntConcept]

inst_num_concepts :: Name -> TypeQ -> DecsQ
inst_num_concepts name = r where
    names = name : takeWhile (name /=) concept_names
    r = if not $ elem name concept_names then fail "unknown concept"
        else inst1__ls names



