
module TH.SeedUtils.MakesFinds
    -- ( module TH.SeedUtils.MakesFinds hiding (range) )
where
import TH.PrintQ
import Language.Haskell.TH as TH
import Language.Haskell.Syntax as Syntax
import TH.SeedUtils.SeedUtils (apps, is_nameBase)
import Numeric.Natural (Natural)

_range :: Natural -> [Natural]
_range 0 = []
_range u = [0..u-1]

-- makeNameBases "a" 3 -> ["a0", "a1", "a2"]
makeNameBases :: String -> Natural -> [String]
makeNameBases prefix = map ((prefix++) . show) . _range
makeNames :: String -> Natural -> [Name]
makeNames prefix = map mkName . makeNameBases prefix


-- VarT's
-- makeTypes "a" 3 = [a0, a1, a2]
makeTypes :: String -> Natural -> [Type]
makeTypes prefix = map VarT . makeNames prefix

-- makeTupleTyEx [a, b, c] = typeof (a, b, c)
makeTupleTyEx :: [Type] -> Type
makeTupleTyEx ts = apps AppT (TupleT u) ts where
    u = length ts
-- makeTupleTy "a" 3 = typeof (a0, a1, a2)
makeTupleTy :: String -> Natural -> Type
makeTupleTy prefix u = makeTupleTyEx $ makeTypes prefix u

-- makeLinkedTupleTyEx [a, b, c] = typeof (a, (b, (c, ())))
makeLinkedTupleTyEx :: [Type] -> Type
makeLinkedTupleTyEx = foldr (AppT . AppT (TupleT 2)) (TupleT 0)
-- makeLinkedTupleTy "a" 3 = typeof (a0, (a1, (a2, ())))
makeLinkedTupleTy :: String -> Natural -> Type
makeLinkedTupleTy prefix u = makeLinkedTupleTyEx $ makeTypes prefix u
-- getLinkedTupleTyArg (typeof (a0, (a1, (a2, ())))) = '[a0, a1, a2]
getLinkedTupleTyArg :: Type -> Type
    --getLinkedTupleTyArg = AppT (ConT ''GetTupleArgs)
getLinkedTupleTyArg = makePromotedTypeList . linkedTupleTy2Types
-- linkedTupleTy2Types (typeof (a0, (a1, (a2, ())))) = [a0, a1, a2]
linkedTupleTy2Types :: Type -> [Type]
linkedTupleTy2Types (AppT (AppT (TupleT 2) h) ts) = h : linkedTupleTy2Types ts
linkedTupleTy2Types _ = [] -- (TupleT 0)
--makePromotedTypeList [a, b, c] = '[a, b, c]
makePromotedTypeList :: [Type] -> Type
makePromotedTypeList = foldr (AppT . AppT PromotedConsT) PromotedNilT


findConTName :: String -> Type -> Bool
findConTName name_base (ConT name) = nameBase name == name_base
findConTName _ _ = False
findConTName__depth :: Natural -> String -> Type -> Bool
findConTName__depth 0 name_base ty = findConTName name_base ty
findConTName__depth u name_base (AppT ty _) =
    findConTName__depth (u-1) name_base ty
findConTName__depth _ _ _ = False

---------------------
-- VarP's
-- makePatterns "a" 3 = [a0, a1, a2]
makePatterns :: String -> Natural -> [Pat]
makePatterns prefix = map VarP . makeNames prefix

makeTuplePatEx, makeLinkedTuplePatEx :: [Pat] -> Pat
makeTuplePatEx = TupP
makeLinkedTuplePatEx = foldr (\a b -> TupP [a,b]) (TupP [])
makeTuplePat, makeLinkedTuplePat :: String -> Natural -> Pat
makeTuplePat prefix = makeTuplePatEx . makePatterns prefix
makeLinkedTuplePat prefix = makeLinkedTuplePatEx . makePatterns prefix



findConPName :: String -> Pat -> Bool
findConPName name_base (ConP name _) = nameBase name == name_base
findConPName _ _ = False
{-
findConPName__depth :: Natural -> String -> Pat -> Bool
findConPName__depth 0 name_base pat = findConPName name_base pat
findConPName__depth u name_base (AppP pat _) =
    findConPName__depth (u-1) name_base pat
-}


-------------------------
makeExprs :: String -> Natural -> [Exp]
makeExprs prefix = map VarE . makeNames prefix

makeTupleExpEx, makeLinkedTupleExpEx :: [Exp] -> Exp
makeTupleExp, makeLinkedTupleExp :: String -> Natural -> Exp
makeTupleExpEx = TupE
makeLinkedTupleExpEx = foldr (\a b -> TupE [a,b]) (TupE [])
makeTupleExp prefix = makeTupleExpEx . makeExprs prefix
makeLinkedTupleExp prefix = makeLinkedTupleExpEx . makeExprs prefix


findConEName :: String -> Exp -> Bool
findConEName name_base (ConE name) = nameBase name == name_base
findConEName _ _ = False
findConEName__depth :: Natural -> String -> Exp -> Bool
findConEName__depth 0 name_base pat = findConEName name_base pat
findConEName__depth u name_base (AppE pat _) =
    findConEName__depth (u-1) name_base pat
findConEName__depth _ _ _ = False




findVarEName :: String -> Exp -> Bool
findVarEName name_base (VarE name) = is_nameBase name_base name
findVarEName _ _ = False
findVarTName :: String -> Type -> Bool
findVarTName name_base (VarT name) = is_nameBase name_base name
findVarTName _ _ = False


