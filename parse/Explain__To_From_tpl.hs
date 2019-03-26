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


module Explain__To_From_tpl
    ( module Explain__To_From_tpl
    -- , module TH
    )
where
import Explain
import PrintQ
import Language.Haskell.TH as TH
import SeedUtils__TH

-- import XXX
data XXX
main = do
    printLnQ $ to_from_tpl --  to_from "XXX" -- :main
    pprintLnQ to_from_tpl
    printLnQ to_from_tpl0
    let type_ls_b = appT listT $ newName "b" >>= varT
        clsD_Q_ex =
            -- b <- newName "b"
            -- let type_ls_b = AppT ListT $ VarT b
            qClsD_SafeToXXX ''SafeTo "List" type_ls_b $
                \name_a strXXX typeXXX_Q ->
                    return ([], ())
        clsD_Q = clsD_Q_ex >>= return . fst
    printLnQ clsD_Q
    pprintLnQ clsD_Q
    -- printLnQ $ let v = mkName "T" in [d| data $v = $v |]
    printLnQ $ let v = conT $ mkName "T" in [d| instance Eq $v |]
    let type_a = newName "a" >>= varT
    printLnQ $ sig__safe_toXXX "IntList" [t|[Int]|] type_a
    printLnQ $ cls__OpSafeToXXX "IntList" [t|[Int]|] $ mkName "a"


------------------ to/from XXX -- alias
to_from_tpl0 = [d|
    class SafeTo XXX a => OpSafeToXXX a where
        unsafe_toXXX :: a -> XXX
        safe_toXXX :: a -> Maybe XXX
    instance SafeTo XXX a => OpSafeToXXX a where
        unsafe_toXXX = unsafe_from
        safe_toXXX = safe_from
    class C a
    instance C a
    |]

to_from_tpl1 = [d|
    class SafeTo (ToXXX_Result a) a => OpSafeToXXX a where
        type ToXXX_Result a :: *
        -- type ToXXX_Result a = XXX
        unsafe_toXXX :: a -> XXX
        safe_toXXX :: a -> Maybe XXX
    instance SafeTo XXX a => OpSafeToXXX a where
        type ToXXX_Result a = XXX
        unsafe_toXXX = unsafe_from
        safe_toXXX = safe_from
    class C a
    instance C a
    |]
to_from_tpl = [d|
    class SafeTo XXX a => OpSafeToXXX a where
        unsafe_toXXX :: a -> XXX
        safe_toXXX :: a -> Maybe XXX
    class To XXX a => OpToXXX a where
        toXXX :: a -> XXX
    instance SafeTo XXX a => OpSafeToXXX a where
        unsafe_toXXX = unsafe_from
        safe_toXXX = safe_from
    instance To XXX a => OpToXXX a where
        toXXX = from

    class SafeFrom XXX a => OpSafeFromXXX a where
        unsafe_fromXXX :: XXX -> a
        safe_fromXXX :: XXX -> Maybe a
    class From XXX a => OpFromXXX a where
        fromXXX :: XXX -> a
    instance SafeFrom XXX a => OpSafeFromXXX a where
        unsafe_fromXXX = unsafe_from
        safe_fromXXX = safe_from
    instance From XXX a => OpFromXXX a where
        fromXXX = from
    |]
  where
    name = ''XXX
    -- xxx = name2ctor name -- str2ctor s
    s = nameBase name
    -- op_safe_to_xxx = $(add'' "OpSafeTo" s) :: TypeQ


qSigD_SafeToXXX name_a strXXX typeXXX_Q = do
    name_unsafe_toXXX <- newName $ "unsafe_to" ++ strXXX
    return ()
--             ''SafeTo   "List" ''[]     return . const [] -- "a"->Q [Dec]
qClsD_SafeToXXX nameSafeTo strXXX typeXXX_Q toDecsQ_ex = do
    name_a <- newName "a"
    typeXXX <- typeXXX_Q
    let a = name_a -- mkName "a"
        -- strXXX = nameBase nameXXX
        -- typeXXX = ConT nameXXX
        type_a = VarT a
        bind_a = PlainTV a
        -- clsOpSafeToXXX = "OpSafeTo" ^^+ strXXX
        strOpSafeTo = "Op" ++ nameBase nameSafeTo
        strOpSafeToXXX = strOpSafeTo ++ strXXX
    nameOpSafeToXXX <- newName strOpSafeToXXX
    (decs, method_names) <- toDecsQ_ex name_a strXXX typeXXX_Q
    return $
        ( ClassD
            [ClassP nameSafeTo [typeXXX, type_a]]
            nameOpSafeToXXX
            [bind_a]
            []
            decs
        , (name_a, method_names)
        )
    -- return strOpSafeToXXX



{-
def__OpSafeToXXX "IntList" [t| [Int] |] $ mkName "a"
def__OpToXXX "IntList" [t| [Int] |] $ mkName "a"
def__OpSafeFromXXX "IntList" (mkName "a") [t| [Int] |]
def__OpFromXXX "IntList" (mkName "a") [t| [Int] |]
--clsD__OpSafeToXXX "IntList" [t| [Int] |] $ mkName "a"
--instD__OpSafeToXXX "IntList" [t| [Int] |] $ mkName "a"
-}
def__Ops_FromOrToXXX "IntList" [t| [Int] |]
b1 = unsafe_toIntList ([1,2]::[Int]) == [1,2]
b2 = safe_toIntList ([1,2]::[Int]) == Just [1,2]
b3 = toIntList ([1,2]::[Int]) == [1,2]
b4 = unsafe_fromIntList [1,2] == ([1,2]::[Int])
b5 = safe_fromIntList [1,2] == Just ([1,2]::[Int])
b6 = fromIntList [1,2] == ([1,2]::[Int])
b = and [b1, b2, b3, b4, b5, b6]


{-
sig__safe_toXXX :: String -> TypeQ -> TypeQ -> DecQ
sig__safe_toXXX strXXX to_q from_q = sigD name sigQ where
    name = mkName $ "safe_to" ++ strXXX
    sigQ = [t| $from_q -> Maybe $to_q|]
sig__unsafe_toXXX :: String -> TypeQ -> TypeQ -> DecQ
sig__unsafe_toXXX strXXX to_q from_q = sigD name sigQ where
    name = mkName $ "unsafe_to" ++ strXXX
    sigQ = [t| $from_q -> $to_q|]

cls__OpSafeToXXX :: String -> TypeQ -> Name -> DecQ
cls__OpSafeToXXX strXXX to_q from_name = classD cxtQ name binds [] decQs where
    name = mkName $ "OpSafeTo" ++ strXXX
    from_q = varT from_name
    decQs = map (\f-> f strXXX to_q from_q) [sig__safe_toXXX, sig__unsafe_toXXX]
    cxtQ = cxt [classP ''SafeTo [to_q, from_q]]
    binds = [PlainTV from_name]


{-
[ClassD [ClassP Explain.SafeTo [ConT Explain__To_From_tpl.XXX,VarT a_21]] OpSafeToXXX_18 [PlainTV a_21] [] [SigD unsafe_toXXX_19 (AppT (AppT ArrowT (VarT a_21)) (ConT Explain__To_From_tpl.XXX)),SigD safe_toXXX_20 (AppT (AppT ArrowT (VarT a_21)) (AppT (ConT Data.Maybe.Maybe) (ConT Explain__To_From_tpl.XXX)))]]
[ ClassD
    [ClassP Explain.SafeTo [ConT Explain__To_From_tpl.XXX,VarT a_21]]
    OpSafeToXXX_18
    [PlainTV a_21]
    []
    [ SigD
        unsafe_toXXX_19
        (AppT (AppT ArrowT (VarT a_21)) (ConT Explain__To_From_tpl.XXX))
    , SigD
        safe_toXXX_20
        (AppT
            (AppT ArrowT (VarT a_21))
            (AppT
                (ConT Data.Maybe.Maybe)
                (ConT Explain__To_From_tpl.XXX)
            )
        )
    ]
]

--}
--}
--}
--}
--}
--}



