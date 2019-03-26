{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

module TH_Utils_base
    -- ( module SeedUtils__TH
    -- , module Visit
    -- , module TH_Utils_base
    ( Visit (..)
    , VisitByTable (..)
    , WildCase (..)
    , Visit_TH (..)
    , RT (..)
    , RM (..)
    , typeAssertMonadReader
    , typeAssertVisit
    , def_VisitInstance_perType__return
    , def_VisitInstance_perType
    , unfold_type
    , unfold_inst_type
    , call
    , def_mainTable
    , def_Table_perType
    , def_Tables
    , def_default_case_perCon
    , def_default_caseX1_sig
    , def_default_caseX1_fun
    , def_default_TableX
    , def_default_TableX_sig
    , def_default_TableX_table
    , def_default_TableMain
    , def_default_TableMain_sig
    , def_default_TableMain_table
    , Info_perCon (..)
    , Info_perType (..)
    , InfoMain (..)
    , def_VisitInstance_perType__fromInfoX
    , def_TableMain_all
    , typeX2strX
    , typeX2may_strX_to_typeX2strX
    , mkInfoX__simple
    , mkInfoListX__simple
    , mkInfoX__wild_simple_ex
    , mkInfoMaybeX__simple
    , mkInfoPairXY__simple
    , mkInfoTuple__simple
    , mkInfoX__wild_simple
    , PairGuardExp
    , ListStmt
    , ListPred
    , infoMain_TH_Q
    , def_VisitInstances__prime_types_TH
    , def_VisitInstance_all__TH
    )
where
import Visit
import SeedUtils__TH
import SeedUtils (lift2, for)
import PrintQ
import Language.Haskell.TH as TH
import Language.Haskell.Syntax
import Language.Haskell.TH.Syntax -- (StrictType, VarStrictType)
import Control.Monad
-- import SeedUtils__TH
{-# LANGUAGE TemplateHaskell #-}
import Data.Maybe
import qualified Data.Map as M 
import Data.Map (Map)
import Control.Monad.Reader
import Data.List as L -- (nub)

{-
type_std_Q :: Type -> Q Bool

type_eq_Q :: Type -> Type -> Q Bool
type_eq_Q t1 t2 = if t1 == t2 then return True
    else do
        t1' <- type_std_Q t1
        t2' <- type_std_Q t2
        return $ t1' == t2'
nubQ = L.nubBy type_eq
-}


--type StrictType = (Strict, Type)
--type VarStrictType = (Name, Strict, Type)

{-

def_VisitInstance_perType
def_mainTable -- TableMain
def_Table_perType -- TableX
def_default_case_perCon -- default_caseX1
def_default_Table_perType -- default_TableX1
def_default_mainTable -- default_TableMain

-}



















def_VisitInstance_perType__return
    :: Type -> Type -> DecQ
def_VisitInstance_perType__return typeB_tvs typeM = do
    return $ InstanceD [ClassP ''Monad [typeM]]
        (call AppT (ConT ''Visit) [typeM, typeB_tvs])
        [ValD (VarP 'visit) (NormalB (VarE 'return)) []]

def_VisitInstance_perType
    :: Bool -> [(String, Name, Int)]
    -> String -> Type -> Type -> Type -> DecQ
def_VisitInstance_perType
    complete str_con_len_ls_B strB typeB_tvs typeM typeTable_tvs = do
  x <- newName "x"
  table <- newName "table"
  name_get_case <- newName "get_case"
  name_caseBx <- newName "caseBx"
  let tableB = mkName $ "table" ++ strB
  matches <- mkMatches complete x name_get_case str_con_len_ls_B strB
  return $ InstanceD
    [ClassP ''MonadReader [typeTable_tvs, typeM]]
    (call AppT (ConT ''Visit) [typeM, typeB_tvs])
    [FunD 'visit [Clause [VarP x] (NormalB (LetE
        [ValD (VarP table) (NormalB (VarE tableB)) []]
        (CaseE (VarE x)
            matches
        )
    ))
    [FunD name_get_case [Clause [VarP name_caseBx] (NormalB (InfixE
        (Just (VarE 'asks))
        (VarE '($))
        (Just (InfixE
            (Just (VarE name_caseBx))
            (VarE '(.))
            (Just (VarE tableB))
        ))
    )) []] ]
   ]]
  where
    mkMatches complete x name_get_case str_con_len_ls_B strB = return $
        map (mkMatch x name_get_case) str_con_len_ls_B
        ++ if complete then [] else [mkWildMatch x name_get_case strB]
    mkMatch x name_get_case (strB1, nameB1, len) =
      let name_get_caseB1 = mkName $ "case" ++ strB1
          name_var_caseB1 = mkName $ "_case" ++ strB1
      in  Match (ConP nameB1 $ replicate len WildP) (NormalB (DoE
            [BindS (VarP name_var_caseB1)
                (AppE (VarE name_get_case) (VarE name_get_caseB1))
            ,NoBindS (AppE (VarE name_var_caseB1) (VarE x))
            ]
          )) []
    mkWildMatch x name_get_case strB =
      let name_var_wildB = mkName $ "_wild" ++ strB
          name_get_wildB = mkName $ "wild" ++ strB
      in  Match WildP (NormalB (DoE
            [BindS (VarP name_var_wildB)
                (AppE (VarE name_get_case) (VarE name_get_wildB))
            ,NoBindS (AppE (VarE name_var_wildB) (VarE x))
            ]
          )) []

con2con_types :: Con -> (Name, [Type])
con2con_types con = case con of
    NormalC name s_types -> (name, map snd s_types)
    RecC name v_s_types -> (name, map (\(_,_,t)->t) v_s_types)
    InfixC st1 name st2 -> (name, map snd [st1, st2])
    ForallC tvs cxt con -> con2con_types con
type_dec2con_types_ls :: Dec -> [(Name, [Type])]
type_dec2con_types_ls dec = case dec of
    DataD cxt name tvs cons derivings -> map con2con_types cons
    _ -> error $ "not type dec:\n" ++ show dec
ty_con_name2con_types_ls :: Name -> Q [(Name, [Type])]
ty_con_name2con_types_ls ty_con = do
    info <- reify ty_con
    case info of
        TyConI dec -> return $ type_dec2con_types_ls dec
        _ -> fail $ "not a type constructor:\n" ++ show ty_con
ty_con_name2con_len_ls :: Name -> Q [(Name, Int)]
ty_con_name2con_len_ls ty_con = do
    con_types_ls <- ty_con_name2con_types_ls ty_con
    return $ map f con_types_ls where
    f (name, types) = (name, length types)

type2ty_con_name :: Type -> Name
type2ty_con_name t = case t of
    ForallT tvs cxt t' -> type2ty_con_name t'
    VarT name -> error "VarT @type2ty_con_name"
    ConT name -> name
    TupleT len -> mkName . parans $ tuple_mid len
    UnboxedTupleT len -> mkName . parans $ "#" ++ tuple_mid len ++ "#"
    ArrowT -> mkName "->"
    ListT -> mkName "[]"
    AppT t1 _ -> type2ty_con_name t1
    SigT t' kind -> type2ty_con_name t'
  where
    parans x = "(" ++ x ++ ")"
    tuple_mid len = if len == 0 then "" else replicate (len-1) ','



unfold_type :: Type -> (Type, [Type])
unfold_type t = case reverse $ reversed_unfold_type t of
    (h : tail) -> (h, tail)
    _ -> error "logic error @ unfold_type"
reversed_unfold_type :: Type -> [Type]
reversed_unfold_type t = case t of
    AppT t1 t2 -> t2 : self_f t1
    ForallT tvs cxt t' -> self_f t'
    SigT t' kind -> self_f t'
    _ -> [t]
  where
    self_f = reversed_unfold_type

unfold_inst_type :: Type -> (Name, [Type])
    -- InstanceD cxt inst_type dec
unfold_inst_type t = let (t', ls) = unfold_type t in case t' of
    ConT name -> (name, ls)
    _ -> error $
        "unfold_inst_type t while t not begin with (ConT Name):\n"
        ++ show t
















call :: (t->t->t) -> t -> [t] -> t
call app f args = foldl app f args

tableX_infos :: String -> [Name] -> (Name, Name, Type)
tableX_infos strX name_tvsTableX = (name_tableX, nameTableX_, typeTableX_tvs)
  where
    -- tableX :: TableX a b m
    name_tableX = mkName $ "table" ++ strX
    nameTableX_ = mkName $ "Table" ++ strX
    argTableX = map VarT $ name_tvsTableX
    typeTableX_tvs = call AppT (ConT nameTableX_) argTableX
def_mainTable
    :: String -- label of the whole type group
    -> [TyVarBndr] -- tvs include "m"
    -> Name
    -> [(String, [Name])] -- per record item, i.e. per subtable
    -> DecQ
def_mainTable strMain tvsMain nameM strX_nametvsTableX_pairs = do
    let nameTable_ = mkName $ "Table" ++ strMain
        -- tvs = map PlainTV $ name_tvs
        item (strX, name_tvsTableX) = (name_tableX, NotStrict, typeTableX_tvs)
          where
            (name_tableX, nameTableX_, typeTableX_tvs) =
                tableX_infos strX name_tvsTableX
        items = map item strX_nametvsTableX_pairs
    return $ DataD [] nameTable_ tvsMain [RecC nameTable_ items] []

tv2name (PlainTV n) = n
tv2name (KindedTV n _) = n
tvs2names = map tv2name
caseXx_info :: Type -> Type -> (String->Name, String->Name, Type)
caseXx_info typeX typeM = (strX2name_wildX, strX12name_caseX1, method_type)
  where strX12name_caseX1 strX1 = mkName $ "case" ++ strX1
        strX2name_wildX strX = mkName $ "wild" ++ strX
        method_type = call AppT ArrowT [typeX, AppT typeM typeX]
def_Table_perType
    :: Name
    -> String -> Type -> [TyVarBndr] -> Bool -> [String]
    -> DecQ
def_Table_perType nameM strX typeX tvsTableX complete strX1_ls = do
    let (name_tableX, nameTableX_, typeTableX_tvs) =
            tableX_infos strX name_tvsTableX
        name_tvsTableX = tvs2names tvsTableX
        typeM = VarT nameM
        (strX2name_wildX, strX12name_caseX1, method_type) =
            caseXx_info typeX typeM
        strXx2record_item strX1 =
            (strX12name_caseX1 strX1, NotStrict, method_type)
        name_wildX = strX2name_wildX strX
        record_items_ = map strXx2record_item strX1_ls
        record_items = if complete then record_items_ else
            (name_wildX, NotStrict, method_type) : record_items_


    return $ DataD [] nameTableX_ tvsTableX [RecC nameTableX_ record_items] []



def_Tables
    :: String -- label of the whole type group
    -> [TyVarBndr] -- tvs include "m"
    -> Name -- name "m"
    -- -> [(String, [Name])] -- per record item, i.e. per subtable
    -> [(String, Type, [TyVarBndr], Bool, [String])]
    -> DecsQ
def_Tables
    strMain tvsMain nameM -- strX_nametvsTableX_pairs
    strX_typeX_tvsTableX_complete_strX1s_ls = do
    mainTable <- def_mainTable strMain tvsMain nameM strX_nametvsTableX_pairs
    subTables <- mapM f strX_typeX_tvsTableX_complete_strX1s_ls
    return $ mainTable : subTables
  where
    f (strX, typeX, tvsTableX, complete, strX1s) =
        def_Table_perType nameM strX typeX tvsTableX complete strX1s
    strX_nametvsTableX_pairs = map g strX_typeX_tvsTableX_complete_strX1s_ls
    g (strX, typeX, tvsTableX, complete, strX1s) =
        (strX, tvs2names tvsTableX)



def_default_case_perCon
    :: Type -> [TyVarBndr]
    -> Type -> [TyVarBndr]
    -> String -> Name -> [Type] -> [Bool]
    -> ([Type] -> Q [Type])
    -> DecsQ
def_default_case_perCon
    typeM tvsM typeX tvsX
    strX1 nameX1 conX1_argtypes_to_be_visited is_arg_to_be_visited
    unique_types = do
    sig <- def_default_caseX1_sig
                typeM tvsM typeX tvsX
                strX1 nameX1 conX1_argtypes_to_be_visited
                unique_types
    fun <- def_default_caseX1_fun strX1 nameX1 is_arg_to_be_visited
    return [sig, fun]


unique_types_TH :: [Type] -> Q [Type]
unique_types_TH types = return . unique_types_ $ map std types where
    std t   | t == AppT ListT (ConT ''Pred) = ConT ''Cxt
            | t == AppT ListT (ConT ''Stmt) = ConT ''ListStmt
            | otherwise = t
unique_types_ :: [Type] -> [Type]
unique_types_ = L.nub
def_default_caseX1_sig
    :: Type -> [TyVarBndr]
    -> Type -> [TyVarBndr]
    -> String -> Name -> [Type]
    -> ([Type] -> Q [Type])
    -> DecQ
def_default_caseX1_sig
    typeM tvsM typeX tvsX
    strX1 nameX1 conX1_argtypes_to_be_visited' unique_types
    = do
    uniqued_conX1_argtypes_to_be_visited <-
        unique_types conX1_argtypes_to_be_visited'
    let name_default_caseX1 = mkName $ "default_case" ++ strX1
        -- cxt ==[ClassP Visit [VarT m,ConT Bool], ...]
        cxt' = for (uniqued_conX1_argtypes_to_be_visited) $
            \t -> ClassP ''Visit [typeM, t]
        cxt = ClassP ''Monad [typeM] : cxt'
        tvs = tvsM ++ tvsX
    return $ SigD name_default_caseX1 $ ForallT tvs cxt $
        call AppT ArrowT [typeX, AppT typeM typeX]


def_default_caseX1_fun
    :: String -> Name -> [Bool]
    -> DecQ
def_default_caseX1_fun strX1 nameX1 is_arg_to_be_visited = do
    let name_default_caseX1 = mkName $ "default_case" ++ strX1
        to_visits = is_arg_to_be_visited
        n = length to_visits
        names = mapM newName $ take n $ repeat "a"
    input_names <- names
    output_names_ <- names
    let io_names = zip to_visits $ zip input_names output_names_
        output_names = for io_names $ \(b, (i, o)) -> if b then o else i
        bind_names = map snd $ filter fst io_names
        pats = map VarP input_names
        binds = for bind_names $ \(a1,a1') ->
            BindS (VarP a1') (AppE (VarE 'visit) (VarE a1))
        result = call AppE (ConE nameX1) $ map VarE output_names
        return_stmt = NoBindS (InfixE
                (Just (VarE 'return))
                (VarE '($))
                (Just result))
        do_body = binds ++ [return_stmt]

    return $ FunD name_default_caseX1
      [Clause [ConP nameX1 pats] (NormalB $ DoE do_body) [] ]



def_default_TableX
    :: String -> [TyVarBndr] -> Type -> [Type]
    -- -> Bool -> [String]
    -> [String] -> Maybe Type
    -> ([Type] -> Q [Type])
    -> DecsQ
def_default_TableX
    strX tvsTableX typeM all_con_arg_typesX
    -- complete strX1_ls = do
    strX1_ls may_typeX_if_incomplete unique_types = do
    --let may_typeX_if_incomplete =
    --        if complete then Nothing else Just typeX
    let complete = Nothing == may_typeX_if_incomplete
    default_TableX_sig <- def_default_TableX_sig
                        strX tvsTableX typeM all_con_arg_typesX
                        may_typeX_if_incomplete unique_types
    default_TableX_table <- def_default_TableX_table
                        strX complete strX1_ls
    return [default_TableX_sig, default_TableX_table]
def_default_TableX_sig :: String -> [TyVarBndr] -> Type -> [Type]
    -> Maybe Type -> ([Type] -> Q [Type]) -> DecQ
def_default_TableX_sig
    strX tvsTableX typeM all_con_arg_typesX'
    may_typeX_if_incomplete unique_types
    = do
    uniqued_all_con_arg_typesX <- unique_types all_con_arg_typesX'
    let name_default_tableX = mkName $ "default_table" ++ strX
        name_tvsTableX = tvs2names tvsTableX
        (_, _, typeTableX_tvs) = tableX_infos strX name_tvsTableX
        classPs' = for (uniqued_all_con_arg_typesX) $ \typeArg ->
            ClassP ''Visit [typeM, typeArg]
        classPs = maybe classPs' f may_typeX_if_incomplete where
            f typeX = ClassP ''WildCase [typeM, typeX] : classPs'
    return $ SigD name_default_tableX (ForallT tvsTableX
        ([ClassP ''Monad [typeM]] ++ classPs)
        typeTableX_tvs
      )
def_default_TableX_table :: String -> Bool -> [String] -> DecQ
def_default_TableX_table strX complete strX1_ls = do
    let name_default_tableX = mkName $ "default_table" ++ strX
        name_TableX_ = mkName $ "Table" ++ strX
        items' = for strX1_ls $ \strX1 ->
            let name_caseX1 = mkName $ "case" ++ strX1
                name_default_caseX1 = mkName $ "default_case" ++ strX1
            in  (name_caseX1, VarE name_default_caseX1)
        name_wildX = mkName $ "wild" ++ strX
        items = if complete then items'
                else (name_wildX, VarE 'wild_case) : items'
    return $ ValD (VarP name_default_tableX)
        (NormalB (RecConE name_TableX_ items)) []


def_default_TableMain
    :: String -> [TyVarBndr] -> Type -> [Type] -> [String]
    -> ([Type] -> Q [Type])
    -> DecsQ
def_default_TableMain
    strMain tvsMain typeM all_typecon_arg_types strX_ls unique_types = do
    sig <- def_default_TableMain_sig
            strMain tvsMain typeM all_typecon_arg_types
            unique_types
    table <- def_default_TableMain_table strMain strX_ls
    return [sig, table]



def_default_TableMain_sig
    :: String -> [TyVarBndr] -> Type -> [Type]
    -> ([Type] -> Q [Type])
    -> DecQ
def_default_TableMain_sig
    strMain tvsMain typeM all_typecon_arg_types unique_types = do
    def_default_TableX_sig
        strMain tvsMain typeM all_typecon_arg_types Nothing unique_types
def_default_TableMain_table :: String -> [String] -> DecQ
def_default_TableMain_table strMain strX_ls = do
    let name_default_TableMain = mkName $ "default_table" ++ strMain
        name_TableMain_ = mkName $ "Table" ++ strMain
        items = for strX_ls $ \strX ->
            let name_tableX = mkName $ "table" ++ strX
                name_default_tableX = mkName $ "default_table" ++ strX
            in  (name_tableX, VarE name_default_tableX)
    return $ ValD (VarP name_default_TableMain)
        (NormalB (RecConE name_TableMain_ items)) []

data Info_perCon = InfoX1
    { strX1 :: String
    , nameX1 :: Name
    , conX1_argtypes_to_be_visited :: [Type] -- <= len(args)
    , is_arg_to_be_visited :: [Bool] -- == len(args)
    }
data Info_perType = InfoX
    { strX :: String
    , typeX :: Type -- typeX_tvs :: *
    , tvsX :: [TyVarBndr]
    , tvsTableX :: [TyVarBndr]
    , complete :: Bool
    , types_to_be_visited_if_incomplete :: [Type]
    , infoX1s :: [Info_perCon]
    }
data InfoMain = InfoMain
    { strMain :: String
    , tvsMain :: [TyVarBndr]
    , unique_typesQ :: [Type] -> Q [Type]
    , nameM :: Name
    , typeM :: Type
    , tvsM :: [TyVarBndr]
    , infoXs :: [Info_perType]
    }

def_VisitInstance_perType__fromInfoX :: Type -> Type -> Info_perType -> DecQ
def_VisitInstance_perType__fromInfoX
    typeTable_tvs typeM
    InfoX {strX, typeX, tvsX, complete, infoX1s} = do
    let str_con_len_ls_X = for infoX1s $
            \InfoX1 {strX1, nameX1, is_arg_to_be_visited}
            -> (strX1, nameX1, length is_arg_to_be_visited)
    def_VisitInstance_perType
        complete str_con_len_ls_X strX typeX typeM typeTable_tvs
def_VisitInstance_all :: Type -> Type -> InfoMain -> DecsQ
def_VisitInstance_all typeTable_tvs typeM info = do
    forM (infoXs info) $
        def_VisitInstance_perType__fromInfoX typeTable_tvs typeM

def_TableMain_all :: InfoMain -> DecsQ
def_TableMain_all InfoMain
    {strMain, tvsMain, nameM, typeM, tvsM, infoXs, unique_typesQ} = do
    let strX_typeX_tvsTableX_complete_strX1s_ls = for infoXs $
            \ InfoX {strX, typeX, tvsTableX, complete, infoX1s} ->
                (strX, typeX, tvsTableX, complete, map strX1 infoX1s)
        strX_ls = map strX infoXs
        all_typecon_arg_types' = concat . concat . for infoXs $
            map conX1_argtypes_to_be_visited . infoX1s
        all_typecon_arg_types'' = concat . for infoXs $
            \InfoX {complete, types_to_be_visited_if_incomplete}
                ->  if complete then []
                    else types_to_be_visited_if_incomplete
        all_typecon_arg_types =
            all_typecon_arg_types' ++ all_typecon_arg_types''
    table_decs <- def_Tables
                strMain tvsMain nameM strX_typeX_tvsTableX_complete_strX1s_ls
    tableMain_decs <- def_default_TableMain
        strMain tvsMain typeM all_typecon_arg_types strX_ls unique_typesQ
    default_caseX1_decss <- forM infoXs $ def_default_caseX1s
    default_tableX_decss <- forM infoXs def_default_TableX'

    return . concat $
        table_decs : tableMain_decs :
        default_caseX1_decss ++ default_tableX_decss
  where
    def_default_caseX1s InfoX {strX, typeX, tvsX, complete, infoX1s} = do
      fmap concat . forM infoX1s $
        \InfoX1 {strX1, nameX1
                ,conX1_argtypes_to_be_visited, is_arg_to_be_visited} ->
          def_default_case_perCon
            typeM tvsM typeX tvsX strX1 nameX1
            conX1_argtypes_to_be_visited is_arg_to_be_visited
            unique_typesQ
    def_default_TableX' InfoX
        { strX, typeX, tvsTableX
        , complete, types_to_be_visited_if_incomplete, infoX1s} = do
        let all_con_arg_typesX' = concat . for infoX1s $
                conX1_argtypes_to_be_visited
            all_con_arg_typesX = all_con_arg_typesX' ++ if complete
                then [] else types_to_be_visited_if_incomplete
            strX1_ls = map strX1 infoX1s
            may_typeX_if_incomplete = if complete then Nothing
                                        else Just typeX
        def_default_TableX
            strX tvsTableX typeM all_con_arg_typesX
            strX1_ls may_typeX_if_incomplete unique_typesQ




data A = A1 | A2 deriving Show
data B = B1 | B2 Int deriving Show

main = do
    pprintLnQ xx
xx = def_TableMain_all InfoMain
    { strMain = "", tvsMain = [PlainTV m], nameM = m, typeM = VarT m
    , tvsM = [PlainTV m]
    , unique_typesQ = return . unique_types_
    , infoXs =
        [ InfoX
            { strX = "A", typeX = ConT ''A, tvsX = [], complete = True
            , types_to_be_visited_if_incomplete = undefined
            , tvsTableX = [tvM]
            , infoX1s =
                [ InfoX1
                    { strX1 = "A1"
                    , nameX1 = 'A1
                    , conX1_argtypes_to_be_visited = []
                    , is_arg_to_be_visited = []
                    }
                , InfoX1
                    { strX1 = "A2"
                    , nameX1 = 'A2
                    , conX1_argtypes_to_be_visited = []
                    , is_arg_to_be_visited = []
                    }
                ]
            }
        , InfoX
            { strX = "B", typeX = ConT ''B, tvsX = [], complete = True
            , types_to_be_visited_if_incomplete = undefined
            , tvsTableX = [tvM]
            , infoX1s =
                [ InfoX1
                    { strX1 = "B1"
                    , nameX1 = 'B1
                    , conX1_argtypes_to_be_visited = []
                    , is_arg_to_be_visited = []
                    }
                , InfoX1
                    { strX1 = "B2"
                    , nameX1 = 'B2
                    , conX1_argtypes_to_be_visited = [ConT ''Int]
                    , is_arg_to_be_visited = [True]
                    }
                ]
            }
        ]
    }
  where
    m = mkName "m"
    tvM = PlainTV m



nameX2strX :: Name -> String
nameX2strX name = r name where
    r' = nameBase name
    r name  | name == '[] = "EmptyList"
            | name == '(:) = "ListCon"
    r _ = r'
    {-
        '[] -> "EmptyList"
        '(:) -> "ListCon"
        _ -> r'
    -}
typeX2strX :: (Name -> String) -> Type -> String
typeX2strX showName typeX = t2s typeX where
    t2s t = case t of
        ForallT _ _ t' -> t2s t'
        VarT name -> showName name
        ConT name -> showName name
        TupleT len -> "Tuple" ++ show len
        UnboxedTupleT len -> "UnboxedTuple" ++ show len
        ArrowT -> "Arr"
        ListT -> "List"
        AppT t1 t2 -> t2s t1 ++ t2s t2
        SigT t' _ -> t2s t'

typeX2may_strX_to_typeX2strX :: (Type -> Maybe String) -> (Type -> String)
typeX2may_strX_to_typeX2strX f typeX = r where
    r = maybe r' id $ f typeX
    r' = typeX2strX nameX2strX typeX
mkInfoX__simple :: String -> Type -> [TyVarBndr] -> Q Info_perType
mkInfoX__simple strX typeX tvsTableX = do
    let nameX_ = type2ty_con_name typeX
    con_types_lsX <- ty_con_name2con_types_ls nameX_
    let tvsX = [] -- simple
        -- tvsTableX = []
        complete = True
        item (con, types) = InfoX1
            { strX1 = nameX2strX con, nameX1 = con
            , conX1_argtypes_to_be_visited = types
            , is_arg_to_be_visited = replicate (length types) True
            }
        infoX1s = map item con_types_lsX
    return InfoX
        { strX = strX
        , typeX = typeX
        , tvsX = tvsX
        , tvsTableX = tvsTableX
        , complete = complete
        , types_to_be_visited_if_incomplete = []
        , infoX1s = infoX1s
        }


mkInfoListX__simple :: String -> Type -> [TyVarBndr] -> Q Info_perType
mkInfoListX__simple strX typeX tvsTableX =
    mkInfoX__wild_simple_ex ("List" ++ strX) (AppT ListT typeX)
        tvsTableX [typeX]
mkInfoMaybeX__simple :: String -> Type -> [TyVarBndr] -> Q Info_perType
mkInfoMaybeX__simple strX typeX tvsTableX =
    mkInfoX__wild_simple_ex ("Maybe" ++ strX) (AppT (ConT ''Maybe) typeX)
        tvsTableX [typeX]
mkInfoPairXY__simple :: String -> Type -> String -> Type
    -> [TyVarBndr] -> Q Info_perType
mkInfoPairXY__simple strX typeX strY typeY tvsTablePairXY =
    mkInfoTuple__simple "Pair" [(strX, typeX), (strY, typeY)]
                        tvsTablePairXY
mkInfoTuple__simple :: String -> [(String, Type)]
    -> [TyVarBndr] -> Q Info_perType
mkInfoTuple__simple strTupleN_ strX_typeX_ls tvsTableTuple_ = do
    let strXs = map fst strX_typeX_ls
        typeXs = map snd strX_typeX_ls
        n = length strX_typeX_ls
    mkInfoX__wild_simple_ex (concat $ strTupleN_ : strXs)
      (call AppT (TupleT n) typeXs)
      tvsTableTuple_ typeXs
mkInfoX__wild_simple :: String -> Type -> [TyVarBndr] -> Q Info_perType
mkInfoX__wild_simple strX typeX tvsTableX = do
    mkInfoX__wild_simple_ex strX typeX tvsTableX []
mkInfoX__wild_simple_ex
    :: String -> Type -> [TyVarBndr] -> [Type]
    -> Q Info_perType
mkInfoX__wild_simple_ex
    strX typeX tvsTableX types_to_be_visited_if_incomplete= do
    return InfoX
        { strX = strX
        , typeX = typeX
        , tvsX = [] -- simple
        , tvsTableX = tvsTableX
        , complete = False -- wild
        , types_to_be_visited_if_incomplete = types_to_be_visited_if_incomplete
        , infoX1s = [] -- wild
        }


type PairGuardExp = (Guard, Exp)
type ListPred = [Pred] -- Cxt
type ListStmt = [Stmt]

namesTH =
    [''Name, ''Cxt, ''FieldPat, ''FieldExp, ''StrictType, ''VarStrictType
    , ''Dec, ''Exp, ''Con, ''Type, ''TyVarBndr, ''Kind, ''Pred, ''Match, ''Clause, ''Body, ''Guard, ''Stmt, ''Range, ''Lit, ''Pat, ''Strict, ''Foreign, ''Callconv, ''Safety, ''Pragma, ''InlineSpec, ''FunDep, ''FamFlavour]
namesTH__ListElem = [''Pred, ''Pat, ''Exp, ''Dec, ''Stmt, ''Match, ''Type, ''Clause, ''TyVarBndr, ''Name, ''Con, ''FunDep, ''FieldExp, ''FieldPat, ''StrictType, ''VarStrictType, ''ListStmt, ''PairGuardExp]
    -- (Guard, Exp) [Stmt]

namesTH__MaybeElem = [''Name, ''Dec, ''Kind, ''Exp, ''InlineSpec]
(namesTH_x, namesTH_normal) = break (==''Dec) namesTH
typeXsTH_normal = map ConT namesTH_normal
strXsTH_normal = map nameX2strX namesTH_normal
typeXsTH__ListElem = map ConT namesTH__ListElem
strXsTH__ListElem = map nameX2strX namesTH__ListElem
typeXsTH__MaybeElem = map ConT namesTH__MaybeElem
strXsTH__MaybeElem = map nameX2strX namesTH__MaybeElem

infoMain_TH_Q :: Q InfoMain
infoMain_TH_Q = do
  let strXs_typeXs2infoXs (mk, strXs, typeXs) =
        mapM (\(s,t) -> mk s t [tvM]) $ zip strXs typeXs
  infoXs <- fmap concat . sequence $ map strXs_typeXs2infoXs
        [(mkInfoX__simple, strXsTH_normal, typeXsTH_normal)
        ,(mkInfoListX__simple, strXsTH__ListElem, typeXsTH__ListElem)
        ,(mkInfoMaybeX__simple, strXsTH__MaybeElem, typeXsTH__MaybeElem)
        ]
  -- ''Name, ''FieldPat, ''FieldExp, ''StrictType, ''VarStrictType
  infoX__Name <- mkInfoX__wild_simple "Name" (ConT ''Name) [tvM]
  info_pairs <- mapM pair_data2infoQ pair_data
  info_triples <- mapM triple_data2infoQ triple_data
  return InfoMain
    { strMain = "TH", tvsMain = [tvM], nameM = nameM, typeM = typeM
    , unique_typesQ = unique_types_TH
    , tvsM = [tvM]
    , infoXs = [infoX__Name]
                ++ info_triples ++ info_pairs ++ infoXs
    }
  where
    infoXs = []
    nameM = mkName "m"
    m = nameM
    typeM = VarT m
    tvM = PlainTV m
    prime_data = [''Name]
    pair_data =
        [(''FieldPat, ''Name, ''Pat)
        ,(''FieldExp, ''Name, ''Exp)
        ,(''StrictType, ''Strict, ''Type)
        ,(''PairGuardExp, ''Guard, ''Exp)
        ]
    triple_data =
        [(''VarStrictType, ''Name, ''Strict, ''Type)
        ]
    pair_data2infoQ (namePair, nameX, nameY) = do
        let strPair = nameX2strX namePair
            strX = nameX2strX nameX
            strY = nameX2strX nameY
        mkInfoPairXY__simple strX (ConT nameX) strY (ConT nameY) [tvM]
    triple_data2infoQ (nameTri, nameX, nameY, nameZ) = do
        let strTri = nameX2strX nameTri
            names = [nameX, nameY, nameZ]
            strX_typeX_ls = map f names where
                f name = (nameX2strX name, ConT name)
        mkInfoTuple__simple "Triple" strX_typeX_ls [tvM]


prime_types_to_be_visited__TH :: [Type]
prime_types_to_be_visited__TH =
    -- (Maybe (Bool, Int)), Bool, Rational, Integer, String, Char, Int
    AppT (ConT ''Maybe) (call AppT (TupleT 2) $ map ConT [''Bool, ''Int])
    : map ConT [''Bool, ''Rational, ''Integer, ''String, ''Char, ''Int]



def_VisitInstances__prime_types_TH :: Type -> DecsQ
def_VisitInstances__prime_types_TH typeM =
    mapM f prime_types_to_be_visited__TH where
    f typeX = def_VisitInstance_perType__return typeX typeM

def_VisitInstance_all__TH :: Type -> Type -> DecsQ
def_VisitInstance_all__TH typeTableTH_ typeM = do
    -- typeTableTH_ = ConT $ mkName "TableTH"
    let typeTable_tvs = AppT typeTableTH_ typeM
    decs1 <- infoMain_TH_Q >>= def_VisitInstance_all typeTable_tvs typeM
    decs2 <- def_VisitInstances__prime_types_TH typeM
    return $ decs1 ++ decs2


































































def__instances [d|
    class
        (Visit m0 PairGuardExp,
         Visit m0 VarStrictType,
         Visit m0 FieldPat,
         Visit m0 FieldExp,
         Visit m0 FunDep,
         Visit m0 TyVarBndr,
         Visit m0 Clause,
         Visit m0 Match,
         Visit m0 Stmt,
         Visit m0 Dec,
         Visit m0 Pred,
         Visit m0 Guard,
         Visit m0 Strict,
         Visit m0 (Maybe (Bool, Int)),
         Visit m0 Bool,
         Visit m0 (Maybe InlineSpec),
         Visit m0 InlineSpec,
         Visit m0 Safety,
         Visit m0 Callconv,
         Visit m0 [FieldPat],
         Visit m0 Rational,
         Visit m0 Integer,
         Visit m0 String,
         Visit m0 Char,
         Visit m0 [[Stmt]],
         Visit m0 [(Guard, Exp)],
         Visit m0 Kind,
         Visit m0 Int,
         Visit m0 StrictType,
         Visit m0 [VarStrictType],
         Visit m0 [StrictType],
         Visit m0 [FieldExp],
         Visit m0 Range,
         Visit m0 ListStmt,
         Visit m0 [Match],
         Visit m0 [Exp],
         Visit m0 [Pat],
         Visit m0 (Maybe Exp),
         Visit m0 Exp,
         Visit m0 Lit,
         Visit m0 [Type],
         Visit m0 (Maybe Kind),
         Visit m0 FamFlavour,
         Visit m0 Pragma,
         Visit m0 Foreign,
         Visit m0 [FunDep],
         Visit m0 Type,
         Visit m0 Con,
         Visit m0 [Name],
         Visit m0 [Con],
         Visit m0 [TyVarBndr],
         Visit m0 Cxt,
         Visit m0 [Dec],
         Visit m0 Body,
         Visit m0 Pat,
         Visit m0 [Clause],
         Visit m0 Name) => Visit_TH m0
    |]


























{-
instance Ord Type where
    compare = lift2 show compare

type CaseName2app = Map Name [Type]
type Type2tvs = Map Type [Name]
    -- {X v : [v], Either a b : [a, b], Maybe Int : []}

(!) :: (Ord k, Show k, Show v) => Map k v -> k -> v
d ! k = case M.lookup k d of
    Just v -> v
    _ -> error $ "cannot find: " ++ show k ++ " in " ++ show d




{-
-- Cxt Name FieldPat FieldExp StrictType VarStrictType
-- [] : Pred Pat Exp Dec [Stmt] Stmt Match Type Clause TyVarBndr Name Con FunDep FieldExp FieldPat StrictType VarStrictType (Guard, Exp) 
-- Maybe : Name Dec Kind Exp InlineSpec
-- (,)
namesTH = [''Name, ''Cxt, ''FieldPat, ''FieldExp, ''StrictType, ''VarStrictType, ''Dec, ''Exp, ''Con, ''Type, ''TyVarBndr, ''Kind, ''Pred, ''Match, ''Clause, ''Body, ''Guard, ''Stmt, ''Range, ''Lit, ''Pat, ''Strict, ''Foreign, ''Callconv, ''Safety, ''Pragma, ''InlineSpec, ''FunDep, ''FamFlavour]

name2usingConTs :: Name -> Q [Name]
name2usingConTs = undefined
name2usingTypes :: Name -> Q [Type]
names2types :: [Name] -> Q [Type]
type2ConT :: Type -> Name
type2usingTypes :: Type -> Q [Type]
type2usingConTs :: Type -> Q [Name]

std :: Type -> Type
names2types names = f names [] where
    f [] types = types
    f (n:ls) types = f ls $ news'++types where
        news = (map std . nub $ name2usingTypes n) \\ types
        news' = filter (\new->type2usingConTs new && names) news

type2tvs__TH = where
    r = M.fromList $ zip types $ repeat []
    excepts = [''Cxt, ''FieldExp, ''FieldPat]
    types = map ConT $ namesTH L.\\ excepts
def_Tables__TH :: DecsQ
def_Tables__TH = do
    undefined
  where
    strALL = "TH"
    tvs_ = []
-}
def_Tables_Instances
    :: String -- label of the whole type group
    -> [Name] -- whole table's tvs except "m"
    --  typeX  strX      nameX1 lenX1 strX1
    -> [(Type, String, [(Name, Int, String)])]
    --    nameM  nameX1:[typeArg1OfX1...] typeX:[name_a,b,c...]
    -> Q (Name, CaseName2app, Type2tvs)
    -> DecsQ
def_Tables_Instances strALL tvs_ type_str_infos_ls m_case2apps_type2tvs_Q = do
    let type_str_strcases_ls = for type_str_infos_ls $ \(a,b,t3s) ->
            (a,b, for t3s $ \(n,i,s)->s)
        typeX_strX_ls = for type_str_infos_ls $ \(a,b,_)->(a, b)
        type_str_caselens_ls = for type_str_infos_ls $ \(a,b,t3s) ->
            (a,b, for t3s $ \(n,i,s)->(n,i))
        strX1_nameX1_typeX_ls = concat $
            for type_str_infos_ls $ \(tX, sX, t3s) ->
                for t3s $ \(nX1, _, sX1) -> (sX1, nX1, tX)



    (nameM, case2apps, type2tvs) <- m_case2apps_type2tvs_Q
    let typeTable_ = ConT nameTable
        nameTable = mkName $ "Table" ++ strALL
        typeTable = AppT typeTable_ $ VarT nameM
        typeTable_Q = return typeTable
        m_type2tvs_Q = return (nameM, type2tvs)
        f = toInstance typeTable_Q
        typeM = VarT nameM
        tvsM = [nameM]
        typeM_tvsM_types_to_visit = return (typeM, tvsM, case2apps, type2tvs)
    -- decs <- forM typeX_strX_ls $ \(typeX, strX) ->
    --        f strX $ return typeX
    decs <- forM type_str_caselens_ls $ \(typeX, strX, caselens) ->
        toInstance_ caselens strX (return typeX) typeTable_Q
    tables <- def_Tables strALL tvs_ type_str_strcases_ls m_type2tvs_Q
    sigfucs <- to_case_sigfuns strX1_nameX1_typeX_ls typeM_tvsM_types_to_visit
    return $ sigfucs ++ tables ++ decs
def_Tables
    :: String -- label of the whole type group
    -> [Name] -- tvs except "m"
    -> [(Type, String, [String])]
    -> Q (Name, Type2tvs)
    -> DecsQ
def_Tables strALL tvs_ type_str_strcases_ls m_type2tvs_Q = do
    t <- def_Table__Types strALL tvs_ type_str_pairs m_type2tvs_Q
    ts <- mapM f type_str_strcases_ls
    return $ t:ts
  where
    type_str_pairs = for type_str_strcases_ls $ \(a,b,_)->(a,b)
    f (typeX,strX,ss) = def_Table_perType strX typeX ss m_type2tvs_Q




to_case_sigfuns :: [(String, Name, Type)]
    -> Q (Type, [Name], CaseName2app, Type2tvs)
    -> DecsQ
to_case_sigfuns ls t4 = r where
    f g (a, b, c) = g a b (return c) t4
    r = sequence $ map (f to_case_fun) ls ++ map (f to_case_sig) ls



--}
--}
--}
--}
--}
--}
--}

