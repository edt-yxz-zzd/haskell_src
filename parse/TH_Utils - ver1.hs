{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

module TH_Utils
    ( module SeedUtils__TH
    , module TH_Utils
    )
where
import SeedUtils__TH
import SeedUtils (lift2, for)
import PrintQ
import Language.Haskell.TH as TH
import Language.Haskell.Syntax as Syntax
import Control.Monad
-- import SeedUtils__TH
{-# LANGUAGE TemplateHaskell #-}
import Data.Maybe
import qualified Data.Map as M 
import Data.Map (Map)
import Control.Monad.Reader



class (Visit m a, MonadReader t m) => VisitByTable t m a where
instance (Visit m a, MonadReader t m) => VisitByTable t m a where
class Monad m => Visit m a where
    visit :: a -> m a
instance Visit m a => Visit m [a] where
    visit = mapM visit



toInstance :: TypeQ -> String -> TypeQ -> DecQ
toInstance typeTable_Q strB typB_Q = do
    typeB <- typB_Q
    let name_conB = type2ty_con_name typeB
    toInstance__ty_con_name name_conB strB typB_Q typeTable_Q
toInstance__ty_con_name :: Name -> String -> TypeQ -> TypeQ -> DecQ
toInstance__ty_con_name name_conB strB typB_Q typeTable_Q = do
    con_len_ls_B <- ty_con_name2con_len_ls name_conB
    toInstance_ con_len_ls_B strB typB_Q typeTable_Q
toInstance_ :: [(Name, Int)] -> String -> TypeQ -> TypeQ -> DecQ
toInstance_ con_len_ls_B strB typB_Q typeTable_Q = do
  typeTable <- typeTable_Q
  m <- newName "m"
  x <- newName "x"
  table <- newName "table"
  typeB <- typB_Q
  name_get_case <- newName "get_case"
  name_caseBx <- newName "caseBx"
  let tableB = mkName $ "table" ++ strB
  matches <- mkMatches x table name_get_case con_len_ls_B
  return $ InstanceD
    [ClassP ''MonadReader [AppT typeTable (VarT m), VarT m]]
    (AppT (AppT (ConT ''Visit) (VarT m)) typeB)
    [FunD 'visit [Clause [VarP x] (NormalB (LetE
        [ValD (VarP table) (NormalB (VarE tableB)) []]
        (CaseE (VarE x)
            matches
        )
    ))
    [FunD name_get_case [Clause [VarP name_caseBx] (NormalB (InfixE
        (Just (VarE 'asks))
        (VarE $ mkName "$")
        (Just (InfixE
            (Just (VarE name_caseBx))
            (VarE $ mkName ".")
            (Just (VarE tableB))
        ))
    )) []] ]
   ]]
  where
    mkMatches x tableB name_get_case con_len_ls_B = return $
        map (mkMatch x tableB name_get_case) con_len_ls_B
    mkMatch x tableB name_get_case (nameB1, len) =
      let strB1 = nameBase nameB1
          name_get_caseB1 = mkName $ "case" ++ strB1
          name_var_caseB1 = mkName $ "_case" ++ strB1
      in  Match (ConP nameB1 $ replicate len WildP) (NormalB (DoE
            [BindS (VarP name_var_caseB1)
                (AppE (VarE name_get_case) (VarE name_get_caseB1))
            ,NoBindS (AppE (VarE name_var_caseB1) (VarE x))
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



















instance Ord Type where
    compare = lift2 show compare

call :: (t->t->t) -> t -> [t] -> t
call app f args = foldl app f args
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
-- Maybe
-- (,)
namesTH = [''Name, ''Cxt, ''Dec, ''Exp, ''Con, ''Type, ''TyVarBndr, ''Kind, ''Pred, ''Match, ''Clause, ''Body, ''Guard, ''Stmt, ''Range, ''Lit, ''Pat, ''FieldPat, ''FieldExp, ''Strict, ''Foreign, ''Callconv, ''Safety, ''Pragma, ''InlineSpec, ''FunDep, ''FamFlavour, ''StrictType, ''VarStrictType]

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
    f (typeX,strX,ss) = def_Table__Type2Case strX typeX ss m_type2tvs_Q

def_Table__Types
    :: String -- label of the whole type group
    -> [Name] -- tvs except "m"
    -> [(Type, String)] -- per record item, i.e. per subtable
    -> Q (Name, Type2tvs)
    -> DecQ
def_Table__Types strALL tvs_ type_str_pairs m_type2tvs_Q = do
    (nameM, type2tvs) <- m_type2tvs_Q
    let nameTable = mkName $ "Table" ++ strALL
        tvs = map PlainTV $ tvs_ ++ [nameM]
        item (typeX, strX) = (name_tableX, NotStrict, typeTableX) where
            name_tableX = mkName $ "table" ++ strX
            nameTableX = mkName $ "Table" ++ strX
            argTableX = map VarT $ (type2tvs ! typeX) ++ [nameM]
            typeTableX = call AppT (ConT nameTableX) argTableX
        items = map item type_str_pairs
    return $ DataD [] nameTable tvs [RecC nameTable items] []

def_Table__Type2Case :: String -> Type -> [String] -> Q (Name, Type2tvs)
    -> DecQ
def_Table__Type2Case strX typeX str_cases m_type2tvs_Q = do
    (nameM, type2tvs) <- m_type2tvs_Q
    let nameTableX = mkName $ "Table" ++ strX
        tvs = map PlainTV $ (type2tvs ! typeX) ++ [nameM]
        typeM = VarT nameM
        method_type = call AppT ArrowT [typeX, AppT typeM typeX]
        str_case2record_item strX1 = (name_caseX1, NotStrict, method_type)
            where name_caseX1 = mkName $ "case" ++ strX1
        record_items = map str_case2record_item str_cases

    return $ DataD [] nameTableX tvs [RecC nameTableX record_items] []


to_case_sigfuns :: [(String, Name, Type)]
    -> Q (Type, [Name], CaseName2app, Type2tvs)
    -> DecsQ
to_case_sigfuns ls t4 = r where
    f g (a, b, c) = g a b (return c) t4
    r = sequence $ map (f to_case_fun) ls ++ map (f to_case_sig) ls

to_case_fun
    :: String -> Name -> TypeQ
    -> Q (Type, [Name], CaseName2app, Type2tvs)
    -> DecQ
to_case_fun strX1 nameX1 typeX_Q typeM_tvsM_types_to_visit = do
    (typeM, tvsM, case2con_types, types_to_visit) <- typeM_tvsM_types_to_visit
    typeX <- typeX_Q
    let name_default_caseX1 = mkName $ "default_case" ++ strX1
    let arg_types = case2con_types M.! nameX1
        to_visits = flip map arg_types $ (`M.member` types_to_visit)
        n = length arg_types
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
                (VarE $ mkName "$")
                (Just result))
        do_body = binds ++ [return_stmt]

    return $ FunD name_default_caseX1
      [Clause [ConP nameX1 pats] (NormalB $ DoE do_body) [] ]

to_case_sig
    :: String -> Name -> TypeQ
    -> Q (Type, [Name], CaseName2app, Type2tvs)
    -> DecQ
to_case_sig strX1 nameX1 typeX_Q typeM_tvsM_types_to_visit = do
    (typeM, tvsM, case2con_types, types_to_visit) <- typeM_tvsM_types_to_visit
        -- cxt ==[ClassP Visit [VarT m,ConT Bool], ...]
    typeX <- typeX_Q
    let tvs = tvs_ tvsM typeX types_to_visit
        cxt = cxt_ nameX1 typeM case2con_types types_to_visit
    return $ SigD name_default_caseX1 $ ForallT tvs cxt $
        call AppT ArrowT [typeX, AppT typeM typeX]
        {-
        (AppT (AppT ArrowT typeX) (AppT typeM typeX))
        (AppT
            typeX
            (AppT typeM typeX)
        )
        -}
  where
    name_default_caseX1 = mkName $ "default_case" ++ strX1
    tvs_ tvsM typeX types_to_visit =
        map PlainTV $ tvsM ++ (types_to_visit ! typeX)
    cxt_ nameX1 typeM case2con_types types_to_visit = cxt
        where
        types = case2con_types M.! nameX1
        types' = filter (`M.member` types_to_visit) types
        cxt = map f types'
        f t = ClassP ''Visit [typeM, t]


--}

