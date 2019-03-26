{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE Rank2Types #-}



module TH.Data2Doc
    ( Data2Doc (..)
    , IData2Doc (..)
    , IArgs__Data2Doc (..)
    , pshow_data
    )
where


import Data.Data
import Text.PrettyPrint
import TH.Transform ()
import TH.Query (query_list_like)
import Data.Typeable
import Seed.Boxed

--py_call2doc :: ToArgsDoc args => String -> args -> Doc
--py_call2doc funcname args = text funcname <> to_args_doc args
comma_ls :: [Doc] -> Doc
comma_ls = sep . punctuate comma -- (nest 4 .)


class IArgs__Data2Doc args where
    make_doc :: args -> Constr -> [Doc] -> Doc
    make_doc__list :: args -> [(Bool, Doc)] -> (Bool, Doc)
    make_doc__string :: args -> String -> (Bool, Doc)
    make_doc__tuple :: args -> [(Bool, Doc)] -> (Bool, Doc)
    make_doc__string _ s = (False, text $ show s)
    make_doc__list _ bool_doc_pairs = (False, doc) where
        doc = brackets . comma_ls $ map snd bool_doc_pairs
    make_doc__tuple _ bool_doc_pairs = (False, doc) where
        doc = parens . comma_ls $ map snd bool_doc_pairs
class (Data a, IArgs__Data2Doc args) => IData2Doc args a where
    -- (need_paran::Bool, doc::Doc)
    data2doc_ex :: args -> a -> (Bool, Doc)
{-
class (Data a, IArgs__Data2Doc args, cls~Cases a) => IData2Doc_ cls args a where
    data2doc_ex_ :: proxy cls -> args -> a -> (Bool, Doc)
-}

class (IData2Doc () a) => Data2Doc a where
    data2doc :: a -> Doc
    data2doc = snd . data2doc_ex ()
pshow_data :: Data2Doc a => a -> String
pshow_data = render . data2doc

instance {-# OVERLAPPABLE #-} (IData2Doc () a) => Data2Doc a where
instance {-# OVERLAPPABLE #-} (Data a, IArgs__Data2Doc args)
    => IData2Doc args a where
    data2doc_ex = _data2doc_ex__otherwise
{-
instance {-# OVERLAPPABLE #-} (Data a, IArgs__Data2Doc args, Otherwise a ~ Cases a)
    => IData2Doc args a where
    data2doc_ex = _data2doc_ex__otherwise
instance {-# INCOHERENT #-} (Data a, IArgs__Data2Doc args)
    => IData2Doc args [a] where
    data2doc_ex = _data2doc_ex__ls
-}

{-
data family Cases x where
    Cases [a] = List a
    Cases a = Otherwise a
-}
{-
type family Cases x = r | r -> x where
    Cases [a] = List a
    Cases a = Otherwise a
data List a
data Otherwise a
-}


{-
instance {-# OVERLAPPABLE #-}
    ( Data a, IArgs__Data2Doc args
    --, IData2Doc_ (Cases a) args a
    )
    => IData2Doc args a where
    data2doc_ex args a = _data2doc_ex_ (Proxy :: Proxy (Cases a)) args a

instance {-# OVERLAPPABLE #-} (Data a, IArgs__Data2Doc args)
    --, IData2Doc_ (Cases a) args a)
    => IData2Doc_ (List a) args [a] where
    data2doc_ex_ _ args ls = _data2doc_ex__ls args ls
instance {-# OVERLAPPABLE #-} (Data a, IArgs__Data2Doc args, Otherwise a ~ Cases a)
    => IData2Doc_ (Otherwise a) args a where
    data2doc_ex_ _ args a = _data2doc_ex__otherwise args a

_data2doc_ex_ :: (Data a, IArgs__Data2Doc args) => proxy (Cases a) -> args -> a -> (Bool, Doc)
_data2doc_ex_ = data2doc_ex_
-}

data2doc_ex_ :: (Data a, IArgs__Data2Doc args) => args -> a -> (Bool, Doc)
data2doc_ex_ = data2doc_ex

_data2doc_ex__ls
    :: (Data a, IArgs__Data2Doc args -- , IData2Doc_ (Cases a) args a
    ) => args -> [a] -> (Bool, Doc)
_data2doc_ex__ls args ls = (need_paran, doc) where
        ls' = map (data2doc_ex args) ls
        (need_paran, doc) = make_doc__list args ls'
_data2doc_ex__otherwise
    :: (Data a, IArgs__Data2Doc args) => args -> a -> (Bool, Doc)
_data2doc_ex__otherwise args a = r where
    subBoolDocs = gmapQ (data2doc_ex args) a
    toDoc (need_paran, doc) = (if need_paran then parens else id) doc
    subDocs = map toDoc subBoolDocs
    constr = toConstr a
    doc = make_doc args constr subDocs
    need_paran = not $ null subDocs
    r__otherwise = (need_paran, doc)
    ------
    ty_con_of_a = typeRepTyCon $ typeOf a
    ty_con_of_ls = typeRepTyCon $ typeOf ""
    isList = ty_con_of_a == ty_con_of_ls
    subBoolDocs' = query_list_like (data2doc_ex args) a
    r__list = make_doc__list args subBoolDocs'
    ------
    ty_of_a = typeOf a
    ty_of_str = typeOf ""
    isStr = ty_of_a == ty_of_str
    r__str = case cast a of
        Just s -> make_doc__string args s
        _ -> error "logic error"
    r = if isStr then r__str else if isList then r__list else r__otherwise

    ---


{-
instance {-# INCOHERENT #-} {- OVERLAPS #-} (Data a, IArgs__Data2Doc args)
    => IData2Doc args [a] where
    data2doc_ex args ls = (need_paran, doc) where
        ls' = map (data2doc_ex args) ls
        (need_paran, doc) = make_doc__list args ls'
-}
instance IArgs__Data2Doc () where
    make_doc _ constr docs = docs' where
        constr' = show constr -- (:)? parans?
        docs' = text constr' $$ nest 4 (cat docs)
        {-
        docs' = case (constr', docs) of
            ("(:)", (h, ts)) -> h <> ts
            ("[]", (h, ts)) -> h <> ts
        -}



