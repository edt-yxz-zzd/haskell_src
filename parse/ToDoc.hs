{-# LANGUAGE FlexibleContexts
    , FlexibleInstances
    , TypeSynonymInstances
    , UndecidableInstances
    , OverlappingInstances
    #-}

-- to_doc/show - repr; text - str

module ToDoc -- (module ToDoc hiding(pair2dict_field))
where

import qualified Data.Map as M -- (Map)
import qualified Data.Set as S -- (Set)
import Data.Map (Map)
import Data.Set (Set)

import Text.PrettyPrint hiding (char)




py_call2doc :: ToArgsDoc args => String -> args -> Doc
py_call2doc funcname args = text funcname <> to_args_doc args
comma_ls :: [Doc] -> Doc
comma_ls = sep . punctuate comma -- (nest 4 .)

class ToArgsDoc a where
    to_args_doc :: a -> Doc -- contains "(" ")"
data Args0 = A0
data Args1 a = A1 a
data Args2 a b = A2 a b
data Args3 a b c = A3 a b c

instance ToArgsDoc Args0 where
    to_args_doc A0 = to_doc ()
instance ToDoc a => ToArgsDoc (Args1 a) where
    to_args_doc (A1 a) = parens . to_doc $ a
instance (ToDoc a, ToDoc b) => ToArgsDoc (Args2 a b) where
    to_args_doc (A2 a b) = to_doc $ (a, b)
instance (ToDoc a, ToDoc b, ToDoc c) => ToArgsDoc (Args3 a b c) where
    to_args_doc (A3 a b c) = to_doc $ (a, b, c)
{- bug: func (..) ==>> func(...) which should be func((..))
instance ToDoc a => ToArgsDoc a where
    to_args_doc = parens . to_doc
instance ToDoc (a,b) => ToArgsDoc (a, b) where
    to_args_doc = to_doc
instance ToDoc (a,b,c) => ToArgsDoc (a, b, c) where
    to_args_doc = to_doc
-}
class ToDoc a where
    to_doc :: a -> Doc
    ls2doc :: [a] -> Doc
    ls2doc = brackets . comma_ls . fmap to_doc
instance ToDoc Doc where
    to_doc = id
instance ToDoc Int where
    to_doc = text . show
instance ToDoc Integer where
    to_doc = text . show
instance ToDoc Char where
    to_doc = text . show
    ls2doc = text . show
--instance ToDoc String where
--    to_doc = text . show
instance ToDoc a => ToDoc [a] where
    -- to_doc = brackets . comma_ls . fmap to_doc
    to_doc = ls2doc
instance ToDoc a => ToDoc (Maybe a) where
    to_doc (Just a)= py_call2doc "Just" $ A1 a
    to_doc Nothing = text "Nothing"
instance (ToDoc a, ToDoc b) => ToDoc (Either a b) where
    to_doc (Left a) = py_call2doc "Left" $ A1 a
    to_doc (Right a) = py_call2doc "Right" $ A1 a
instance (ToDoc a, ToDoc b) => ToDoc (Map a b) where
    to_doc = braces . comma_ls . fmap pair2dict_field . M.toList where
        pair2dict_field :: (ToDoc a, ToDoc b) => (a, b) -> Doc -- a : b
        pair2dict_field (a, b) = hsep [to_doc a, text ":", to_doc b]
instance ToDoc a => ToDoc (Set a) where
    to_doc = braces . comma_ls . fmap to_doc . S.toList









-- tuples
instance ToDoc () where
    to_doc = text . show
instance (ToDoc a, ToDoc b) => ToDoc (a, b) where
    to_doc (a, b) = parens $ comma_ls [to_doc a, to_doc b]
instance (ToDoc a, ToDoc b, ToDoc c) => ToDoc (a, b, c) where
    to_doc (a, b, c) = parens $ comma_ls [to_doc a, to_doc b, to_doc c]
instance (ToDoc a, ToDoc b, ToDoc c, ToDoc d) =>
    ToDoc (a, b, c, d) where
    to_doc (a, b, c, d) = parens $ comma_ls
        [to_doc a, to_doc b, to_doc c, to_doc d]
instance (ToDoc a, ToDoc b, ToDoc c, ToDoc d, ToDoc e) =>
    ToDoc (a, b, c, d, e) where
    to_doc (a, b, c, d, e) = parens $ comma_ls
        [to_doc a, to_doc b, to_doc c, to_doc d, to_doc e]






