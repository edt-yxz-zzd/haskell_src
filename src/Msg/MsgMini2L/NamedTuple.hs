{-# LANGUAGE FlexibleContexts
    , UndecidableInstances

    , TypeOperators
    #-}

{-
    why not use record syntax?
    I cannot control how to show it
    maybe I should try Generic...
-}
module NamedTuple where
import ToDoc
import Text.PrettyPrint hiding (char, space)


-- data FieldPair a b = FieldPair a b
-- ":...:" type (-:) = (,)
(-:) = (,)
data Tuple a o t = (t, a) :-: (o t) -- Con (t, a) (o t)
data Tuple0 t = Tuple0
type a :-: o = Tuple a o
type a :-:: b = a :-: b :-: Tuple0
type a :$: b = a b
(///) :: (t, a) -> (t, b) -> a :-:: b :$: t -- Tuple a (Tuple b Tuple0) t
a /// b = a // b // Tuple0
(//) :: (t, a) -> tpl t -> a :-: tpl :$: t -- Tuple a tpl t
(//) = (:-:) -- Con
infixr 8 ///
infixr 8 //
infixr 9 -:
infixr 9 :-:
infixr 9 :-::
infixr 0 :$:
-- str_parens s = "(" ++ s ++ ")"
-- show_field (t, a) = str_parens (show t) ++ "-:" ++ str_parens (show a)
class NamedTupleCls t where
    to_tuple_field_docs :: t -> [Doc] -- without "(" ")"
instance (NamedTupleCls (o t), ToDoc a, ToDoc t)
    => NamedTupleCls (Tuple a o t) where
    to_tuple_field_docs (ta :-: ot) -- (Con ta ot)
      = fieldpair2doc ta : to_tuple_field_docs ot where
        fieldpair2doc (t, a) = hsep [to_doc t, text "-:", to_doc a]
        -- sep [to_doc t, text " -: ", to_doc a]
instance NamedTupleCls (Tuple0 t) where
    to_tuple_field_docs _ = []
instance ToDoc (Tuple0 t) where
    to_doc _ = text "()"
instance NamedTupleCls (Tuple a o t) => ToDoc (Tuple a o t) where
    to_doc = parens . comma_ls . to_tuple_field_docs
instance NamedTupleCls (Tuple a o t) => Show (Tuple a o t) where
    show = show . to_doc














{-
data NamedTuple a n t = Tagged t a (n t)
data EmptyNamedTuple t = EmptyNamedTuple
type NamedTuple0 = EmptyNamedTuple
type NamedTuple1 a = NamedTuple a NamedTuple0
type NamedTuple2 a b = NamedTuple a (NamedTuple1 b)
type NamedTuple3 a b c = NamedTuple a (NamedTuple2 b c)
makeNamedTuple0 :: NamedTuple0 t
makeNamedTuple0 = EmptyNamedTuple
makeNamedTuple1 :: t -> a -> NamedTuple1 a t
makeNamedTuple1 t a = Tagged t a makeNamedTuple0

class NamedTpl (n :: * -> *) where -- out | out -> n where
    -- | (MakeTpl n t (n t)) -> n where
    type MakeTpl n t out -- :: * -> * -- out
    --make_tpl :: MakeTpl n t (n t) -- t -> a ... -> n t
    -- what! 'n' not in scope?????????
    -- default 
    -- make_tpl = (make_p :: forall t. (n t -> n t) -> MakeTpl n t (n t)) id
    make_p :: (n t -> out) -> MakeTpl n t out -- t -> a ... -> out
make_tpl :: forall n a c t. (NamedTpl c, n ~ NamedTuple a c) => t -> a -> MakeTpl c t (n t)
make_tpl    = (make_p :: forall . (n t -> n t) -> MakeTpl n t (n t)) id


instance NamedTpl EmptyNamedTuple where
    type MakeTpl EmptyNamedTuple t out = out -- EmptyNamedTuple t
    make_p f = f EmptyNamedTuple
instance NamedTpl n => NamedTpl (NamedTuple a n) where
    type MakeTpl (NamedTuple a n) t out = t -> a -> MakeTpl n t out
    make_p f t a = make_p (\nt_ -> f $ Tagged t a nt_)

tpl = make_tpl 1 'a' 2 "" 3 0 :: NamedTuple3 a b c t
-}






