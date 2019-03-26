{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-
untyped lambda syntax
    Expr = Abs | Apps
    Atom = Val | '(' Expr ')'
    Abs = '\' Val '.' Expr
    Apps = Atom+ Abs?   -- last one without ()
    token Val '(' ')' '\' '.'

    -- reduction -> lambda
    [x </- FV(t)] ==>> [(\x. t) s --> t] -- K
    [(\x. x) s --> s] -- I
    [x <- FV(\y. t)] ==>>
        [y </- FV(s)] ==>> [(\x. \y. t) s --> (\y. (\x. t) s)]
        [y <- FV(s)][z </- FV(s)\-/FV(\x.\y.t)] ==>>
            [(\x.\y.t) s
                --> (\z. (\x.\y.t) ((\y. s) z)) y
                --> (\z. \y. (\x.t) ((\y. s) z)) y
                --> not good!! we have to rename y in first arg!
                ]
                -- leftmost
            [(\y.t) z --> v] ==>> [(\x.\y.t) s
                --> (\x.(\z.(\y.t) z)) s
                --> (\x.(\z.v)) s
                ]
                -- leftmost: evalue ((\y.t) z) first
    [x <- FV(f e)] ==>> [(\x. f e) s --> (\x. f) s ((\x. e) s)] -- S
-}

module Lambda.UntypedLambdaTerm
    ( Term(..)
    , TermWO(..)
    , FV(..)
    , ForUntypedLambdaTermParser(..)
    -- , ID(..)
    , is_closed_term, getFV, mkVal, mkAbs, mkApp
    , untyped_lambda_term_parse_ex
    , untyped_lambda_term_parse
    , fixL
    , left_most_eval_lambda_term
    , left_most_eval_lambda_termIO
    , lambdaI, lambdaS, lambdaK, lambdaB, lambdaC
    , _mkApp_, _mkApps_, _mkApps
    , mkApps, mkAbss
    , show_lambda
    , str_const, str_K
    , str_flip, str_C
    , str_composition, str_B
    , str_I, str_S
    , str_app
    )
where

import qualified Data.Set as S
import Data.List (foldl1', foldl')
import qualified Text.Parsec as P
import qualified Data.Functor.Identity as P
import Text.Parsec (Stream, ParsecT, SourceName, ParseError)
import Data.Proxy
import Control.Exception (assert)
import Lambda.OpFreshID

type FV = S.Set

data Term id = Term Bool (FV id) (TermWO id)
    deriving (Read, Show)
    -- Bool: True -- a value, i.e. evalued
data TermWO id = Val id | Abs id (Term id) | App (Term id) (Term id)
    deriving (Read, Show)
{- error
instance Functor Term where
    fmap f (Term fv wo) = Term (fmap f fv) (fmap f wo)
instance Functor TermWO where
    fmap f (Val id) = Val (f id)
    fmap f (Abs id t) = Abs (f id) (fmap f t)
    fmap f (App t2 t1) = App (fmap f t1) (fmap f t2)
-}

is_closed_term :: Term id -> Bool
is_closed_term = S.null . getFV
getFV :: Term id -> FV id
getFV (Term _ fv _) = fv
getDone :: Term id -> Bool
getDone (Term b _ _) = b
isAbsWO :: TermWO id -> Bool
isAbsWO (Abs _ _) = True
isAbsWO _ = False

mkVal :: id -> Term id
mkVal id = Term True (S.singleton id) (Val id)
mkAbs :: Ord id => id -> Term id -> Term id
mkAbs id t@(Term b fv _) = Term b (S.delete id fv) (Abs id t)
mkApp :: Ord id => Term id -> Term id -> Term id
mkApp t1@(Term b1 fv1 wo1) t2@(Term b2 fv2 _)
    = Term b (fv1 `S.union` fv2) (App t1 t2) where
    -- fv1 = getFV t1
    fv2 = getFV t2
    b = b1 && b2 && not (isAbsWO wo1)
mkApps :: Ord id => Term id -> [Term id] -> Term id
mkApps = foldl' mkApp
mkAbss :: Ord id => [id] -> Term id -> Term id
mkAbss ls t = foldr mkAbs t ls


_mkApp_ :: Ord id => id -> id -> Term id
_mkApps_ :: Ord id => id -> [id] -> Term id
_mkApps :: Ord id => id -> [Term id] -> Term id
_mkApp_ f a = _mkApps_ f [a]
-- _mkApp_ f a = mkVal f `mkApp` mkVal a

_mkApps_ f = _mkApps f . map mkVal
_mkApps = mkApps . mkVal

str_const = "\\x.\\y. x"
str_K = str_const
str_flip = "\\f.\\x.\\y. f y x"
str_C = str_flip
str_composition = "\\f.\\g.\\x. f (g x)"
str_B = str_composition
str_I = "\\x.x"
str_S = "\\f.\\g.\\x. (f x) (g x)"
str_app = "\\f.\\a. f a" -- == I
lambdaI, lambdaS, lambdaK, lambdaB, lambdaC
    :: OpFreshID id => Term id
lambdaI = let x = fresh_id [] in mkAbs x $ mkVal x
lambdaS = let f:g:x:_ = fresh_ids [] in
    mkAbss [f, g, x] $ _mkApp_ f x `mkApp` _mkApp_ g x
lambdaK = let f:g:x:_ = fresh_ids [] in
    mkAbss [f, g] $ mkVal f
lambdaB = let f:g:x:_ = fresh_ids [] in
    mkAbss [f, g, x] $ mkVal f `mkApp` _mkApp_ g x
lambdaC = let f:g:x:_ = fresh_ids [] in
    mkAbss [f, g, x] $ _mkApp_ f x `mkApp` mkVal g
show_lambda :: OpStr id => Term id -> String
show_lambda = f (False, False) where
{-
    Expr = Abs | Apps
    Atom = Val | '(' Expr ')'
    Abs = '\' Val '.' Expr
    Apps = Atom+ Abs?   -- last one without ()
    token Val '(' ')' '\' '.'
-}
    -- (Bool, Bool) -- (abs need (), app need ())
    -- (False, False) - \y. <here> or outermost or (<here>)
    --                  abs/wrap/outermost
    -- (False, True) - (apps+ <here>) -- last
    -- (True, True) - (apps+ <here> apps+) -- middle
    -- (True, False) - (<here> apps+) -- first
    f bb (Term _ fv wo) = g bb wo
    g bb@(absB, appB) wo = case wo of
        App t1 t2 -> h appB $
            -- not outermost; not middle; not last
            -- first
                f (True, False) t1 ++ ' ' :
            -- not outermost; not first; middle or last
            -- if (t1 t2) outermost/wraped then last else middle
                f (not $ appB || bb ==  (False, False), True) t2
        Abs id t -> h absB $ '\\' : str id ++ "." ++ f (False, False) t
        Val id -> str id
    h b s = if b then '(' : s ++ ")" else s
    wrap s2s = ('(':) . s2s . (')':)


untyped_lambda_term_parse_ex
    :: ForUntypedLambdaTermParser s P.Identity id
    => SourceName -> s -> Either ParseError (Term id)
untyped_lambda_term_parse
    :: Stream s P.Identity Char
    => SourceName -> s -> Either ParseError (Term IDEX)
untyped_lambda_term_parse_ex = P.parse untyped_lambda_term_parser
untyped_lambda_term_parse = untyped_lambda_term_parse_ex
class (Stream s m Char, Ord id) => ForUntypedLambdaTermParser s m id where
    untyped_lambda_id :: ParsecT s u m id
    {-# MINIMAL untyped_lambda_id #-}

    untyped_lambda_lambda, untyped_lambda_dot
        , untyped_lambda_open, untyped_lambda_close
        :: proxy id -> ParsecT s u m Char
    untyped_lambda_abs, untyped_lambda_apps
        , untyped_lambda_val
        , untyped_lambda_atom
        :: ParsecT s u m (Term id)
    untyped_lambda_term, untyped_lambda_term_parser
        :: ParsecT s u m (Term id)

    untyped_lambda_lambda _ = P.char '\\'
    untyped_lambda_dot _ = P.char '.'
    untyped_lambda_open _ = P.char '('
    untyped_lambda_close _ = P.char ')'
    untyped_lambda_abs = do
        untyped_lambda_lambda by
        P.spaces
        id <- untyped_lambda_id
        P.spaces
        untyped_lambda_dot by
        P.spaces
        term <- untyped_lambda_term
        return $ mkAbs id term
        where by = Proxy :: Proxy id
    untyped_lambda_apps = do
        atoms <- P.many1 $ do
            a <- untyped_lambda_atom
            P.spaces
            return a
        may_abs <- P.optionMaybe untyped_lambda_abs
        let t1 = foldl1' mkApp atoms
            t = maybe t1 (mkApp t1) may_abs
        return t
    untyped_lambda_val = do
        id <- untyped_lambda_id
        return $ mkVal id
    untyped_lambda_atom = do
        untyped_lambda_val
            P.<|> P.between
                    (untyped_lambda_open by >> P.spaces)
                    (P.spaces >> untyped_lambda_close by)
                    untyped_lambda_term
        where by = Proxy :: Proxy id
    untyped_lambda_term = do
        untyped_lambda_abs P.<|> untyped_lambda_apps
    untyped_lambda_term_parser = P.spaces >> untyped_lambda_term


instance (Stream s m Char) => ForUntypedLambdaTermParser s m ID where
    untyped_lambda_id =
        P.many1 (P.alphaNum P.<|> P.oneOf "_") >>= return . mkID

parseNat :: Stream s m Char => ParsecT s u m Integer
parseNat = do
    h <- P.oneOf ['1'..'9']
    ts <- P.many1 P.digit
    return . read $ h:ts
instance (Stream s m Char) => ForUntypedLambdaTermParser s m IDEX where
    untyped_lambda_id = do
        name <- P.many1 (P.alphaNum P.<|> P.oneOf "_")
        -- no spaces
        let sep = P.char sepIDEX
        may_n <- P.optionMaybe $ P.between sep sep parseNat
        return $ mkIDEX name $ maybe 0 id may_n


left_most_eval_lambda_termIO :: (Ord id, OpFreshID id, OpStr id) => Term id -> IO (Term id)
left_most_eval_lambda_termIO = f where
    f t@(Term done fv wo) = do
        output <- ff t
        if not done then do
            print $ show_lambda t
            print $ "   " ++ show_lambda output
        else return ()
        return output
    ff input@(Term done fv wo) = if done then return input else
        case wo of
            Val _ -> return input
            Abs id body -> fmap (mkAbs id) $ f body
            App t1 t2 -> do
                t1' <- f t1
                app t1' t2

    -- app (evalued) (unevalued)
    app t1@(Term True fv1 wo1) t2 = do
        t <- _app t1 t2
        if getDone t2 == True then return ()
        else do
            print $ "app" ++ ' ' : show_lambda t1 ++ ";  " ++ show_lambda t2
            print $ "-> " ++ ' ' : show_lambda t
        return t
    _app t1@(Term True fv1 wo1) t2 = case wo1 of
      Abs id body@(Term True body_fv body_wo) ->
        if id `S.notMember` body_fv then return body else -- K
        case body_wo of
        -- (\id. id') t2
          Val id' -> assert (id == id') $ f t2 -- I
        -- (\id. s1 s2) t2 = ((\id. s1) t2) ((\id. s2) t2)
          App s1 s2 -> do
            t2' <- f t2
            -- g t = (\id. t) t2
            let g t = mkAbs id t `mkApp` t2'
            s1' <- f $ g s1
            app s1' $ g s2
          Abs id'' body'' -> assert (id /= id'') $
            if not $ id'' `S.member` getFV t2 then do
                -- (\x.\y.b) t2 ==>> (\y.(\x.b) t2)
                new_body <- app (mkAbs id body'') t2
                return $ mkAbs id'' new_body
            else do
                -- (\x.\y.b) t2 ==>> (\x.\z.!((\y.b) z)) t2
                -- bug: let z = fresh_id [S.singleton id, getFV t2]
                let z = fresh_id [S.singleton id, getFV t2, getFV body]
                new_body <- fmap (mkAbs z) $ app body (mkVal z)
                app (mkAbs id new_body) t2
      _ -> fmap (mkApp t1) $ f t2

left_most_eval_lambda_term :: (Ord id, OpFreshID id, OpStr id) => Term id -> Term id
left_most_eval_lambda_term = ff where
    ff input =
        if (getFV output /= getFV input) then
            error $ show_lambda input ++ '\n' : show_lambda output
        else
            assert (getDone output == True) $
            assert (getFV output == getFV input) $
            output
        where output = f input
    f input@(Term done fv wo) = if done then input else
        case wo of
            Val _ -> error "ValueError: Lambda Term: getDone (Val id) == False"
            Abs id body -> mkAbs id $ f body
            App t1 t2 ->
                let t1' = f t1 in
                app t1' t2

    -- app (evalued) (unevalued)
    app t1@(Term True fv1 wo1) t2 = case wo1 of
      Abs id body@(Term True body_fv body_wo) ->
        if id `S.notMember` body_fv then body else -- K
        case body_wo of
        -- (\id. id') t2
          Val id' -> assert (id == id') $ f t2 -- I
        -- (\id. s1 s2) t2 = ((\id. s1) t2) ((\id. s2) t2)
          App s1 s2 ->
            let t2' = f t2
                -- g t = (\id. t) t2
                g t = mkAbs id t `mkApp` t2'
                s1' = f $ g s1
            in  app s1' $ g s2
          Abs snd_id snd_body -> assert (id /= snd_id) $
            if snd_id `S.notMember` getFV t2 then
                -- (\x.\y.b) t2 ==>> (\y.(\x.b) t2)
                let new_body = app (mkAbs id snd_body) t2
                in  mkAbs snd_id new_body
            else
                -- (\x.\y.b) t2 ==>> (\x.\z.!((\y.b) z)) t2
                let z = fresh_id [S.singleton id, getFV t2, getFV body]
                    new_body = mkAbs z $ app body (mkVal z)
                in  assert (z /= id && z /= snd_id && z `S.notMember` getFV t2)
                    $ app (mkAbs id new_body) t2
      _ -> mkApp t1 $ f t2






Right fixL = untyped_lambda_term_parse "<>"
    "\\x2x. (\\rec_x. x2x (rec_x rec_x)) (\\rec_x. x2x (rec_x rec_x))"

main = do
    print $ untyped_lambda_term_parse "<>" "t"
    print $ untyped_lambda_term_parse "<>" "t t2"
    print $ untyped_lambda_term_parse "<>" "\\x.t"
    print $ untyped_lambda_term_parse "<>" "\\x. t ((\\y. y) z) x"
    print "\n"
    print fixL


