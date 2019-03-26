
{-

combinators

    r'''
T[x] => x
T[(f e)] => (T[f] T[e])
T[\x->e] => (K T[e]) (if x is not free in e)
T[\x->x] => I
T[\x->\y->e] => T[\x->T[\y->e]] (if x is free in e)
+ eta-reduction: T[\x->(f x)] = T[f] (if x is not free in f)
    <==> B I e == e; B e I == e
T[\x->(f e)] => (S T[\x->f] T[\x->e]) (if x is free in both f and e)
T[\x->(f e)] => (C T[\x->f] T[e]) (if x is free in f but not e)
T[\x->(f e)] => (B T[f] T[\x->e]) (if x is free in e but not f)
'''



-}

module Lambda.CombinatorSK where
import Lambda.UntypedLambdaTerm
    ( Term(..), TermWO(..), FV
    , untyped_lambda_term_parse, is_closed_term
    )
import qualified Data.Set as S
import Data.List (foldl1')

---------------
data Combinator = S | K | AppC Combinator Combinator
    deriving (Read, Show)
data TermNoAbs id = TS | TK | TermNoAbs (FV id) (TermNoAbsWO id)
    deriving (Read, Show)
data TermNoAbsWO id = TValC id | TAppC (TermNoAbs id) (TermNoAbs id)
    deriving (Read, Show)
show_combinator :: Combinator -> String
show_combinator = f False where
    f b (AppC c1 c2)= s where
        s1 = f False c1
        s2 = f True c2
        s = if b then '(' : s1 ++ ' ' : s2 ++ ")" else s1 ++ ' ' : s2
    f _ t = show t
eval_combinator :: Combinator -> Combinator
eval_combinator = f where
    f (AppC c1 c2) = c where
        a1 = eval_combinator c1
        a2 = eval_combinator c2
        c = case a1 of
            -- S (K a)(K b)==K(a b)
            -- S K x==I
            -- 
            AppC K a1' -> a1'
            AppC (AppC S f) g -> eval_combinator
                                $ AppC (AppC f a2) (AppC g a2)
            _ -> AppC a1 a2
    f c = c

lambda2combinator :: (Monad m, Ord id) => Term id -> m Combinator
lambda2combinator = f where
    f t = if is_closed_term t
            then term_no_abs2combinator $ remove_abs t
            else fail "lambda2combinator: input term is not closed"
term_no_abs2combinator :: Monad m => TermNoAbs id -> m Combinator
term_no_abs2combinator = f where
    f (TermNoAbs fv wo) =
        if S.null fv then wo2c wo else fail "FV /= { }"
    f TS = return S
    f TK = return K
    wo2c (TAppC t1 t2) = do
        t1' <- f t1
        t2' <- f t2
        return $ AppC t1' t2'


mkTI :: TermNoAbs id
mkTI = foldl1' mkTAppC [TS, TK, TK]
    where
        mkTAppC t1 t2 = TermNoAbs S.empty (TAppC t1 t2)
mkTValC :: id -> TermNoAbs id
mkTValC id = TermNoAbs (S.singleton id) (TValC id)
getFVC :: TermNoAbs id -> FV id
getFVC (TermNoAbs fv _) = fv
getFVC _ = S.empty
mkTAppC :: Ord id => TermNoAbs id -> TermNoAbs id -> TermNoAbs id
mkTAppC t1 t2 = TermNoAbs (fv1 `S.union` fv2) (TAppC t1 t2) where
    fv1 = getFVC t1
    fv2 = getFVC t2

{-
    remove_abs :: space O(n) -> O(f(n))
    sub :: 1 -> O(n) -> O(g(n))
    remove_abs
        Val :: 1 -> f 1
        App :: 1+n+m -> f n + f m <= f (1+n+m)
        Abs :: 1+n -> g (f n) <= f (1+n)
    sub
        TS :: 1 -> 1
        TK :: 1 -> 1
        TermNoAbs
            TValC :: 1 -> 1
            TAppC :: 1+n+m -> 3 + g n + g m <= g (1+n+m)
    ==>> g n = 3*n
    ==>> 3*(f n) <= f (1+n)
    ==>> f n = 3^n
    O(3^n)!!
    O(n^2)!!
-}
remove_abs :: Ord id => Term id -> TermNoAbs id
remove_abs (Term _ (Val id)) = mkTValC id
remove_abs (Term _ (App t1 t2)) = mkTAppC (remove_abs t1) (remove_abs t2)
remove_abs (Term _ (Abs id _t)) = sub id (remove_abs _t) where
    sub id t = case t of
        TermNoAbs fv wo ->
            let t' = case wo of
                    TValC id' -> -- mkTI -- I
                        -- assert id == id'
                        if id == id' then mkTI
                            else error "ValueError: FV(\\x.y) == { }"
                    TAppC t1 t2 -> foldl1' mkTAppC [TS, sub id t1, sub id t2]
            in  if id `S.member` fv then t' else mkTAppC TK t
        -- bug:
        -- TS -> TS; TK -> TK
        p -> mkTAppC TK t


main = do
    let Right t = untyped_lambda_term_parse "<>"
         "\\x2x. (\\rec_x. x2x (rec_x rec_x)) (\\rec_x. x2x (rec_x rec_x))"
        Right c = lambda2combinator t
    print $ show_combinator c
    print $ show_combinator $ eval_combinator c


