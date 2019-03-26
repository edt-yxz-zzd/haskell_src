{-# LANGUAGE FunctionalDependencies #-}
class C t a b c | t->a b c, a b c->t where
    get0 :: t->a
    get1 :: t->b
    get2 :: t->c

instance C (a, b, c) a b c where
    get0 (a, _, _) = a
    get1 (_, b, _) = b
    get2 (_, _, c) = c


