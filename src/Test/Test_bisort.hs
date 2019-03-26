{-# LANGUAGE Arrows #-}

{-
    test bisort in
        “A New Notation for Arrows”, Ross Paterson, in ICFP, Sep 2001
    bisort is error!!!!!!!!
-}
module Test.Test_bisort

where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

type Pair a = (a, a)
data Hom a b = (a -> b ) :&: Hom (Pair a) (Pair b)

data BalTree a = Zero a | Succ (BalTree (Pair a))
    deriving (Eq, Ord, Show)
apply :: Hom a b -> BalTree a -> BalTree b
apply (f :&: fs ) (Zero x ) = Zero (f x )
apply (f :&: fs ) (Succ t ) = Succ (apply fs t )


instance Category Hom where
    (g :&: gs ) . (f :&: fs ) = (g . f ) :&:(fs >>> gs)
    id = arr id
instance Arrow Hom where
    arr f = f :&: arr (f *** f )
    --(f :&: fs ) >>> (g :&: gs ) = (g . f ) :&:(fs >>> gs )
    first (f :&: fs ) = first f :&:
        (arr transpose >>> first fs >>> arr transpose )
transpose :: ((a, b ), (c, d )) -> ((a, c), (b, d ))
transpose ((a, b ), (c, d )) = ((a, c), (b, d ))




scan :: (a -> a -> a ) -> a -> Hom a a
scan add b = id :&: proc (x, y) -> do
    y' <- scan add b  -< x `add` y
    yl <- rsh b  -< y'
    returnA  -< (yl `add` x, y')




rsh :: a -> Hom a a
rsh b = const b :&: proc (x, y) -> do
    yl <- rsh b  -< y
    returnA -< (yl, x)




butterfly :: (Pair a -> Pair a) -> Hom a a
butterfly f = id :&: proc (x, y) -> do
    x' <- butterfly f  -< x
    y' <- butterfly f  -< y
    returnA -< f (x', y')



rev :: Hom a a
rev = butterfly swap
unriffle :: Hom (Pair a) (Pair a)
unriffle = butterfly transpose


bisort :: Ord a => Hom a a
bisort = butterfly cmp
    where cmp (x, y ) = (min x y, max x y)


addTree :: BalTree a -> BalTree a -> Maybe (BalTree a)
addTree (Zero a) (Zero b) = Just . Succ $ Zero (a,b)
addTree (Succ lhs) (Succ rhs) = do
    t <- addTree lhs rhs
    return $ Succ t
addTree _ _ = Nothing
list2BalTree :: Int -> [a] -> Maybe (BalTree a, [a])
list2BalTree _ [] = Nothing
list2BalTree i ls | i < 0 = Nothing
list2BalTree 0 (h:ts) = Just (Zero h, ts)
list2BalTree i ls = do
    let i' = pred i
    (treel, ls') <- list2BalTree i' ls
    (treer, ls'') <- list2BalTree i' ls'
    t <- addTree treel treer
    return (t, ls'')

t,t' :: BalTree Int
t = let Just (a,[]) = list2BalTree 2 [1,3,2,4] in a
t' = let Just (a,[]) = list2BalTree 2 [4,2,3,1] in a
pr = do
    print $ apply bisort t
    print $ apply bisort t == t
    print $ apply bisort t' == t



