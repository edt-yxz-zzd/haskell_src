{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Seed.ArrowOps
    ( module Seed.ArrowOps
    , module Seed.OpSwap
    )
where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
--import Seed.PairOps (pair_flip)
import Seed.ListOps hiding (uncons)
import Seed.EitherOps
import Seed.PairOps
import Seed.MaybeOps
import Numeric.Natural
import Seed.OpSwap
import Container.IContainer (Element)
import Container.IStream(IStream (..))
import Container.Instances__List()
import Seed.Types (Op)


--fork :: i -> (i, i)
--switch :: bool -> i -> Either i i

-- forkA >>> i2a *** i2b === i2a &&& i2b
forkA :: Arrow arr => arr i (i,i)
forkA = arr fork

-- either
switchA :: Arrow arr => arr i Bool -> arr i (Either i i)
switchA i2b = i2bi >>> bi2eii where
    i2bi = withInput i2b
    bi2eii = arr $ uncurry switch
unEitherA :: Arrow arr => arr (Either i i) i
unEitherA = arr unEither
doubleA :: ArrowPlus arr => arr i i
doubleA = bothA >>> unEitherA
bothA :: ArrowPlus arr => arr i (Either i i)
bothA = arr Left <+> arr Right
eitherA
    :: (ArrowChoice arr, ArrowPlus arr)
    => (i2a -> i2b -> arr (Either i i) (Either a b))
    -> (i2a -> i2b -> arr i (Either a b))
eitherA op i2a i2b = bothA >>> (i2a `op` i2b)

maybe2eitherA :: Arrow arr => arr (Maybe a) (Either () a)
maybe2eitherA = arr maybe2either
i2mo_to_i2iEo :: Arrow arr => arr i (Maybe o) -> arr i (Either i o)
i2mo_to_i2iEo i2mo = i2iEo where
    i2mo_i = withInput i2mo
    mo_i_to_iEo (Just o, _) = Right o
    mo_i_to_iEo (_, i) = Left i
    i2iEo = i2mo_i >>^ mo_i_to_iEo

unconsA :: (Arrow arr, IStream s, a ~ Element s) => arr s (Maybe (a, s))
unconsEA :: (Arrow arr, IStream s, a ~ Element s) => arr s (Either s (a, s))
unconsZA
    :: (ArrowZero arr, ArrowChoice arr, IStream s, a ~ Element s)
    => arr s (a, s)
headZA
    :: (ArrowZero arr, ArrowChoice arr, IStream s, a ~ Element s)
    => arr s a
tailZA
    :: (ArrowZero arr, ArrowChoice arr, IStream s)
    => arr s s
stream2plusZA
    :: (ArrowPlus arr, ArrowChoice arr, IStream s, a ~ Element s)
    => arr s a
unconsA = arr uncons
unconsEA = (id &&& unconsA) >>> arr s_mas2sEas where
    s_mas2sEas (_, Just a_s) = Right a_s
    s_mas2sEas (s, _) = Left s
unconsZA = unconsEA >>> (zeroArrow ||| id)
headZA = unconsZA >>^ fst
tailZA = unconsZA >>^ snd
stream2plusZA = unconsZA >>> bothA >>> (arr fst ||| (snd ^>> stream2plusZA))
list2plusZA :: (ArrowPlus arr, ArrowChoice arr) => arr [a] a
list2plusZA = stream2plusZA


--- pair
ab_c2a_bcA :: Arrow arr => arr ((a,b), c) (a, (b,c))
a_bc2ab_cA :: Arrow arr => arr (a, (b,c)) ((a,b), c)
ab_c2a_bcA = arr ab_c2a_bc
a_bc2ab_cA = arr a_bc2ab_c



ab2baA :: Arrow arr => arr (a,b) (b,a)
ab2baA = arr ab2ba

ab_c2ac_bA :: Arrow arr => arr ((a,b),c) ((a,c),b)
ab_c2ac_bA = arr ab_c2ac_b
ab_c2ac_cA :: Arrow arr => arr ((a,b),c) ((a,c),c)
ab_c2ac_cA = arr ab_c2ac_c

ab2bbA :: Arrow arr => arr (a,b) (b,b)
ab2aaA :: Arrow arr => arr (a,b) (a,a)
ab2_aA :: Arrow arr => arr (a,b) ((),a)
ab2bbA = arr ab2bb
ab2aaA = arr ab2aa
ab2_aA = arr ab2_a



----------------

idA :: Arrow arr => arr i i
idA = returnA
constA :: Arrow arr => o -> arr i o
constA = arr . const
flipA :: Arrow arr => arr (a,b) o -> arr (b,a) o
flipA a = swapA >>> a
list1x2list :: (a, [a]) -> [a]
list1x2list (h, ts) = h:ts
list1x2listA :: Arrow arr => arr (a, [a]) [a]
list1x2listA = arr list1x2list
sequenceArr :: Arrow arr => [arr i o] -> arr i [o]
sequenceArr (h:ts) = (h &&& sequenceArr ts) >>> list1x2listA
sequenceArr [] = constA []
discardArr, execA :: Arrow arr => arr i x -> arr i i
discardArr a = withInput a >>> arr snd
execA = discardArr
{-
voidArr :: Arrow arr => arr i ()
voidArr = constA ()
-}
voidArr :: Arrow arr => arr i x -> arr i ()
voidArr a = a >>> constA ()

liftA2 :: Arrow arr => (a->b->c) -> arr i a -> arr i b -> arr i c
liftA2 op a b = (a &&& b) >>> arr (uncurry op)
--foldrA :: Arrow arr => (arr (a,o) o) -> arr i ([a], o) -> arr i o
foldrA :: Arrow arr => (a->o->o) -> arr i ([a], o) -> arr i o
foldrA op a = a >>> arr f where
    f (ls, o) = foldr op o ls

withInput, i2o_to_i2oi :: Arrow arr => arr i o -> arr i (o, i)
i2o_to_i2oi = (&&& returnA)
withInput = i2o_to_i2oi
--ifArr :: Arrow arr => arr i Bool -> arr i a -> arr i b -> arr i' i -> arr i' (Either a b)

bool_a2either, not_bool_a2either :: (Bool, a) -> Either a a
bool_a2either (b, a) = if b then Right a else Left a
not_bool_a2either (b, a) = if b then Left a else Right a
ifArr :: ArrowChoice arr => arr i Bool -> arr i a -> arr i b -> arr i (Either a b)
ifArr_ :: Arrow arr => arr i Bool -> arr i (Either i i)
ifArr_ i2Bool = withInput i2Bool >>> arr not_bool_a2either
ifArr i2Bool i2a i2b = ifArr_ i2Bool >>> (i2a +++ i2b)



--------------- helper
mk_app
    :: (ArrowApply arr, arI ~ (ar i o, i), arrI ~ (arr i o, i))
    => (arr arI o -> ar arI o) -> (ar i o -> arr i o) -> ar arI o
mk_app mkAr unAr = mkAr $ first (arr unAr) >>> app where
    f (i2oAr, i) = (unAr i2oAr, i)
mk_arr_plus
    :: (ArrowPlus arr)
    => (new_arr i o -> new_arr e o -> new_arr i o)
    -> (arr i' o' -> new_arr i o)
    -> (arr e' o' -> new_arr e o)
    -> (new_arr i o -> arr i' o')
    -> Op (new_arr i o)
mk_arr_plus catchA_ box_i2o box_e2o unbox a b = box_i2o $ f a <+> f b where
        f a = unbox . catchA_ a $ box_e2o zeroArrow



mkKleisli :: m o -> Kleisli m i o
mkKleisli = Kleisli . const


instance Arrow arr => Arrow2Functor arr where
class Arrow arr => Arrow2Functor arr where
    fmapA2M :: (a->b) -> arr i a -> arr i b
    fmapA2M a2b i2a = i2a >>> arr a2b
instance Arrow arr => Arrow2Applicative arr where
class Arrow arr => Arrow2Applicative arr where
    pureA2M :: o -> arr i o
    pureA2M = arr . const
    (<**>) :: arr i (a->b) -> arr i a -> arr i b
    (<**>) i2_a2b i2a = i2b where
        i2__a2b_a = i2_a2b &&& i2a
        i2b = i2__a2b_a >>> arr a2b_a2b
        a2b_a2b (a2b, a) = a2b a
instance ArrowApply arr => Arrow2Monad arr where
class ArrowApply arr => Arrow2Monad arr where
    returnA2M :: o -> arr i o
    returnA2M = pureA2M
    (>>==) :: arr i a -> (a -> arr i b) -> arr i b
    (>>==) i2a a2i2b = i2b where
        i2_i2b = i2a >>> arr a2i2b
        i2__i2b_i = withInput i2_i2b
        i2b = i2__i2b_i >>> app
infixl 1 >>==
infixl 4 <**>










--------
unJustA :: (ArrowZero arr, ArrowChoice arr) => arr (Maybe i) i
unJustA = maybe2either ^>> (zeroArrow ||| id)
assertA :: (ArrowZero arr, ArrowChoice arr) => (i->Bool) -> arr i i
assertA pred = switchA (arr pred) >>> (zeroArrow ||| id)
----- parser
betweenA :: Arrow arr => arr i x -> arr i o -> arr i y -> arr i o
betweenA x o y = (x &&& o &&& y) >>> arr snd >>> arr fst
countA :: Arrow arr => Natural -> arr i a -> arr i [a]
countA n = sequenceArr . genericReplicate n

---- plus, now move to OpArrowPlusBy

manyA :: ArrowPlus arr => arr i a -> arr i [a]
manyA a = many1A a <+> constA []
many1A :: ArrowPlus arr => arr i a -> arr i [a]
many1A a = (a &&& manyA a) >>> arr list1x2list

choiceA :: ArrowPlus arr => [arr i a] -> arr i a
choiceA = foldr (<+>) zeroArrow

optionA :: ArrowPlus arr => a -> arr i a -> arr i a
optionA a i2a = i2a <+> constA a
optionalA :: ArrowPlus arr => arr i a -> arr i ()
optionalA a = voidArr a <+> constA ()

skipManyA :: ArrowPlus arr => arr i a -> arr i ()
skipManyA = voidArr . manyA
skipMany1A :: ArrowPlus arr => arr i a -> arr i ()
skipMany1A = voidArr . many1A
sepByA, sepBy1A, endByA, endBy1A :: ArrowPlus arr => arr i a -> arr i sep -> arr i [a]
sepByA a sep = sepBy1A a sep <+> constA []
sepBy1A a sep = (a &&& sep &&& sepByA a sep) >>> arr tols where
    tols (a, (sep, ls)) = a:ls
endByA a sep = fmapA2M (map fst) $ manyA (a &&& sep)
endBy1A a sep = fmapA2M (map fst) $ many1A (a &&& sep)

--chainrA :: ArrowPlus arr => arr i a -> arr (a,a) a -> a -> arr i a
chainrA :: ArrowPlus arr => arr i a -> (a->b->b) -> b -> arr i b
chainrA a op b0 = fmapA2M (foldr op b0) $ manyA a
chainlA :: ArrowPlus arr => arr i a -> (b->a->b) -> b -> arr i b
chainlA a op b0 = fmapA2M (foldl op b0) $ manyA a

chainr1A :: ArrowPlus arr => arr i a -> (a->a->a) -> arr i a
chainr1A a op = fmapA2M (foldr1 op) $ many1A a
chainl1A :: ArrowPlus arr => arr i a -> (a->a->a) -> arr i a
chainl1A a op = fmapA2M (foldl1 op) $ many1A a

manyTillA :: ArrowPlus arr => arr i a -> arr i end -> arr i [a]
manyTillA a end = (manyA a &&& end) >>> arr fst








