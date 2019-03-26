{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}
-}

module ADT.ArrowCC
where

--import Control.Monad.Trans.Cont
import Control.Arrow
import qualified Control.Category as Cat
import Control.Category (Category)
import Seed.ArrowOps (Arrow2Functor(..), Arrow2Applicative(..), Arrow2Monad(..))
{-
import qualified Control.Arrow as A
import Control.Arrow (Arrow)
--import qualified Prelude as P
--import Prelude hiding ((.), id)
-}
import Seed.Boxed
import Seed.Utils ((...))
import Seed.EitherOps (either_merge, either_flip, bimap, isLeft)
import Seed.PairOps (pair_flip, bimap)
import Data.Semigroup
import Data.Monoid



i2o_to_i2io :: Arrow arr => arr i o -> arr i (i,o)
i2o_to_i2io a = returnA &&& a
discardA :: Arrow arr => arr i o -> arr i i
discardA a = i2o_to_i2io a >>> arr fst
i2o_to_ix2o :: Arrow arr => arr i o -> arr (i,x) o
i2o_to_ix2o a = fst ^>> a
i2o_to_Eix2o :: (ArrowZero arr, ArrowChoice arr)
    => arr i o -> arr (Either i x) o
i2o_to_Eix2o a = a ||| zeroArrow
x2a_to_xb2ab :: (Arrow arr)
    => (x->a) -> arr (x, b) (a, b)
x2a_to_xb2ab = first . arr
x2a_ab2c_to_xb2c :: (Arrow arr)
    => (x->a) -> arr (a, b) c -> arr (x, b) c
x2a_ab2c_to_xb2c x2a a = first (arr x2a) >>> a

class Arrow (ArrowCC2Arrow arr) => ArrowCC arr where
    type ArrowCC2Arrow arr :: * -> * -> *
    type ArrowCC2Arrow arr = (->)
    idCC :: arr r a a
    idCC = arrCC_ex returnA
    (>>>>) :: arr r i x -> arr r x o -> arr r i o

    firstCC :: arr r i o -> arr r (i, x) (o, x)
    arrCC_ex :: ArrowCC2Arrow arr i o -> arr r i o
    exitCC_ex :: arr r i r -> arr r i y


    arrCC :: (i -> o) -> arr r i o
    arrCC = arrCC_ex . arr
    exitCC :: r -> arr r i y
    exitCC = exitCC_ex . arrCC . const
    callCC :: ((r -> arr r i y) -> arr r i r) -> arr r i r
    callCC f = f exitCC

    switchCC :: arr r i o -> arr o i r
    mapCC
        :: (ArrowCC2Arrow arr i x -> ArrowCC2Arrow arr i o)
        -> (arr r i x -> arr r i o)
    discardCC :: arr r i o -> arr r i i
    discardCC = mapCC discardA
    discardCC_ex :: arr r i ox -> arr r i o -> arr r i o
    discardCC_ex arr = (discardCC arr >>>>)


    runCC_ex :: arr r o r -> arr r i o -> ArrowCC2Arrow arr i r
    runCC_ex o2r i2o = runCC $ i2o >>>> o2r
    runCC :: arr r i r -> ArrowCC2Arrow arr i r
    runCC = runCC_ex idCC
    runCC_exR :: arr o r o -> arr r i o -> ArrowCC2Arrow arr i o
    runCC_exR r2o i2o = runCC_ex r2o $ switchCC i2o
    rename_resultCC :: arr o i o -> arr r i o
    rename_resultCC = arrCC_ex . runCC

    {-# MINIMAL ((>>>>), firstCC, arrCC_ex, exitCC_ex
                , switchCC, mapCC, (runCC_ex | runCC)) #-}
-----------
newtype ArrCC arr r i o = ArrCC { unArrCC :: Either (arr i r) (arr i o) }

instance Boxed (ArrCC arr r i o) where
    type BoxedFrom (ArrCC arr r i o) = Either (arr i r) (arr i o)
    box = ArrCC
    unbox = unArrCC

runArrCC :: ArrCC arr r i r -> arr i r
runArrCC = either_merge . unbox
composeArrCC
    :: (ArrCC ar ~ arr, Category ar) => arr r i x -> arr r x o -> arr r i o
composeArrCC lhs rhs = box $ case (unbox lhs, unbox rhs) of
    (Left i2r, _) -> Left i2r
    (Right i2x, Left x2r) -> Left $ i2x >>> x2r
    (Right i2x, Right x2o) -> Right $ i2x >>> x2o
idArrCC :: Category arr => ArrCC arr r a a
idArrCC = box . Right $ Cat.id
firstArrCC
    :: (ArrCC ar ~ arr, Arrow ar) => arr r i o -> arr r (i, x) (o, x)
firstArrCC = mapGBox $ bimap i2o_to_ix2o first

instance Arrow arr => ArrowCC (ArrCC arr) where
    type ArrowCC2Arrow (ArrCC arr) = arr
    (>>>>) = composeArrCC
    firstCC = firstArrCC
    arrCC_ex = box . Right
    exitCC_ex = box . Left . runArrCC

    mapCC = mapGBox . fmap
    switchCC = mapGBox either_flip
    runCC = runArrCC



instance Category arr => Category (ArrCC arr r) where
    (.) = flip composeArrCC
    id = idArrCC
instance Arrow arr => Arrow (ArrCC arr r) where
    arr = arrCC
    first = firstCC

{-
plusArrCC
    :: (ArrCC ar ~ arr, ArrowPlus ar) => arr r i o -> arr r i o -> arr r i o
plusArrCC lhs rhs = box $ case (unbox lhs, unbox rhs) of
    (Right i2r, Right i2r') -> Right $ i2r <+> i2r'
    (Left i2o, Left i2o') -> Left $ i2o <+> i2o'
    -- ???????????
    (Right i2r, _) -> Right i2r
    (_, Right i2r) -> Right i2r
    {-
    (Left i2o, _) -> Left i2o
    (_, Left i2o) -> Left i2o
    -}
-}
plusArrCC
    :: (ArrCC ar ~ arr) => arr r i o -> arr r i o -> arr r i o
plusArrCC lhs rhs = if isLeft $ unbox lhs then lhs else rhs
instance ArrowZero arr => ArrowZero (ArrCC arr r) where
    zeroArrow = box . Right $ zeroArrow
instance ArrowZero arr => ArrowPlus (ArrCC arr r) where
    (<+>) = plusArrCC
instance ArrowZero arr => Semigroup (ArrCC arr r i o) where
    (<>) = plusArrCC
instance ArrowZero arr => Monoid (ArrCC arr r i o) where
    mappend = (<+>)
    mempty = zeroArrow



instance (ArrowZero arr, ArrowChoice arr) => ArrowChoice (ArrCC arr r) where
    left = mapGBox $ bimap i2o_to_Eix2o left
instance (ArrowZero arr, ArrowApply arr) => ArrowApply (ArrCC arr r) where
    app = arrCC_ex $ x2a_ab2c_to_xb2c (runCC_exR zeroArrow) app

instance (ArrowZero arr, ArrowLoop arr) => ArrowLoop (ArrCC arr r) where
    loop = arrCC_ex . loop . runCC_exR zeroArrow




instance Arrow arr => Functor (ArrCC arr r i) where
    fmap = fmapA2M
instance Arrow arr => Applicative (ArrCC arr r i) where
    pure = pureA2M
    (<*>) = (<**>)
instance (ArrowZero arr, ArrowApply arr) => Monad (ArrCC arr r i) where
    (>>=) = (>>==)




------------------------------------
-- ArrErrCC and ArrCC
--      diff when used as ArrowPlus
--      ArrCC think r is good result
--      while ArrErrCC treat r as excecption
newtype ArrErrCC arr err i o = ArrErrCC {unArrErrCC :: ArrCC arr err i o}
    deriving (Category, Arrow, ArrowZero, Semigroup, Monoid, ArrowChoice, ArrowLoop)

instance Boxed (ArrErrCC arr r i o) where
    type BoxedFrom (ArrErrCC arr r i o) = ArrCC arr r i o
    -- Either (arr i r) (arr i o)
    box = ArrErrCC
    unbox = unArrErrCC


instance Arrow arr => ArrowCC (ArrErrCC arr) where
    type ArrowCC2Arrow (ArrErrCC arr) = arr
    (>>>>) = opmapbGBox (>>>>)
    firstCC = mapGBox firstCC
    arrCC_ex = box . arrCC_ex
    exitCC_ex = mapGBox exitCC_ex

    mapCC = mapGBox . mapCC
    switchCC = mapGBox switchCC
    runCC = runCC . unbox
plusArrErrCC
    :: (ArrErrCC ar ~ arr) => arr r i o -> arr r i o -> arr r i o
plusArrErrCC lhs rhs = if isLeft . unbox $ unbox lhs then rhs else lhs
instance ArrowZero arr => ArrowPlus (ArrErrCC arr r) where
    (<+>) = plusArrErrCC

instance (ArrowZero arr, ArrowApply arr) => ArrowApply (ArrErrCC arr r) where
    app = arrCC_ex $ x2a_ab2c_to_xb2c (runCC_exR zeroArrow) app

instance Arrow arr => Functor (ArrErrCC arr r i) where
    fmap = fmapA2M
instance Arrow arr => Applicative (ArrErrCC arr r i) where
    pure = pureA2M
    (<*>) = (<**>)
instance (ArrowZero arr, ArrowApply arr) => Monad (ArrErrCC arr r i) where
    (>>=) = (>>==)









---------------------------------------
newtype ArrPlusCC arr r i o = ArrPlusCC { unArrPlusCC :: (arr i r, arr i o) }

runArrPlusCC :: ArrowPlus arr => ArrPlusCC arr r i r -> arr i r
runArrPlusCC a = case unbox a of
    (i2r, i2o) -> i2r <+> i2o
instance Boxed (ArrPlusCC arr r i o) where
    type BoxedFrom (ArrPlusCC arr r i o) = (arr i r, arr i o)
    box = ArrPlusCC
    unbox = unArrPlusCC
composeArrPlusCC
    :: (ArrPlusCC ar ~ arr, ArrowPlus ar)
    => arr r i x -> arr r x o -> arr r i o
composeArrPlusCC lhs rhs = box $ case (unbox lhs, unbox rhs) of
    ((i2r, i2x), (x2r, x2o)) -> (i2r <+> (i2x >>> x2r), i2x >>> x2o)
instance ArrowPlus arr => Category (ArrPlusCC arr r) where
    (.) = flip composeArrPlusCC
    id = box (zeroArrow, Cat.id)

firstArrPlusCC
    :: (ArrPlusCC ar ~ arr, ArrowPlus ar)
    => arr r i o -> arr r (i, x) (o, x)
firstArrPlusCC = mapGBox $ bimap i2o_to_ix2o first


instance ArrowPlus arr => ArrowCC (ArrPlusCC arr) where
    type ArrowCC2Arrow (ArrPlusCC arr) = arr
    (>>>>) = composeArrPlusCC
    firstCC = firstArrPlusCC
    arrCC_ex a = box (zeroArrow, a)
    exitCC_ex a = box (runArrPlusCC a, zeroArrow)

    mapCC = mapGBox . fmap
    switchCC = mapGBox pair_flip
    runCC = runArrPlusCC
instance ArrowPlus arr => Arrow (ArrPlusCC arr r) where
    arr = arrCC
    first = firstCC



plusArrPlusCC
    :: (ArrPlusCC ar ~ arr, ArrowPlus ar)
    => arr r i o -> arr r i o -> arr r i o
plusArrPlusCC lhs rhs = box $ case (unbox lhs, unbox rhs) of
    ((i2r, i2o), (i2r', i2o')) -> (i2r <+> i2r', i2o <+> i2o')
instance ArrowPlus arr => ArrowZero (ArrPlusCC arr r) where
    zeroArrow = box (zeroArrow, zeroArrow)
instance ArrowPlus arr => ArrowPlus (ArrPlusCC arr r) where
    (<+>) = plusArrPlusCC
instance ArrowPlus arr => Semigroup (ArrPlusCC arr r i o) where
    (<>) = plusArrPlusCC
instance ArrowPlus arr => Monoid (ArrPlusCC arr r i o) where
    mappend = (<+>)
    mempty = zeroArrow




instance (ArrowPlus arr, ArrowChoice arr) => ArrowChoice (ArrPlusCC arr r) where
    left = mapGBox $ bimap i2o_to_Eix2o left
instance (ArrowPlus arr, ArrowApply arr) => ArrowApply (ArrPlusCC arr r) where
    app = arrCC_ex $ x2a_ab2c_to_xb2c (runCC_exR zeroArrow) app

instance (ArrowPlus arr, ArrowLoop arr) => ArrowLoop (ArrPlusCC arr r) where
    loop = arrCC_ex . loop . runCC_exR zeroArrow










--}
--}
--}
--}
--}
--}
--}
--}
