{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ImplicitParams #-}



module ADT.OpArrowPlusBy
where

import Seed.Types
import Seed.ProxyOps
import Seed.ArrowOps
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)


data ByArrowPlus
data ByArrowBiasedPlus on_err
data ByArrowBiasedPlusSS on_err
class Arrow arr => OpArrowZeroBy by arr where
    {-# MINIMAL (zeroBy | zeroBy_) #-}
    zeroBy :: proxy by -> arr x y
    zeroBy_ :: (?by :: proxy by) => arr x y
    zeroBy_ = withBy_ zeroBy
    zeroBy = withBy zeroBy_
class Arrow arr => OpArrowPlusBy by arr where
    -- plusBy = <+> | <<+> | <$+> | ...
    {-# MINIMAL (plusBy | plusBy_) #-}
    plusBy :: proxy by -> Op (arr i o)
    plusBy_, (<?+>?) :: (?by :: proxy by) => Op (arr i o)
    plusBy = withBy plusBy_
    plusBy_ = withBy_ plusBy
    (<?+>?) = plusBy_

    manyBy_ :: (?by :: proxy by) => arr i a -> arr i [a]
    manyBy_ a = many1By_ a `plusBy_` constA []
    many1By_ :: (?by :: proxy by) => arr i a -> arr i [a]
    many1By_ a = (a &&& manyBy_ a) >>> arr list1x2list

    choiceBy_ :: (?by :: proxy by) => arr i a -> [arr i a] -> arr i a
    choice0By_ :: ((?by :: proxy by), OpArrowZeroBy by arr) => [arr i a] -> arr i a
    --choiceBy_ = foldr (`plusBy_`) zeroArrow
    choiceBy_ = foldr (plusBy_)
    choice0By_ = choiceBy_ zeroBy_

    optionBy_ :: (?by :: proxy by) => a -> arr i a -> arr i a
    optionBy_ a i2a = i2a `plusBy_` constA a
    optionalBy_ :: (?by :: proxy by) => arr i a -> arr i ()
    optionalBy_ a = voidArr a `plusBy_` constA ()

    skipManyBy_ :: (?by :: proxy by) => arr i a -> arr i ()
    skipManyBy_ = voidArr . manyBy_
    skipMany1By_ :: (?by :: proxy by) => arr i a -> arr i ()
    skipMany1By_ = voidArr . many1By_
    sepByBy_, sepBy1By_, endByBy_, endBy1By_ :: (?by :: proxy by) => arr i a -> arr i sep -> arr i [a]
    sepByBy_ a sep = sepBy1By_ a sep `plusBy_` constA []
    sepBy1By_ a sep = (a &&& sep &&& sepByBy_ a sep) >>> arr tols where
        tols (a, (sep, ls)) = a:ls
    endByBy_ a sep = fmapA2M (map fst) $ manyBy_ (a &&& sep)
    endBy1By_ a sep = fmapA2M (map fst) $ many1By_ (a &&& sep)

    --chainrBy_ :: (?by :: proxy by) => arr i a -> arr (a,a) a -> a -> arr i a
    chainrBy_ :: (?by :: proxy by) => arr i a -> (a->b->b) -> b -> arr i b
    chainrBy_ a op b0 = fmapA2M (foldr op b0) $ manyBy_ a
    chainlBy_ :: (?by :: proxy by) => arr i a -> (b->a->b) -> b -> arr i b
    chainlBy_ a op b0 = fmapA2M (foldl op b0) $ manyBy_ a

    chainr1By_ :: (?by :: proxy by) => arr i a -> (a->a->a) -> arr i a
    chainr1By_ a op = fmapA2M (foldr1 op) $ many1By_ a
    chainl1By_ :: (?by :: proxy by) => arr i a -> (a->a->a) -> arr i a
    chainl1By_ a op = fmapA2M (foldl1 op) $ many1By_ a

    manyTillBy_ :: (?by :: proxy by) => arr i a -> arr i end -> arr i [a]
    manyTillBy_ a end = (manyBy_ a &&& end) >>> arr fst


    -----------------------
    manyBy :: proxy by -> arr i a -> arr i [a]
    many1By :: proxy by -> arr i a -> arr i [a]

    choiceBy :: proxy by -> arr i a -> [arr i a] -> arr i a
    choice0By :: (OpArrowZeroBy by arr) => proxy by -> [arr i a] -> arr i a

    optionBy :: proxy by -> a -> arr i a -> arr i a
    optionalBy :: proxy by -> arr i a -> arr i ()

    skipManyBy :: proxy by -> arr i a -> arr i ()
    skipMany1By :: proxy by -> arr i a -> arr i ()
    sepByBy, sepBy1By, endByBy, endBy1By :: proxy by -> arr i a -> arr i sep -> arr i [a]

    chainrBy :: proxy by -> arr i a -> (a->b->b) -> b -> arr i b
    chainlBy :: proxy by -> arr i a -> (b->a->b) -> b -> arr i b

    chainr1By :: proxy by -> arr i a -> (a->a->a) -> arr i a
    chainl1By :: proxy by -> arr i a -> (a->a->a) -> arr i a

    manyTillBy :: proxy by -> arr i a -> arr i end -> arr i [a]


    -------------------
    manyBy = withBy manyBy_
    many1By = withBy many1By_

    choiceBy = withBy choiceBy_
    choice0By = withBy choice0By_

    optionBy = withBy optionBy_
    optionalBy = withBy optionalBy_

    skipManyBy = withBy skipManyBy_
    skipMany1By = withBy skipMany1By_
    sepByBy = withBy sepByBy_
    sepBy1By = withBy sepBy1By_
    endByBy = withBy endByBy_
    endBy1By = withBy endBy1By_

    chainrBy = withBy chainrBy_
    chainlBy = withBy chainlBy_

    chainr1By = withBy chainr1By_
    chainl1By = withBy chainl1By_

    manyTillBy = withBy manyTillBy_



instance ArrowPlus arr => OpArrowPlusBy ByArrowPlus arr where
    plusBy_ = (<+>)
instance ArrowZero arr => OpArrowZeroBy ByArrowPlus arr where
    zeroBy_ = zeroArrow

