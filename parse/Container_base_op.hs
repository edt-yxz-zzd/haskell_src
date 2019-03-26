{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Container_base_op where
import Container_base_static
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import qualified Data.List as L
import qualified Data.Map as M -- (Map)
import qualified Data.Set as S -- (Set)
import qualified Prelude as P
import qualified Data.Bits as B
import Data.Monoid
import Group

import Prelude hiding (lookup, null, take, length, fromInteger, filter)
-- import Prelude (Eq, Ord, Integer, Maybe, (.), ($), Bool, (>>=))

import Data.Bits ((.&.))
import SeedUtils -- hiding (safe_head)
import Ordering
import Boxed
import Explain






------------ IO: OpDyn

class (Container a c, Container b d) => OpOmapEx a c b d where
    omap_ex :: (a->b) -> c -> d
class OpOmapEx a c a c => OpOmap a c where
    omap :: (a->a) -> c -> c
    omap = omap_ex

class   ( Countable a inner, Countable a outer
        , Container a result)
    => OpMergeCC_Base a inner outer result where
    merge_countable_countable_base :: (a -> inner) -> outer -> result
class OpMergeCC_Base a inner outer outer => OpMergeCC a inner outer where
    -- unorder
    -- countable infinite
    -- see SeedUtils.merge_lsls
    --      Merge a [a] [a]
    --      merge_countable_countable f = merge_lsls . map f
    -- used in Merge pst psts psts => NonDeterministicAutomaton
    --      null_transition :: a -> pst -> psts
    --      null_transitions_step1 :: a -> psts -> psts
    --      null_transitions_step1 a =
    --          merge_countable_countable (null_transition a)
    merge_countable_countable :: (a -> inner) -> outer -> outer
    merge_countable_countable = merge_countable_countable_base
    merge_closureBy
        :: (outer -> outer -> Bool) -> (a -> inner) -> outer -> outer
    merge_closureBy eq f = find_fixed_pointBy eq
        $ merge_countable_countable f

class -- OpNull a c => null??
    Container a c => OpEmpty a c where
    -- OpNull may not be OpEmpty
    -- e.g. NonNullList
    empty :: c
    -- allow lots empties, may not equal
    -- null empty == True
    -- Eq a => null a =xx=>> a == empty ; there're lots nulls

class Container a c => OpInsert a c where
    extend :: Iterable a it => Integer -> it -> c -> c
    -- extend n it c = foldl' (flip insert) c $ take (fromInteger n) (iter it)
    -- extend n it c = foldr insert c $ take (fromInteger n) (iter it)
    extend n it c = foldr insert c $ take n it
    insert :: a -> c -> c
class Iterable a c => OpPop a c where
    -- sometimes Iterable is not OpPop
    -- e.g. NonNullList, size >= 1 ==>> pop :: c -> (a, Maybe c)
    pop :: c -> Maybe (a, c)
    pop c = listToMaybe $ pops c
    pops :: c -> [(a, c)]
    -- take care: sometimes we cannot implement pops via pop
    -- i.e. (-oo, -2) \/ (2, +oo)
    --  if pop (-oo, i)\/(j,+oo) = Just (i-1, (-oo, i-1)\/(j,+oo))
    --  then pops cannot be implemented via pop
class OpMember a c => OpRemove a c where
    remove :: a -> c -> Maybe ([a], c)
    discard :: a -> c -> Maybe c
    discard a c = remove a c >>= return . snd
    delete :: a -> c -> c
    delete a c = maybe c snd $ remove a c

class (Optional a c, OptionalOp a c)
    => DynOptional a c where
class (OpEmpty a c, OpSingleton a c)
    => OptionalOp a c

class Container a c => OpSingleton a c where
    singleton :: a -> c
    default singleton :: (OpEmpty a c, OpInsert a c) => a -> c
    singleton a = insert a empty
class (OpEmpty a c, OpInsert a c, OpSingleton a c)
    => InputFiniteSize a c where
    -- xxxx error: c can be of any finite size xxxxx
    --      e.g. NonNullList cannot have size 1
    --      input can be of any finite size instead of c!!!
    --      and may be infinite!???
    -- AnyFiniteSize v.s. OpInsert ==>> OpEmpty
    from_iterable :: Iterable a it => Integer -> it -> c
    from_iterable n it = extend n it empty
    fromList :: [a] -> c -- unsafe
    default fromList :: Iterable a [a] => [a] -> c -- unsafe
    fromList ls = from_iterable (unsafe_len ls) ls
class   ( InputFiniteSize a q, OpPop a q, WithIdentity q
        , OpOmap a q, OpIsFull a q)
    => Buffer a q




----
class Container a c => OpPartition a c where
    partition :: (a->Bool) -> c -> (c, c) -- (trues, falses)


class Container a c => OpFilter a c where
    filter :: (a -> Bool) -> c -> c
        -- c from True
    filter_not :: (a -> Bool) -> c -> c
    filter_not f = filter (not . f)
class Container a c => OpPartitionList a c where
    partition_list :: (a -> Maybe r) -> c -> ([r], c)
        -- ([r] from Just r, c from Nothing)



-- -}
-- -}
-- -}
-- -}
-- -}


