{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE IncoherentInstances #-}


module Explain.ExplainBase
    ( Explain (..)
    , Make (..)
    , View (..)
    , OpFrom (..)
    , OpSafeFrom (..)
    , OpLimitedFrom (..)

    )
where
--import SeedUtils__TH (decsQ_add, decQ2decsQ)
--import Language.Haskell.TH
{-# LANGUAGE TemplateHaskell #-}
--import SeedUtils (justif, unjust) -- , fromInteger)
import Seed.UnsafeUnpack (unsafe_unjust)
--import qualified Data.Ratio as Ratio
--import qualified Prelude as P
--import Prelude hiding (Integral, fromInteger, toInteger, Rational, Real)










-- like Boxed
-- but  1) Boxed assume O(1)
--          and assume box . unbox === unbox . box === id
--      2) no "| u -> spec where"
-- safe_from -- 1..* -> 0/1
-- from -- 1..* -> 1, may not surjection, may not injection
-- explain -- 1 -> 1, injection, may not surjection
-- make -- 1..* -> 1, surjection, may not injection
--    v.s. from
--    explain . make . explain = explain
--    make . explain . make = make
class OpFrom u spec => Explain spec u where
    explain :: u -> spec
    --default explain :: OpFrom u spec => u -> spec
    --explain = from -- alias
class OpFrom spec u => Make spec u where
    -- offer an opportunity to add auxiliary data
    -- or modify data, e.g. to simplify and standardize data
    -- so id /= explan . make /= make . explain
    -- not O(1)
    make :: spec -> u
    default make :: OpFrom spec u => spec -> u
    make = from -- alias

-- not: instance To spec u => Explain spec u where
-- but: instance Explain spec u => To spec u
instance {-# OVERLAPS #-} Explain spec u => OpFrom u spec where
    from = explain
{- why fail????
instance {-# OVERLAPS #-} Make spec u => OpFrom spec u where
    from = make
-}





class (OpSafeFrom from to, OpLimitedFrom from to)
    => OpFrom from to where
    from :: from -> to
class OpSafeFrom from to where
    safe_from :: Monad m => from -> m to
    unsafe_from :: from -> to

    --default safe_from :: OpFrom from to => from -> Maybe to
    --safe_from = Just . from
    unsafe_from = unsafe_unjust . safe_from
instance {-# OVERLAPS #-} OpFrom from to => OpSafeFrom from to where
    safe_from = return . from





class (Explain spec u, Make spec u) => View spec u where
instance (Explain spec u, Make spec u) => View spec u where


class OpLimitedFrom from to where
    -- e.g. int -> uint => -1 -> 0
    limited_from :: from -> to
instance {-# OVERLAPS #-} OpFrom from to => OpLimitedFrom from to where
    limited_from = from




------------------ a a
instance {-# OVERLAPS #-} Explain a a where
    explain = id
instance Make a a


------------------ Integral
instance {-# OVERLAPS #-} Integral a => Explain Integer a where
    explain = toInteger




