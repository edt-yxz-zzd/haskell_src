{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , Rank2Types
            , KindSignatures
            , ScopedTypeVariables
            , DefaultSignatures #-}
            -- 


-- if I have more and more specialized definition in subclasses...
class G a where
    g :: a -> Int
    default g :: H a => a -> Int
    g = h

    {- Duplicate default type signature
    default g :: K a => a -> Int
    g = k
    -}
class G a => H a where
    h :: a -> Int
{-
instance G a => H a where
    h _ = 1
    -- g = h -- `g' is not a (visible) method of class `H'
-}
class H a => K a where
    k :: a -> Int
    -- g = k -- `g' is not a (visible) method of class `K'

{-
-- Duplicate instance declarations
instance H a => G a where
    g = h
instance (H a, K a) => G a where
    g = k
-- -}

instance H a => G a where
    g = h
instance K a => H a where
    h = k

class G a => I a where
    -- Multiple declarations of `g'
    -- g :: a -> Int
    _I2G_g :: a -> Int
{- Duplicate instance declarations : with H a => G a
instance I a => G a where
    g = _I2G_g
-}

