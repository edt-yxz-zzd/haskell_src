
module Standardizable
    (Standardizable(..)
    ,Standard()--nothing
    ,mkStandard
    ,unStandard
    )
where

newtype Standard a = Private_Standard a
    deriving (Show)

class Standardizable a where
    -- standardize . standardize == standardize
    standardize :: a -> a

mkStandard :: Standardizable a => a -> Standard a
mkStandard a = Private_Standard (standardize a)
unStandard :: Standard a -> a
unStandard (Private_Standard a) = a


