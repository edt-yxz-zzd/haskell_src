
{-
see: Prelude(error) instead!!!!

usage:
    import Control.Exception(assert)
    import Control.Exception.Base (throw)
    f = if ... then ... else throw . ErrorMsg $ " ..... "
    g = assert (a==b) result
    main = ...call f # f can be pure!!!
-}

module ErrorMsg
    (ErrorMsg(..)
    )
where

import Control.Exception -- .Base
import Data.Typeable

newtype ErrorMsg = ErrorMsg String
    deriving (Show, Typeable)
instance Exception ErrorMsg where

