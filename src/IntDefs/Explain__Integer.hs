{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-}
module IntDefs.Explain__Integer
where

import Explain.ExplainBase
import IntDefs.IntDefs

instance OpSafeFrom Integer UInt where
    safe_from i = if i < 0  then fail "UInt from Integer < 0"
                            else return (fromInteger i)


