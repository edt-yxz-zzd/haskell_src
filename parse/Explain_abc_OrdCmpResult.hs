{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}


module Explain_abc_OrdCmpResult
where
import Language.Haskell.TH
{-# LANGUAGE TemplateHaskell #-}
import Explain
import Explain__FromOrToXXX
import Ordering

import qualified Prelude as P

class Explain POrd a => POrdCmpResult a
class (POrdCmpResult a, Explain P.Ordering a) => OrdCmpResult a
instance (POrdCmpResult a, Explain P.Ordering a) => OrdCmpResult a

instExplain [t| POrd |] [t| P.Ordering |] [e| ord2pord |]

instance POrdCmpResult POrd
instance POrdCmpResult P.Ordering






