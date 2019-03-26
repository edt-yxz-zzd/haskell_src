{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE TemplateHaskell #-}




module Explain.OpDynTheOnlyValue where
import Explain.ExplainBase
import Control.Monad
--import Language.Haskell.TH
{-# LANGUAGE TemplateHaskell #-}



class (Ord a, Explain () a) => OpDynTheOnlyValue a where
    -- undefined!
    -- EQ
    the_only_value :: a
    the_only_value = undefined

instance OpDynTheOnlyValue () where
    the_only_value = ()


{-
instance {-# OVERLAPS #-} OpDynTheOnlyValue tq => Eq tq where
    _ == _ = True
instance {-# OVERLAPS #-} OpDynTheOnlyValue tq => Ord tq where
    compare _ _ = EQ
instance {-# OVERLAPS #-} OpDynTheOnlyValue tq => Explain () tq where
    explain _ = ()
-}


{-
instOpDynTheOnlyValue_before :: TypeQ -> DecsQ
instOpDynTheOnlyValue_before tq = [d|
    instance Eq $tq where
        _ == _ = True
    instance Ord $tq where
        compare _ _ = EQ
    instance OpSafeFrom $tq () where
    instance OpFrom $tq () where
        from _ = ()
    instance Explain () $tq where
    |]

instOpDynTheOnlyValue :: TypeQ -> DecsQ
instOpDynTheOnlyValue tq = liftM2 (++)
    [d| instance OpDynTheOnlyValue $tq |]
    $ instOpDynTheOnlyValue_before tq

instOpDynTheOnlyValues :: [TypeQ] -> DecsQ
instOpDynTheOnlyValues = liftM concat . mapM instOpDynTheOnlyValue
-}


