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




module OpDynTheOnlyValue where
import Explain
import Control.Monad
import Language.Haskell.TH
{-# LANGUAGE TemplateHaskell #-}



class (Ord a, Explain () a) => OpDynTheOnlyValue a where
    -- undefined!
    -- EQ
    the_only_value :: a
    the_only_value = undefined

instance OpDynTheOnlyValue () where
    the_only_value = ()


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



