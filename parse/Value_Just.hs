{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Value_Just
where
import Language.Haskell.TH
import Language.Haskell.Syntax
import Control.Applicative
import Explain
import OpDynTheOnlyValue


data Value_Just a = Value_Just a
    deriving (Ord, Eq, Show, Read)
instance Functor Value_Just where
    fmap f (Value_Just a) = Value_Just (f a)
instance Monad Value_Just where
    return = Value_Just
    (Value_Just a) >>= f = f a
instance Applicative Value_Just where
    pure = Value_Just
    (Value_Just f) <*> (Value_Just a) = Value_Just (f a)
    _ *> a = a
    a <* _ = a
data Value_EmptySet a = Value_EmptySet
    deriving (Show, Read)
instOpDynTheOnlyValue . return $
    AppT (ConT ''Value_EmptySet) (VarT $ mkName "a")



$(let a = varT $ mkName "a" in
    instExplain [t| Maybe $a |] [t| Value_Just $a |]
        $ [e| \(Value_Just a) -> Just a |]
 )

$(let a = varT $ mkName "a" in
    instExplain [t| Maybe $a |] [t| Value_EmptySet $a |]
        $ [e| \_ -> Nothing |]
 )


