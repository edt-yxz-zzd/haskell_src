{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module OpTest where

import Explain_abc_Boolean
import Explain__Bool
import Explain
import OpDynTheOnlyValue
import Prelude as P

class (OpToBool a, Boolean (TestExResult a)) => OpTest a where
    type TestExResult a :: *
    test_ex :: a -> TestExResult a
    default test_ex :: OpFrom a (TestExResult a) => a -> TestExResult a
    test_ex = from
    test :: a -> Bool
    test = toBool


--------------- Bool
instance OpTest Bool where
    type TestExResult Bool = Bool
instance OpTest Value_False where
    type TestExResult Value_False = Value_False
instance OpTest Value_True where
    type TestExResult Value_True = Value_True
--------------- Integer
instance OpTest Integer where
    type TestExResult Integer = Bool
instance OpSafeFrom Integer Bool where
instance OpFrom Integer Bool where
    from = (0 /=)
--------------- Int
instance OpTest Int where
    type TestExResult Int = Bool
instance OpSafeFrom Int Bool where
instance OpFrom Int Bool where
    from = (0 /=)
-- UInt/...



--------------- ()
instance OpTest () where
    type TestExResult () = Value_False
instance OpSafeFrom () Value_False where
instance OpFrom () Value_False where
    from _ = the_only_value
instance OpSafeFrom () Bool where
instance OpFrom () Bool where
    from _ = False

--------------- []
instance OpTest [a] where
    type TestExResult [a] = Bool
instance OpSafeFrom [a] Bool where
instance OpFrom [a] Bool where
    from = P.not . null
--------------- Maybe
instance OpTest (Maybe a) where
    type TestExResult (Maybe a) = Bool
instance OpSafeFrom (Maybe a) Bool where
instance OpFrom (Maybe a) Bool where
    from Nothing = False
    from _ = True
--------------- Either
instance OpTest (Either a b) where
    type TestExResult (Either a b) = Bool
instance OpSafeFrom (Either a b) Bool where
instance OpFrom (Either a b) Bool where
    from (Left _) = False
    from (Right _) = True
-- Set Map

