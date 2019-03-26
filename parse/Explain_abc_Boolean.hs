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


module Explain_abc_Boolean
where
import SeedUtils__TH (decsQ_add, decQ2decsQ)
import Language.Haskell.TH
{-# LANGUAGE TemplateHaskell #-}
import Explain
import Explain__FromOrToXXX
import OpDynTheOnlyValue

import SeedUtils (justif)
import qualified Prelude as P -- hiding (or, and, not)
import Prelude hiding (or, and, not)







---------------------- abc Bool

class Explain Bool a => Boolean a where
instance Explain Bool a => Boolean a where


-- bad idea: list can not explain as Bool
-- test should not be defined as below:
--      test :: Explain Bool a => a -> Bool
--      test = explain
--
-- good, but what if always be True??
--      test :: ToBool a => a -> Bool
--      test = toBool

class (Explain Value_True a, Boolean a) => Boolean_True a
class (Explain Value_False a, Boolean a) => Boolean_False a
instance (Explain Value_True a, Boolean a) => Boolean_True a
instance (Explain Value_False a, Boolean a) => Boolean_False a









data Value_False
data Value_True



------------------ Value_False
{-
instance OpDynTheOnlyValue Value_False
instance Eq Value_False where
    _ == _ = True
instance Ord Value_False where
    compare _ _ = EQ
instance OpSafeFrom Value_False () where
instance OpFrom Value_False () where
    from _ = ()
instance Explain () Value_False where

instance OpSafeFrom Value_False Bool where
instance OpFrom Value_False Bool where
    from _ = False
instance Explain Bool Value_False where
-}
instance OpSafeFrom Bool Value_False where
    safe_from b = justif (P.not b) the_only_value
instOpDynTheOnlyValue (conT ''Value_False)
instExplain (conT ''Bool) (conT ''Value_False) $ [| const False |]

------------------ Value_True
{-
instance OpDynTheOnlyValue Value_True
instance Eq Value_True where
    _ == _ = True
instance Ord Value_True where
    compare _ _ = EQ
instance OpSafeFrom Value_True () where
instance OpFrom Value_True () where
    from _ = ()
instance Explain () Value_True where

instance OpSafeFrom Value_True Bool where
instance OpFrom Value_True Bool where
    from _ = False
instance Explain Bool Value_True where
-}
instance OpSafeFrom Bool Value_True where
    safe_from b = justif b the_only_value
instOpDynTheOnlyValue (conT ''Value_True)
instExplain (conT ''Bool) (conT ''Value_True) $ [| const True |]






---------------------- DynBoolean

infixr 3 /\
infixr 2 \/
class OpDynTrue a where
    true :: a
class OpDynFalse a where
    false :: a
class OpLogicAnd a where
    and, (/\) :: a -> a -> a
    (/\) = and
class OpLogicOr a where
    or, (\/) :: a -> a -> a
    (\/) = or
class OpLogicNot a where
    not :: a -> a
class (OpLogicNot a, OpLogicOr a, OpLogicAnd a) => LogicOp a
instance (OpLogicNot a, OpLogicOr a, OpLogicAnd a) => LogicOp a



class (OpDynFalse a, OpSafeFromBool a, Boolean a)
    => DynBooleanFalse a where
class (OpDynTrue a, OpSafeFromBool a, Boolean a)
    => DynBooleanTrue a where
class (LogicOp a, DynBooleanTrue a, DynBooleanFalse a, OpFromBool a)
    => DynBoolean a where




instance OpDynFalse Value_False where
    false = the_only_value
instance DynBooleanFalse Value_False
instance OpLogicOr Value_False where
    or _ = id
instance OpLogicAnd Value_False where
    and _ = id

instance OpDynTrue Value_True where
    true = the_only_value
instance DynBooleanTrue Value_True
instance OpLogicOr Value_True where
    or _ = id
instance OpLogicAnd Value_True where
    and _ = id




instance OpDynFalse Bool where
    false = False
instance OpDynTrue Bool where
    true = True
instance DynBooleanTrue Bool
instance DynBooleanFalse Bool
instance OpLogicOr Bool where
    or _ = id
instance OpLogicAnd Bool where
    and _ = id
instance OpLogicNot Bool where
    not = P.not
instance DynBoolean Bool



------------------- 




