{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}



-- abs for Num
module ADT.IModuloBy
where
import ADT.SemigroupBy
import Numeric.Natural
import Data.List
import Seed.By

class (Ord m, ISemigroupBy by m) => IModuloBy by m where
    -- a <> b >= a
class (IModuloBy (ModuloBy by a) (ModuloType by a)
    , ISemigroupBy (OpToModulo2SemigroupBy by a) a
    )
    => OpToModulo by a where
    type OpToModulo2SemigroupBy by a
    type ModuloBy by a
    type ModuloType by a
    -- to_moduloBy a <> to_moduloBy b === to_moduloBy (a <> b)
    -- OR
    -- to_moduloBy a <> to_moduloBy b >= to_moduloBy (a <> b)
    to_moduloBy :: proxy by -> a -> ModuloType by a



instance OpToModulo ByLen [a] where
    type OpToModulo2SemigroupBy ByLen [a] = ()
    type ModuloBy ByLen [a] = BySum
    type ModuloType ByLen [a] = Natural
    to_moduloBy _ = genericLength

instance IModuloBy BySum Natural where
{-
instance ISemigroupBy BySum Natural where
    mulBy _ = (+)
-}
