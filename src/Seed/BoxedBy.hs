

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}



module Seed.BoxedBy
where


class -- (BoxedByFrom by o ~ i, BoxedByTo by i ~ o) =>
    BoxedBy by i o | by i -> o, by o -> i where
    -- i - old type
    -- o - new type
    {-
    type BoxedByFrom by o
    type BoxedByTo by i
    type BoxedByFrom by o = i
    type BoxedByTo by i = o
    -}
    boxBy :: proxy by -> i -> o
    unboxBy :: proxy by -> o -> i

{-
class BoxedBy by (BoxedByFrom by o) o => BoxedBy_ by o where
    type BoxedByFrom by o
instance BoxedBy by (BoxedByFrom by o) o => BoxedBy_ by o where
-}

{-
data ArrowBy by
instance IArrowByEx by arr i o i2o
    => BoxedBy (ArrowBy by) i2o (arr i o) where
    boxBy = boxArrowBy . last1P
    unboxBy = unboxArrowBy . last1P
-}
