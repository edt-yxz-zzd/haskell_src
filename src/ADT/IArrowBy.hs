

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




module ADT.IArrowBy
where




import Control.Arrow
import Seed.Boxed
import Seed.BoxedBy
import Seed.ProxyOps
class (IArrowBy by (GetArrowBy by a)
    , a ~ UnboxArrowBy by (GetArrowBy by a)
        (GetArrowInputBy by a) (GetArrowOutputBy by a)
    ) => IBoxArrowBy by a where
    type GetArrowBy by a :: * -> * -> *
    type GetArrowInputBy by a
    type GetArrowOutputBy by a


class Arrow arr => IArrowBy by arr where
    -- GetArrowBy by (UnboxArrowBy by arr i o) === arr
    type UnboxArrowBy by arr i o
    unboxArrowBy
        :: IArrowByEx by arr i o i2o => proxy by -> arr i o -> i2o
    boxArrowBy
        :: IArrowByEx by arr i o i2o => proxy by -> i2o -> arr i o
    chainBy -- >>>
        :: (IArrowByEx by arr i x i2x, IArrowByEx by arr x o x2o
            , IArrowByEx by arr i o i2o)
        => proxy by -> i2x -> x2o -> i2o
    chainBy p' i2x x2o = unbox (boxBy p i2x >>> boxBy p x2o) where
        unbox = unboxArrowBy p'
        p = proxy_castArrowBy p'

class (IArrowBy by arr, IBoxArrowBy by i2o
    , i2o ~ UnboxArrowBy by arr i o
    , arr ~ GetArrowBy by i2o
    , i ~ GetArrowInputBy by i2o
    , o ~ GetArrowOutputBy by i2o
    ) => IArrowByEx by arr i o i2o
        | by arr i o -> i2o, by i2o -> arr i o
instance (IArrowBy by arr, IBoxArrowBy by i2o
    , i2o ~ UnboxArrowBy by arr i o
    , arr ~ GetArrowBy by i2o
    , i ~ GetArrowInputBy by i2o
    , o ~ GetArrowOutputBy by i2o
    ) => IArrowByEx by arr i o i2o


data ArrowBy by
proxy_castArrowBy :: proxy by -> Proxy (ArrowBy by)
proxy_castArrowBy = appP (Proxy :: Proxy ArrowBy)
instance IArrowByEx by arr i o i2o
    => BoxedBy (ArrowBy by) i2o (arr i o) where
    --type BoxedByFrom (ArrowBy by) (arr i o) = i2o
    --type BoxedByTo (ArrowBy by) i2o = (arr i o)
    boxBy = boxArrowBy . last1P
    unboxBy = unboxArrowBy . last1P
