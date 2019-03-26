{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}



module Container.IBuffer
where
import Container.IContainer
--import Container.OpIsFull
import Container.OpIter
import Container.OpEmpty
import Container.OpSingleton
import Container.OpInsert
import Container.OpPop
import Data.Semigroup

class (OpIsEmpty a, OpIsFull a, OpIter a) => IBufferR a
class   (OpEmpty a, OpSingleton a, OpInsert a, Monoid a, Semigroup a)
    => IBufferW a
class (IBufferW a, IBufferR a, OpPop a) => IBuffer a

instance (OpIsEmpty a, OpIsFull a, OpIter a) => IBufferR a
instance   (OpEmpty a, OpSingleton a, OpInsert a, Monoid a, Semigroup a)
    => IBufferW a
instance (IBufferW a, IBufferR a, OpPop a) => IBuffer a


