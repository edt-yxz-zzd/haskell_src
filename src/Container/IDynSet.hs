{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Container.IDynSet
where

import Container.IContainer
import Container.ISet
import Container.OpMember
import Container.SetOps
import Container.OpComplement
import Container.OpUniversal
import Container.OpEmpty
import Container.IBuffer
import Container.OpDiscard

{-
import Prelude hiding (Ordering(..))
import qualified Prelude as P
-}


class (ISet s, OpUniversal s) => UniversalSet s where
class (ISet s, IBuffer s, SetOp s, OpDiscard s) => IDynSet s where
    unions :: [s] -> s
    unions = foldl union empty
class (IDynSet s, UniversalSet s, OpComplement s, OpIsUniversal s)
    => IDynUniversalSet s


instance (ISet s, OpUniversal s) => UniversalSet s where
instance (ISet s, IBuffer s, SetOp s, OpDiscard s) => IDynSet s where
instance (IDynSet s, UniversalSet s, OpComplement s, OpIsUniversal s)
    => IDynUniversalSet s





