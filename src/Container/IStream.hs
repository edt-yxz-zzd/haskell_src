{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Container.IStream
where
import Container.IContainer
import Container.OpIter
import Container.OpPop
import Numeric.Natural

class (OpIter c, SequenceConcept c, OpPop c) => IStream c where
    uncons :: c -> Maybe (Element c, c)
    uncons = pop
    stream_drop_le :: Natural -> c -> c
    stream_drop_le 0 s = s
    stream_drop_le n s = case uncons s of
        Just (_, s') -> stream_drop_le (pred n) s'
        _ -> s

instance (OpIter c, SequenceConcept c, OpPop c) => IStream c where


