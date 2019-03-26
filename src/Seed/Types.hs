


module Seed.Types
where

type Op a = a -> a -> a
type Transform a = a -> a

type ChainA arr i x o = arr i x -> arr x o -> arr i o
type ChannelA p arr i o i' o' = arr i o -> arr i' o' -> arr (p i i') (p o o')

type MulA arr i o i' o' = ChannelA (,) arr i o i' o' -- ***
type AddA arr i o i' o' = ChannelA Either arr i o i' o' -- +++
type PlusA arr i o = Op (arr i o) -- <+>
type AndA arr i o o' = arr i o -> arr i o' -> arr i (o,o') -- &&&
type OrA arr i i' o = arr i o -> arr i' o -> arr (Either i i') o -- |||
type BothA arr i o o' = arr i o -> arr i o' -> arr i (Either o o')
    -- bothA >>> f +++ g -- require <+>






