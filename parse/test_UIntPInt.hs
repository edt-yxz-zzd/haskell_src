
{-# LANGUAGE ScopedTypeVariables #-}
-- ScopedTypeVariables


import UInt
import PInt
import DInt
import Prelude as P
import NumOp as N



b :: forall n.
    ( Integral n, OpMul n, OpAdd n, OpSafeSub n
    , OpSafeDivModEx n n n n
    --, Read n
    )
    => n -> Bool
b _ = all id
    [ (take 3 [1, 4..] :: [n]) == [1, 4, 7]
    , (take 3 [4, 1..] :: [n]) == [4, 1]
    , length (take 300 [1, 4..] :: [n]) == 300
    , length ([2, 4..1] :: [n]) == 0
    , length ([1, 4..1] :: [n]) == 1
    , length ([1, 4..3] :: [n]) == 1
    , length ([1, 4..4] :: [n]) == 2
    , length ([1, 4..6] :: [n]) == 2
    , length ([1, 4..7] :: [n]) == 3
    , length ([1, 4..8] :: [n]) == 3
    , (8::n) == 4 P.* 2
    , (8::n) == 4 N.* 2
    , (6::n) == 4 P.+ 2
    , (6::n) == 4 N.+ 2
    , (2::n) == 4 P.- 2
    , (2::n) == 4 N.!-! 2
    , (2::n) == (4::n) `P.div` (2::n)
    , (2::n) == (4::n) N.!//! (2::n)
    --, (2::n) == (4::n) `P.mod` (2::n)
    --, (2::n) == (4::n) N.!%%! (2::n)
    , (1::n) == (4::n) `P.mod` (3::n)
    , (1::n) == (4::n) N.!%%! (3::n)
    , (1::n) == (4::n) N.!//! (4::n)
    {-
    --}
    ]


b1 = (1::UInt) == read "(( UInt 1 ))"
b2 = (1::PInt) == read "(( PInt 1 ))"
b3 = (1::DInt) == read "(( DInt 1 ))"
b4 = (-1::DInt) == read "(( DInt -1 ))"

u = undefined :: UInt
p = undefined :: PInt
main = do
    print "UInt"
    print $ b u
    print "PInt"
    print $ b p
    print "read"
    print $ all id [b1, b2, b3, b4]






