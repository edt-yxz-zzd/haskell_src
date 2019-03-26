{-# LANGUAGE MagicHash #-}



module IntTransform.Integer2Bytes
    ( uint2digits__be, uint2digits__le
    , digits2uint__be, digits2uint__le
    , uint2digits, digits2uint
    , uint2bytes__be, bytes2uint__be
    , int2uint__eo, uint2int__eo
    , uint2chunks__be, chunks2uint__be
    , uint2chunks__le, chunks2uint__le
    )
where


--import GHC.Integer
import Data.Bits
import Numeric.Natural
import Data.Binary
import qualified Data.ByteString.Lazy as BL
-- import Data.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BX
import Data.Word
import Data.List (foldl', dropWhile, dropWhileEnd
                    , genericReplicate, genericLength)
-- import GHC.Integer.Logarithms
import Control.Exception (assert)
import Data.Ratio (numerator, (%))



byte_mask = 0xFF :: Integer
num_bits_of_byte = 8 :: Int





square :: Num a => a -> a
square a = a * a

-- [] or head result > 0
-- O(N*logN*logN) -- assume mul/divMod is O(N*logN)
uint2digits__be = uint2digits False
uint2digits__le = uint2digits True
uint2digits :: Bool -> Natural -> Natural -> [Natural]
uint2digits is_little_endian base u
    | base < 2  = error "radix must > 1"
    | u == 0    = []
    | otherwise = remove_leading_0s $ _uint2digits base u where
    -- base >= 2; u > 0
    _uint2digits base u = if base > u then [u] else
                    unpack_squares base $ _uint2digits (square base) u
    -- unpack_squares base [u] where u in [0..base^^2-1]
    -- ==>> [u'] where u' in [0..base-1]
    unpack_squares base = concat . map (unpack_square base)
    (unpack_square, remove_leading_0s) =
            if is_little_endian
                then (unpack_square__le, remove_leading_0s__le)
                else (unpack_square__be, remove_leading_0s__be)

    unpack_square__be base square = let (q, r) = divMod square base in [q,r]
    unpack_square__le base square = let (q, r) = divMod square base in [r,q]

    remove_leading_0s__be = dropWhile (0==)
    remove_leading_0s__le = dropWhileEnd (0==)
    {-
    remove_leading_0s__be (0:ls) = remove_leading_0s__be ls
    remove_leading_0s__be ls = ls
    remove_leading_0s__le (h:ls)
                | h == 0 && ls' == [] = []
                | otherwise = h: ls'
            where ls' = remove_leading_0s__le ls
    remove_leading_0s__le [] = []
    -}




digits2uint :: Bool -> Natural -> [Natural] -> Natural
digits2uint is_little_endian base digits = fold op 0 digits where
    (fold, op) = if is_little_endian then (foldr, opr) else (foldl', opl)
    high `opl` low = high * base + low
    opr = flip opl
digits2uint__be, digits2uint__le :: Natural -> [Natural] -> Natural
digits2uint__be = digits2uint False
digits2uint__le = digits2uint True













bin :: Natural -> String
bin = show_uint 2 f where
    f 0 = '0'
    f 1 = '1'
show_uintx :: Bool -> Natural -> (Natural->a) -> Natural -> [a]
show_uintx is_little_endian base show_radix =
    map show_radix . uint2digits is_little_endian base
show_uintx__table :: Bool -> Natural -> [a] -> Natural -> [a]
show_uintx__table is_little_endian base table =
    show_uintx is_little_endian base f where
    f i = let Just a = lookup i $ zip [0..] table in a
show_uint = show_uintx False

dec = show_uint 10 f where
    f = toEnum . (+i0) . fromIntegral
    i0 = fromEnum '0'

hexx is_little_endian = show_uintx__table is_little_endian 16 (['0'..'9']++['A'..'F'])

bDec = all (\i -> show i == dec i) [1..100] && dec 0 == ""

main = do
    -- print $ map bin [0..100]
    -- print $ map (uint2digits__le 2 ) [0..100]
    -- print $ map (hexx True) [0..100]
    -- print $ zip [1..] $ map (floor_logarithm 2) [1..100]
    print $ bDec && bU2I && bU2Rs1 && bU2Cs && null lsUP && null lsU2Cs
    print $ lsU2Cs

--uint2digits__le :: Natural -> [Natural]

uint2bytes__be :: Natural -> [Natural]
uint2bytes__be = uint2digits__be 0x100
bytes2uint__be :: [Natural] -> Natural
bytes2uint__be = digits2uint__be 0x100


test_id_ex :: (Eq a, Eq b) => (a->b) -> (b->a) -> [(a,b)] -> [(a,b,b,a)]
test_id_ex f g = filter pred . map two2foure where
        pred (a,b,b',a') = (a, b) /= (a', b')
        two2foure (a,b) = (a, f a, b, g b)
test_id :: (Eq a, Eq b) => (a->b) -> (b->a) -> [(a,b)] -> Bool
test_id f g = all $ \(a,b) -> f a == b && a == g b
                            && (g $ f a) == a && (f $ g b) == b



__data__bin__be = -- [0..100
    ["","1","10","11","100","101","110","111"
    ,"1000","1001","1010","1011","1100","1101","1110","1111"
    ,"10000","10001","10010","10011","10100","10101","10110"
    ,"10111","11000","11001","11010","11011","11100","11101"
    ,"11110","11111","100000","100001","100010","100011","100100"
    ,"100101","100110","100111","101000","101001","101010","101011"
    ,"101100","101101","101110","101111","110000","110001","110010"
    ,"110011","110100","110101","110110","110111","111000","111001"
    ,"111010","111011","111100","111101","111110","111111","1000000"
    ,"1000001","1000010","1000011","1000100","1000101","1000110"
    ,"1000111","1001000","1001001","1001010","1001011","1001100"
    ,"1001101","1001110","1001111","1010000","1010001","1010010"
    ,"1010011","1010100","1010101","1010110","1010111","1011000"
    ,"1011001","1011010","1011011","1011100","1011101","1011110"
    ,"1011111","1100000","1100001","1100010","1100011","1100100"]
    -- :: [[Natural]]
bU2Rs1 = test_id f g . zip [0..100] .
    map (map $ \c -> read [c]) $ __data__bin__be where
    f = uint2digits__be 2
    g = digits2uint__be 2


----------------------------
__int2uint :: Integer -> Integer
__int2uint i = if i >= 0 then shiftL i 1
                else shiftL (-i) 1 - 1

__uint2int :: Integer -> Integer
__uint2int u = if (u .&. 1) == 0 then u' else -(u'+1) where
    u' = shiftR u 1

is_id :: Eq a => (a->a) -> a -> Bool
is_id f a = a == f a
b1 = all (is_id $ __uint2int . __int2uint) [0..10]
b2 = all (is_id $ __int2uint . __uint2int) [0..10]
__data__uint2int = [0, -1, 1, -2, 2, -3, 3]
b3 = all (\(u, i) -> __uint2int u == i && __int2uint i == u)
        $ zip [0..] __data__uint2int
bU2I = b1 && b2 && b3

int2uint__eo :: Integer -> Natural
int2uint__eo = fromInteger . __int2uint
uint2int__eo :: Natural -> Integer
uint2int__eo = __uint2int . toInteger

------------------------------------








------------------------------------- [Natural] <-> Natural

------------------- [Byte] == chunks <-> Natural
{-
    uint2bytes__be forbids leading 0's
    but list of bytes may leading with 0
    so it's a 1-to-many mapping (1:Natural <-> many:[Byte])

    here to implement a bijection

    assume order:
        to_uint ls1 > to_uint ls2 <==>
            length ls1 > length s2
            or length ls1 == length ls2
                and ls1 > ls2
        ==>> to_uint [] == 0
        ==>> to_uint (replicate n 0) == sum $ map num_per_size [0..n-1]
                where num_per_size n = base ^^ n
            == (base ^^ n - 1) / (base-1) = offset n
        if n == length (encode u):
            encode u = pad_leading_0s n . uint2digits__be base $ u - offset n
        to implement encode, we need to calc n
        offset n <= u < offset (n+1)
        base^^n <= u*(base-1)+1 < base^^(n+1)
        n <= logarithm base (u*base-u+1) < n+1
        n = floor $ logarithm base (u*base - u+1)
-}

uint2chunks__be :: Natural -> Natural -> [Natural]
uint2chunks__le :: Natural -> Natural -> [Natural]
uint2chunks__le base = reverse . uint2chunks__be base
uint2chunks__be base u = r where
    n = chunk_size base u
    offset = chunks_offset base n
    pad_leading_0s ls = genericReplicate delta 0 ++ ls where
        digits_len = genericLength ls
        delta = if digits_len > n
            then error $ show (base, u, n, offset, digits_len, ls)
            else assert (digits_len <= n) (n - digits_len)
    r = pad_leading_0s . uint2digits__be base $
        assert (offset <= u) (u - offset)
chunks2uint__be :: Natural -> [Natural] -> Natural
chunks2uint__le :: Natural -> [Natural] -> Natural
chunks2uint__le base = chunks2uint__be base . reverse
chunks2uint__be base chunks = r where
    n = genericLength chunks
    offset = chunks_offset base n
    digits = dropWhile (0==) chunks
    r = digits2uint__be base digits + offset
chunks_offset :: Natural -> Natural -> Natural
chunks_offset base n = assert (base > 1 && pow > 0)
        $ div (pow - 1) (base - 1)
        where pow = uint_pow n base



pow_uint :: a -> (a->a) -> (a->a->a) -> Natural -> a -> a
pow_uint one square mul exp a = r where
    pows = iterate square a
    bits = uint2digits__le 2 exp
    factors = [x | (bit, x) <- zip bits pows, bit /= 0]
    r = foldr mul one factors

pow_uint__Num :: Num a => Natural -> a -> a
pow_uint__Num = pow_uint 1 square (*)

uint_pow :: Natural -> Natural -> Natural
--uint_pow exp = fromInteger . (^^ exp) . toInteger
--uint_pow = pow_uint__Num
uint_pow exp u = numerator $ (u % 1) ^^ exp




__data__pow_uint = map _two2three $ zip [0..] [1,3,9,27,81,243,729] where
    _two2three (exp, ans) = (exp, ans, pow_uint__Num exp 3)
lsUP = flip filter __data__pow_uint $ \(exp, ans, r) -> r /= ans



chunk_size :: Natural -> Natural -> Natural
chunk_size base u = assert (base > 1) $ floor_logarithm base (u*(base-1) + 1)

floor_logarithm :: Natural -> Natural -> Natural
floor_logarithm base u
    | base < 2 = error "base < 2"
    | u == 0 = error "logarithm base u where u == 0"
    -- base >= 2, u > 0
    | otherwise = log where
        f b u | u < b = (u, 0) -- (high_part, log)
              -- | high_part == 0 = (div u b, 1)
              | otherwise = if to_div then (div high_part b, 2*log+1)
                            else (high_part, 2*log)
            where
                (high_part, log) = f (square b) u
                to_div = high_part >= b
        (_, log) = f base u

__data__uint2chunks__be = zip [0..]
    [ [], [0], [1], [0,0], [0,1], [1,0], [1,1]
    , [0,0,0], [0,0,1], [0,1,0], [0,1,1], [1,0,0], [1,0,1], [1,1,0], [1,1,1]
    , [0,0,0,0]
    ]
bU2Cs = test_id (uint2chunks__be 2) (chunks2uint__be 2) __data__uint2chunks__be
lsU2Cs = test_id_ex (uint2chunks__be 2) (chunks2uint__be 2) __data__uint2chunks__be


{-
type Byte = Word8 -- Integer
type Bytes = [Byte]

int2bytes :: Integer -> Bytes
int2bytes = BX.unpack . encode
    -- (0::byte, big-endian::[byte])
    -- (1::byte, sign::byte, big-endian[byte])
    -- O(n**2), too slow!
main = do
    print $ int2bytes__le (-1)
-}


-- r = toInteger (I# (integerLog2# 1))

{-

type Byte = Integer
type Bytes = [Byte]

int2low_byte :: Integer -> Byte
int2low_byte = (.&.) byte_mask
int_exclude_low_byte :: Integer -> Integer
int_exclude_low_byte = flip shiftR num_bits_of_byte

-- assert int2low_byte (-1) == byte_mask
-- assert int_exclude_low_byte (-1) 323# == -1

partition_low_byte :: Integer -> (Byte, Integer)
partition_low_byte i = (int2low_byte i, int_exclude_low_byte i)







int2bytes__le :: Integer -> [Byte]
int2bytes__le = uint2bytes__le . __int2uint
uint2bytes__le :: Integer -> [Byte]
uint2bytes__le u = if u < 0 then error "uint2bytes__le : uint < 0"
                    else __uint2bytes__le u
__uint2bytes__le :: Integer -> [Byte]
__uint2bytes__le i = if i == 0 then [] else b : __uint2bytes__le i'
    where (b, i') = partition_low_byte i
main = do
    print b

--}




