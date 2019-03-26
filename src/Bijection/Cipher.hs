{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}



module Cipher
where

import Boxed (Label(..), Boxed(..))

import Integer2Bytes
            ( uint2bytes__be, bytes2uint__be
            , uint2int__eo, int2uint__eo
            , uint2digits__be, digits2uint__be
            , uint2chunks__be, chunks2uint__be
            )
import Numeric.Natural
import Data.List (intercalate)
import Data.Word
import Control.Monad ((>=>))



class CipherBase cipher where
    type ClearText cipher :: *
    type CipherText cipher :: *
class (CipherBase cipher, Monad m) => Cipher m cipher where
    -- cipher = cipher_name key_value salt
    -- encrypt : 1 -> 1+
    -- decrypt : many -> 1/0
    encrypt :: cipher -> ClearText cipher -> m (CipherText cipher)
    decrypt :: cipher -> CipherText cipher -> m (ClearText cipher)
    -- return == decrypt cipher . encrypt cipher
    ---not required: return == encrypt cipher . decrypt cipher



data ChainCipher outer inner = CC outer inner
    -- a: outer, b: inner; like []
instance (CipherBase a, CipherBase b, CipherText a ~ ClearText b)
    => CipherBase (ChainCipher a b) where
    type ClearText (ChainCipher a b) = ClearText a
    type CipherText (ChainCipher a b) = CipherText b
instance (Cipher m a, Cipher m b, CipherText a ~ ClearText b)
    => Cipher m (ChainCipher a b) where
    encrypt (CC a b) t = encrypt a t >>= encrypt b
    decrypt (CC a b) t = decrypt b t >>= decrypt a
{-
chain_cipher
            :: (Cipher m a, Cipher m b
                , CipherText a ~ ClearText b
                , Cipher m (ChainCipher m a b)
                , ClearText (ChainCipher m a b) ~ ClearText a
                , CipherText (ChainCipher m a b) ~ CipherText b
                )
            => a -> b -> (ChainCipher m a b)
-}
chain_cipher :: a -> b -> ChainCipher a b
chain_cipher = CC
data CipherObj_VTable m a from to = CipherObj_VTable
        { encrypt__CipherObj_VTable :: a -> from -> m to
        , decrypt__CipherObj_VTable :: a -> to -> m from
        }
class Cipher m a => ICipherObj_VTable m a where
    cipher_obj_vtable :: CipherObj_VTable m a (ClearText a) (CipherText a)
instance Cipher m a => ICipherObj_VTable m a where
    cipher_obj_vtable = CipherObj_VTable encrypt decrypt
data CipherObj m from to = forall a. CipherObj
        { _CipherObj_VTable :: CipherObj_VTable m a from to
        , _CipherObj :: a
        }
make_cipher_obj :: Cipher m a => a -> CipherObj m (ClearText a) (CipherText a)
make_cipher_obj = CipherObj cipher_obj_vtable
{-
data CipherObj m from to = forall a. CipherObj
        { _unCipherObj
                :: (Cipher m a, from ~ ClearText a, to ~ CipherText a)
                => a
        }
-}



{-
instance Boxed (CipherObj m) where
    --type OriginalType_inBoxed (CipherObj m) = forall . (forall a. Cipher m a => a)
    box = CipherObj
    unbox = unCipherObj
instance Cipher m (CipherObj m) where
    encrypt = encrypt . unbox
    decrypt = decrypt . unbox
-}
instance CipherBase (CipherObj m from to) where
    type ClearText (CipherObj m from to) = from
    type CipherText (CipherObj m from to) = to
instance Monad m => Cipher m (CipherObj m from to) where
    encrypt CipherObj {_CipherObj = obj, _CipherObj_VTable = vtable} =
            encrypt__CipherObj_VTable vtable obj
    decrypt CipherObj {_CipherObj = obj, _CipherObj_VTable = vtable} =
            decrypt__CipherObj_VTable vtable obj


data ChainBijectionCipher outer inner = CBC outer inner
class CipherBase cipher => BijectionCipher cipher where
    bijection_forward :: cipher -> ClearText cipher -> CipherText cipher
    bijection_backward :: cipher -> CipherText cipher -> ClearText cipher
instance (BijectionCipher a, BijectionCipher b
            , CipherText a ~ ClearText b)
    => CipherBase (ChainBijectionCipher a b) where
    type ClearText (ChainBijectionCipher a b) = ClearText a
    type CipherText (ChainBijectionCipher a b) = CipherText b
instance (BijectionCipher a, BijectionCipher b
            , CipherText a ~ ClearText b)
    => BijectionCipher (ChainBijectionCipher a b) where
    bijection_forward (CBC a b) = bijection_forward b . bijection_forward a
    bijection_backward (CBC a b) = bijection_backward a . bijection_backward b
chain_bijection_cipher
    :: BijectionCipher (ChainBijectionCipher a b)
    => a -> b -> ChainBijectionCipher a b
chain_bijection_cipher = CBC





data ChainPureCipher outer inner = CPC outer inner
class CipherBase cipher => PureCipher cipher where
    type PureCipherError cipher :: *
    pure_encrypt :: cipher -> ClearText cipher -> CipherText cipher
    pure_decrypt :: cipher -> CipherText cipher
                -> Either (PureCipherError cipher) (ClearText cipher)
    -- Right == pure_decrypt a . pure_encrypt a
instance (PureCipher a, PureCipher b
            , CipherText a ~ ClearText b)
    => CipherBase (ChainPureCipher a b) where
    type ClearText (ChainPureCipher a b) = ClearText a
    type CipherText (ChainPureCipher a b) = CipherText b
instance (PureCipher a, PureCipher b
            , CipherText a ~ ClearText b
            , PureCipherError a ~ PureCipherError b)
    => PureCipher (ChainPureCipher a b) where
    type PureCipherError (ChainPureCipher a b) = PureCipherError a
    pure_encrypt (CPC a b) = pure_encrypt b . pure_encrypt a
    pure_decrypt (CPC a b) = pure_decrypt b >=> pure_decrypt a
chain_pure_cipher
    :: PureCipher (ChainPureCipher a b) => a -> b -> ChainPureCipher a b
chain_pure_cipher = CPC


{- Duplicate instance declarations : see CipherWithSalt below
instance (PureCipher cipher, Monad m, PureCipherError cipher ~ String)
    => Cipher m cipher where
    encrypt a = return . pure_encrypt a
    decrypt a dat = case pure_decrypt a dat of
        Right t -> return t
        Left err -> fail err
-}

newtype BijectionCipher2PureCipher a = BC2PC a
instance Boxed (BijectionCipher2PureCipher a) where
    type OriginalType_inBoxed (BijectionCipher2PureCipher a) = a
    box = BC2PC
    unbox (BC2PC a) = a

instance BijectionCipher a => BijectionCipher (BijectionCipher2PureCipher a) where
    bijection_forward = bijection_forward . unbox
    bijection_backward = bijection_backward . unbox


instance BijectionCipher a => CipherBase (BijectionCipher2PureCipher a) where
    type ClearText (BijectionCipher2PureCipher a) = ClearText a
    type CipherText (BijectionCipher2PureCipher a) = CipherText a
instance BijectionCipher a => PureCipher (BijectionCipher2PureCipher a) where
    type PureCipherError (BijectionCipher2PureCipher a) = ()
    pure_encrypt = bijection_forward
    pure_decrypt a = Right . bijection_backward a
newtype PureCipher2Cipher a = PC2C a
instance Boxed (PureCipher2Cipher a) where
    type OriginalType_inBoxed (PureCipher2Cipher a) = a
    box = PC2C
    unbox (PC2C a) = a
{-
instance XXX a => CipherBase (XXX2YYY a) where
    type ClearText (XXX2YYY a) = ClearText a
    type CipherText (XXX2YYY a) = CipherText a
-}
instance PureCipher a => CipherBase (PureCipher2Cipher a) where
    type ClearText (PureCipher2Cipher a) = ClearText a
    type CipherText (PureCipher2Cipher a) = CipherText a
instance PureCipher a => PureCipher (PureCipher2Cipher a) where
    type PureCipherError (PureCipher2Cipher a) = PureCipherError a
    pure_encrypt = pure_encrypt . unbox
    pure_decrypt = pure_decrypt . unbox
instance (Condition__PureCipher2Cipher m a)
    => Cipher m (PureCipher2Cipher a) where
    encrypt = encrypt__PureCipher
    decrypt = decrypt__PureCipher



class (PureCipher a, Monad m, PureCipherError a ~ String)
    => Condition__PureCipher2Cipher m a
instance (PureCipher a, Monad m, PureCipherError a ~ String)
    => Condition__PureCipher2Cipher m a

encrypt__PureCipher :: (PureCipher a, Monad m, PureCipherError a ~ String)
    => a -> ClearText a -> m (CipherText a)
decrypt__PureCipher :: Condition__PureCipher2Cipher m a
    => a -> (CipherText a) -> m (ClearText a)

encrypt__PureCipher a = return . pure_encrypt a
decrypt__PureCipher a dat = case pure_decrypt a dat of
    Right t -> return t
    Left err -> fail err




{-

    String = [Char]
    Byte = Word8
    Bytes = [Byte]
    LHan = ByteHan Byte | ControHan Int0_81
    outmost_encrypt :: String -> [LHan]

-}

type Byte = Word8
type Bytes = [Byte]
type UInt = Natural
type UInts = [UInt]
data Int0_81
data LHan = ByteHan Byte | ControHan Int0_81
type LHans = [LHan]
class       ( Cipher m (Get_Str2UIntsChipher m a)
            , ClearText (Get_Str2UIntsChipher m a) ~ String
            , CipherText (Get_Str2UIntsChipher m a) ~ UInts
            , Cipher m (Get_UInts2BytesChipher m a)
            , ClearText (Get_UInts2BytesChipher m a) ~ UInts
            , CipherText (Get_UInts2BytesChipher m a) ~ Bytes
            , Cipher m (Get_Bytes2LHansChipher m a)
            , ClearText (Get_Bytes2LHansChipher m a) ~ Bytes
            , CipherText (Get_Bytes2LHansChipher m a) ~ LHans
            )
    => String2LHans m a where
    type Get_Str2UIntsChipher (m :: * -> *) a :: *
    type Get_UInts2BytesChipher m a :: * -- :: (* -> *) -> * -> *
    type Get_Bytes2LHansChipher m a :: *

    -- compress string here!
    str2uints_cipher
        :: (Cipher m (Get_Str2UIntsChipher m a)
            , ClearText (Get_Str2UIntsChipher m a) ~ String
            , CipherText (Get_Str2UIntsChipher m a) ~ UInts
            )
        => Label (m ()) a -> Get_Str2UIntsChipher m a

    -- maybe bijection
    uints2bytes_cipher
        :: (Cipher m (Get_UInts2BytesChipher m a)
            , ClearText (Get_UInts2BytesChipher m a) ~ UInts
            , CipherText (Get_UInts2BytesChipher m a) ~ Bytes
            )
        => Label (m ()) a -> Get_UInts2BytesChipher m a

    -- salt and box
    bytes2lhans_cipher
        :: (Cipher m (Get_Bytes2LHansChipher m a)
            , ClearText (Get_Bytes2LHansChipher m a) ~ Bytes
            , CipherText (Get_Bytes2LHansChipher m a) ~ LHans
            )
        => Label (m ()) a -> Get_Bytes2LHansChipher m a
{-
    outmost_cipher
        :: Cipher m (CipherObj m String LHans)
        => Label (m ()) a -> CipherObj m String Bytes
    outmost_cipher a = make_cipher_obj $ chain_cipher (str2uints_cipher a) (uints2bytes_cipher a)
--}

    outmost_cipher
        -- :: forall. (Cipher m x, ClearText x ~ String, CipherText x ~ LHans)
        -- => a -> x
        :: Cipher m (CipherObj m String LHans)
        => Label (m ()) a -> CipherObj m String LHans
    outmost_cipher a = make_cipher_obj
                        $ chain_cipher (str2uints_cipher a)
                        $ chain_cipher (uints2bytes_cipher a)
                        $               (bytes2lhans_cipher a)
    {-
    outmost_encrypt :: a -> String -> m LHans
    outmost_encrypt = encrypt . outmost_cipher . (box :: a -> Label (m ()) a)
    outmost_decrypt :: a -> String -> m LHans
    outmost_decrypt = decrypt . outmost_cipher . (box :: a -> Label (m ()) a)
--}
--}





{-
    outer salt:
        t --> Salted salt t -[encrypt]-> d -[decrypt]-> Salted salt t --> t
    inner salt:
        t -[encrypt=get_salt >>= return . encrypt_with_salt t]-> d
            -[decrypt]-> t
        (salt, t) -[encrypt_with_salt]-> d -[decrypt_without_salt]-> t


-}

-- type family CipherSalt cipher :: *
-- type family ClearTextWithoutSalt cipher :: *
class CipherBase cipher => CipherWithSaltBase cipher where
    -- type ClearTextWithoutSalt cipher :: *
    type CipherSalt cipher :: *
class   (Monad m, CipherWithSaltBase cipher)
    => CipherWithSalt m cipher where
    -- inner salt
    encrypt_with_salt
        :: cipher -> CipherSalt cipher
        -> ClearText cipher -> m (CipherText cipher)
    decrypt_without_salt
        :: cipher -> CipherText cipher -> m (ClearText cipher)

class Monad m => MonadWithSalt m where
    type CipherSalt_in_Monad m :: *
    get_salt :: m salt

{-
instance (CipherWithSalt m cipher, MonadWithSalt m salt
        , salt ~ CipherSalt cipher)
    => Cipher m cipher where
    encrypt cipher t = do
        salt <- get_salt
        encrypt_with_salt cipher salt t
    decrypt = decrypt_without_salt
-}


class (CipherWithSalt m cipher, MonadWithSalt m
        , CipherSalt_in_Monad m ~ CipherSalt cipher)
    => Condition__CipherWithSalt2Cipher m cipher
instance (CipherWithSalt m cipher, MonadWithSalt m
        , CipherSalt_in_Monad m ~ CipherSalt cipher)
    => Condition__CipherWithSalt2Cipher m cipher
encrypt__CipherWithSalt :: Condition__CipherWithSalt2Cipher m a
    => a -> ClearText a -> m (CipherText a)
decrypt__CipherWithSalt :: Condition__CipherWithSalt2Cipher m a
    => a -> (CipherText a) -> m (ClearText a)

encrypt__CipherWithSalt cipher t = do
    salt <- get_salt
    encrypt_with_salt cipher salt t
decrypt__CipherWithSalt = decrypt_without_salt










-- outer salt
data Salted salt a = Salted salt a

add_salt :: (Cipher m a, ClearText a ~ Salted salt t)
        => a -> salt -> t -> m (CipherText a)
add_salt a salt = encrypt a . Salted salt

remove_salt_ex :: (Cipher m a, ClearText a ~ Salted salt t)
        => a -> (CipherText a) -> m (salt, t)
remove_salt :: (Cipher m a, ClearText a ~ Salted salt t)
        => a -> (CipherText a) -> m t
remove_salt a dat = remove_salt_ex a dat >>= return . snd
remove_salt_ex a dat = do
    Salted salt t <- decrypt a dat
    return (salt, t)


{-
instance CipherWithSalt m cipher => Cipher cipher where
------------------------------

--}



{-
data Hanzi
-- data Byte -- Word8
type Byte = Natural
data LHan -- Limiited Hanzi < 342 = 82+256
    -- 341 = 0b1010_1_0101
newtype LLHan = LLHan Byte
type LLHans = [LLHan]
type Bytes = [Byte]
type Err = Either String
char2int :: Char -> Integer
int2either_char :: Integer -> Err Char
int2bytes_le :: Integer -> Bytes
bytes2llhans :: Bytes -> LLHans
llhans2bytes :: LLHans -> Bytes
global_ints2int :: [Integer] -> Integer
global_int2ints :: Integer -> [Integer]
global_str2ints :: String -> [Integer]
global_ints2either_str :: [Integer] -> Err String




--unsafe_toUInt x = let i = toInteger x in if x >= 0 then UInt i else
--                error $ "(toUInt x) but x < 0"
--char2int = unsafe_toUInt . fromEnum

-- isBounded :: Bounded a => Integer -> Bool
safe_toEnum :: (Bounded a, Enum a) => Integer -> Maybe a
safe_toEnum i = if l <= i && i <= r then Just ans
                else Nothing
    where
        to_int = toInteger . fromEnum
        to_int_ a = to_int $ a `asTypeOf` ans
        l = to_int_ minBound
        r = to_int_ maxBound
        ans = toEnum $ fromInteger i
char2int = toInteger . fromEnum
int2either_char i = case safe_toEnum i of
                        Nothing -> Left (show i ++ " is not char")
                        Just c -> Right c
int2bytes_le = uint2bytes__be . int2uint__eo
bytes2int_le = uint2int__eo . bytes2uint__be
global_int2ints =
    -- int -[le]-> bytes -[split 0xFF]-> [[int0_254]] -[map le255]-> [int]
    map (uint2int__eo . chunks2uint__be 0xFE)
    . split_elem__Eq 0xFF . uint2bytes__be . int2uint__eo
global_ints2int =
    -- [int] -[map le255]-> [[int0_254]] -[join 0xFF]-> bytes -[le]-> int
    uint2int__eo . bytes2uint__be . join_elem 0xFF
    . map (uint2chunks__be 0xFE . int2uint__eo)

join :: [a] -> [[a]] -> [a]
join = intercalate
join_elem :: a -> [[a]] -> [a]
join_elem a = join [a]
split_elem :: (a->[a]->Bool) -> [a] -> [[a]]
split_elem pred ls = f ls where
    split1 (h:ts)   | pred h ts = ([], ts)
                    | otherwise = (h:hs', ts')
                    where (hs', ts') = split1 ts
    split1 _ = ([], [])
    f [] = []
    f ls = let (hs, ts) = split1 ls in hs : f ts
split_elem__Eq :: Eq a => a -> [a] -> [[a]]
split_elem__Eq a = split_elem (\h ts -> h == a)








----------------
byte2llhan = LLHan
llhan2byte (LLHan b) = b
bytes2llhans = map byte2llhan
llhans2bytes = map llhan2byte

global_str2ints = map char2int
global_ints2either_str = mapM int2either_char
-- int2int




global_str2int = global_ints2int . global_str2ints
global_int2either_str = global_ints2either_str . global_int2ints

global_int2llhans = bytes2llhans . int2bytes_le
global_llhans2int = bytes2int_le . llhans2bytes



class Cipher_Str2LLHans a where
    str2llhans :: a -> String -> LLHans
    llhans2str :: a -> LLHans -> Err String

    default str2llhans
        :: Cipher_Str2LLHans__via_Int a => a -> String -> LLHans
    default llhans2str
        :: Cipher_Str2LLHans__via_Int a => a -> LLHans -> Err String
    str2llhans a = global_int2llhans . int2int__encrypt a . global_str2int
    llhans2str a = global_int2either_str . int2int__decrypt a . global_llhans2int
class Cipher_Str2LLHans a => Cipher_Str2LLHans__via_Int a where
    int2int__encrypt :: a -> Integer -> Integer
    int2int__decrypt :: a -> Integer -> Integer




test_id :: Eq a => (a->b) -> (b->a) -> a -> Bool
test_id a2b b2a a = b2a (a2b a) == a


main = do
    print "fine!"
    print $ test_id global_int2ints global_ints2int 234355453546464565636134134
    print $ test_id global_ints2int global_int2ints [0xFF, 1,3,4,0xFE,0xFF,23,3]


--}
--
--
