{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}












{-
env/conf - common info
    i.e.
        sizeof ptr
        encoding of str
type in file = type in memory + encoder
    self-known:
        detect-union {t1,...} = obj
    extern-known:
        t -> obj
size (unit:byte) of obj in file:
    static-known:
        byte
        ptr
        word32
    self-known:
        c_str = "[^\0]*\0"
        case-union {t1,t2...} = (type_id, obj)
        detect-union {t1,...} = obj
    extern-known:
        [byte] = len -> ".{len}"
        [static-known] = len -> len*static_size
    dyn-known: -- self+extern
        [str] = len -> "([^\0]*\0){len}"
        [self-known] = len -> "<self-known>*len"

-}

import Container2
import UInt
import Data.Maybe
import Data.Word
import System.IO
import System.IO.Error hiding (catch)
import Prelude as P hiding (catch)
import Control.Exception (catch)
-- import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import Explain
-- import Explain_abc_Integral

-- type UInt = Integer
class (Iterable s) => Stream s where
    -- type Token s
    -- toList :: s -> [Token s]


data SizeInfo t env ex
    = StaticSize UInt                   -- SizeInfo t ID  ()
    | SelfKnown (env (Maybe UInt))      -- SizeInfo t env ()
    | ExternKnown (ex -> UInt)          -- SizeInfo t ID  ex
    | DynKnown (ex -> env (Maybe UInt)) -- SizeInfo t env ex

size_info2size :: Monad env => SizeInfo t env ex -> ex -> env (Maybe UInt)
size_info2size (StaticSize u) _ = return $ Just u
size_info2size (SelfKnown eU) _ = eU
size_info2size (ExternKnown x2U) x = return . Just $ x2U x
size_info2size (DynKnown f) x = f x
data Type a = Type
data Env a b = Env {unEnv :: b}
with_env :: b -> a -> Env a b
with_env b _ = Env b
instance Functor (Env a) where
    fmap f (Env b) = Env (f b)
instance Monad (Env a) where
    return = Env
    Env b >>= f = f b


class Monad env => OpSkipSize env where
    skip_size :: UInt -> env ()
    maybe_skip_size :: UInt -> env Bool
    skip_size n = do
        r <- maybe_skip_size n
        if r then return () else fail "skip_size fail"
class (Monad env) => OpSkipx env t where
    type Extra env t
    size_info :: SizeInfo t env (Extra env t)
    maybe_skipx :: Extra env t -> env (Env t Bool)
    skipx :: Extra env t -> env (Env t ())
    -- skipx x = decodex x >>= return . with_env ()
    default skipx :: OpSkipSize env => Extra env t -> env (Env t ())
    default maybe_skipx :: OpSkipSize env => Extra env t -> env (Env t Bool)
    skipx x =
        size_info2size (size_info :: SizeInfo t env (Extra env t)) x
            >>= maybe err skip_size >>= return . Env where
            err = fail "skipx fail"
    maybe_skipx x =
        size_info2size (size_info :: SizeInfo t env (Extra env t)) x
            >>= maybe (return False) maybe_skip_size >>= return . Env

{-
    --}
class (OpSkipx env t) => Decoder env t where
    decodex :: Extra env t -> env t
    default decodex :: Detectable env t => Extra env t -> env t
    decodex x = maybe_decodex x >>= maybe (fail "decode fail") return
    -- decodex = size_info2decoder size_info

class Monad env => OpSeek env where
    type SeekPos env
    tell :: env (SeekPos env)
    seek :: SeekPos env -> env ()

{-
instance Pos UInt where
    pos_begin = 0
instance Rel Integer where
    rel_zero = 0
    rel_neg = negate
instance PosRelOp UInt Integer where
    pos_add rel pos = unsafe_from $ P.toInteger pos + rel
    pos_sub p1 p2 = P.toInteger p1 - P.toInteger p2
class Pos pos where
    pos_begin :: pos
class Rel rel where
    rel_zero :: rel
    rel_neg :: rel -> rel
class (Pos pos, Rel rel) => PosRelOp pos rel where
    pos_add :: rel -> pos -> pos
    pos_sub :: pos -> pos -> rel
class (OpSeek env, PosRelOp (SeekPos env) (SeekRel env))
    => OpSeekRel env where
    type SeekRel env
    seek_rel :: SeekRel env -> env ()
    seek_rel rel = tell >>= seek . (pos_add rel)
-}

class (OpSeek env, Decoder env t) => Detectable env t where
    maybe_decodex :: Extra env t -> env (Maybe t)
    detectx :: Extra env t -> env (Env t Bool)
    detectx x = (maybe_decodex x :: env (Maybe t))
        >>= return . Env . isJust
class Decoder env t => DecoderArray env t where
    decodexN :: [Extra env t] -> env [t]
    decodexN = mapM decodex
    skipxN :: [Extra env t] -> env (Env [t] ())
    skipxN xs = mapM_ (skipx :: Extra env t -> env (Env t ())) xs
        >>= return . Env



type Byte = Word8
data SeekRelResult
    = PermissionError
    | IllegalOperationError
    | Positions (UInt, HandlePosn) Integer UInt -- old size new
-- isPermissionError isIllegalOperationError 
read_le_IO :: Handle -> UInt -> IO ByteString
read_le_IO h n = B.hGet h . unsafe_from $ P.toInteger n
read_eq_IO :: Handle -> UInt -> IO (Maybe ByteString)
read_eq_IO h n = do
    (old, hpos) <- fullTell h
    bs <- read_le_IO h n
    new <- hTell h
    if P.toInteger (old + n) == new then return $ Just bs else do
    hSetPosn hpos
    return Nothing
read_byte_IO :: Handle -> IO (Maybe Byte)
read_byte_IO h = read_eq_IO h 1 >>= return . fmap B.head
maybe_skip_size_IO :: Handle -> UInt -> IO Bool
maybe_skip_size_IO h n = do
    r <- seek_rel_IO h (P.toInteger n)
    case r of
        Positions (old, hpos) i new ->
            if old + n == new then return True else do
            hSetPosn hpos
            return False
        _ -> return False

fullTell :: Handle -> IO (UInt, HandlePosn)
fullTell h = do
    i <- hTell h
    hpos <- hGetPosn h
    return (unsafe_from i, hpos)
seek_rel_IO :: Handle -> Integer -> IO SeekRelResult
seek_rel_IO h i = catch seek error2IO where
    seek = do
        old <- fullTell h
        hSeek h RelativeSeek i
        new <- hTell h
        return $ Positions old i (fromInteger new)
    error2IO e =
        if isIllegalOperation e then return IllegalOperationError
        else if isPermissionError e then return PermissionError
        else error $
            "error: not IllegalOperationError | PermissionError\n"
            ++ "\tbut " ++ show (ioeGetErrorString e)



{-
    File:
        Monad File -- as "env"
        MonadIO File -- for liftIO
        getH :: File Handle
-}
-- type File = StateT Handle IO
type File = ReaderT Handle IO
getH :: File Handle
-- getH = get
getH = ask
instance OpSkipSize File where
    maybe_skip_size n = do
        h <- getH
        liftIO $ maybe_skip_size_IO h n
instance OpSeek File where
    {-
    type SeekPos File = UInt
    tell = getH >>= liftIO . hTell >>= return . unsafe_from
    seek pos = do
        h <- getH
        liftIO $ hSeek h AbsoluteSeek $ P.toInteger pos
    -}
    type SeekPos File = HandlePosn
    tell = getH >>= liftIO . hGetPosn
    seek hpos = liftIO $ hSetPosn hpos


instance OpSkipx File Byte where
    type Extra File Byte = ()
    size_info = StaticSize 1
    skipx _ = skip_size 1 >>= return . Env
    maybe_skipx _ = maybe_skip_size 1 >>= return . Env
instance Decoder File Byte where
instance Detectable File Byte where
    maybe_decodex _ = getH >>= liftIO . read_byte_IO






type Bytes = ByteString
instance OpSkipx File Bytes where
    type Extra File Bytes = UInt
    size_info = ExternKnown id
    skipx n = skip_size n >>= return . Env
    maybe_skipx n = maybe_skip_size n >>= return . Env
instance Decoder File Bytes where
instance Detectable File Bytes where
    maybe_decodex n = getH >>= liftIO . flip read_eq_IO n




newtype CString = CS Bytes -- end by NUL in File ; without NUL in Memory
cstr_size_withoutNUL_IO :: Handle -> IO (Maybe UInt) -- NUL not count
cstr_size_withoutNUL_IO h = cstrs_eq_IO h 1 >>= return . fmap f where
    f [CS s] = P.fromIntegral $ B.length s
    f _  = error "logic error"
cstrs_le_IO :: Handle -> UInt -> IO [CString]
cstrs_le_IO h n = do
    hpos <- hGetPosn h
    bs <- B.hGetContents h -- lazy
    let ls = case P.splitAt (unsafe_from $ P.toInteger n) $ B.split 0 bs of
            (hs, []) -> init hs
            (hs, _) -> hs

    hSetPosn hpos
    hSeek h RelativeSeek . P.toInteger . sum . map (1+) $ map B.length ls
    return $ fmap CS ls
cstrs_eq_IO :: Handle -> UInt -> IO (Maybe [CString])
cstrs_eq_IO h n = do
    hpos <- hGetPosn h
    bs <- B.hGetContents h -- lazy
    case P.splitAt (unsafe_from $ P.toInteger n) $ B.split 0 bs of
        (hs, []) -> do
            hSetPosn hpos
            return Nothing
        (hs, _) -> do
            hSetPosn hpos
            hSeek h RelativeSeek . P.toInteger . sum . map (1+) $
                map B.length hs
            return $ Just $ map CS hs
instance OpSkipx File CString where
    type Extra File CString = ()
    size_info = SelfKnown $ getH >>=
        liftIO . fmap (fmap (1+)) . cstr_size_withoutNUL_IO
instance Decoder File CString where
instance Detectable File CString where
    maybe_decodex _ = getH >>=
        liftIO . fmap (fmap head) . flip cstrs_eq_IO 1


