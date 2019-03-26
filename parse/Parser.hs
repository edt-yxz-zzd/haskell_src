
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
{-# LANGUAGE GADTs #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

import Container2
import Control.Applicative
import Label
import Boxed
import MonadEx
import Explain
import Control.Monad.Error hiding (Error)

-- what about Arrow?


{- useless
class Monad m => OpIsFail m where
    is_fail :: m a -> Bool
    is_fail :: m Boo
-}

class Monadx m => MonadContinueOnFail m where
    -- continue_on_fail mOnFail  mExe
    -- mExe may fail
    -- if mExe fail, then recovery and return mIfFail
    continue_on_fail :: m a -> m a -> m a
    -- why continue_on_fail?
    --      there are some useful work done, i.e. a correct prefex parsed
    --      or we cannot rollback at all, i.e. IO monad
class Monadx m => MonadErrorState m where
    -- getErrorState :: m a -> m (Maybe (ErrorState m))
class (MonadError (ErrorState m) m, Monadx m)
    => MonadTry m where
    type ErrorState m
    -- tryM :: (ErrorState m -> m a) -> m a -> m a
    throwM :: ErrorState m -> m a
    throwM = throwError
    catchM :: m a -> (ErrorState m -> m a) -> m a
    catchM = catchError
    tryM :: m a -> m (Either (ErrorState m) a)
    tryM ma = catchM (ma >>= return . Right) (return . Left)
    recoverM :: m a -> m ()
    recoverM = ignoreM . tryM
class Monadx m => MonadRollback m where
    -- rollback_on_fail mOnFail onSuccess   mTry
    rollback_on_fail :: m a -> (x->m a) -> m x -> m a
    -- rollback mOnFail mTry
    rollback :: m a -> m a -> m a
    rollback mOnFail = rollback_on_fail mOnFail return
    rollback_on_fail mOnFail onSuccess mTry =
        rollback mOnFail (mTry >>= onSuccess)

    {- error:
    rollback_on_fail ma x2ma mx = do
        x <- mx
        b <- is_fail
        if b then ma else x2ma x
    -}
    -- rollback_on_fail ma x2ma mx = ifM (will_fail mx) ma (mx >>= x2ma)

    will_fail :: m x -> m Bool -- without execute mx
    will_success :: m x -> m Bool -- without execute mx
    will_fail = rollback (return True) . (>> return False)
    will_success = fmap not . will_fail

if_fail, if_success
    :: MonadContinueOnFail m => m x -> m a -> m a -> m a
if_will_fail, if_will_success
    :: MonadRollback m => m x -> m a -> m a -> m a
if_fail mx = continue_on_fail . (mx >>)
if_success = flip . if_fail
if_will_fail = ifM . will_fail
if_will_success = ifM . will_success



class MonadRollback m => MonadReset m where
    type MonadStateType m
    getM :: m (MonadStateType m)
    resetM :: MonadStateType m -> m ()




{-
    mayM :: m x -> m (Maybe x)
    mayM = rollback_on_fail (return Nothing) (return . Just)
    eitherM :: m err -> (x -> m success) -> m x -> m (Either err success)
    eitherM mErr onSuccess = rollback_on_fail (mErr >>= return . Left) f where
        f x = onSuccess x >>= return . Right
-}

class Iterable s => Stream s where
    uncons :: s -> Maybe (Element s, s)

type Token m = Element (StreamType m)

class Monadx m => MonadPosition m where
    type Position m
class MonadPosition m => OpTell m where
    tell :: m (Position m)
class MonadPosition m => OpSeek m where
    seek :: Position m -> m ()
class (OpTell m, OpSeek m) => MonadSeekable m
instance (OpTell m, OpSeek m) => MonadSeekable m

type PositionInfoEx m = Label (m()) (PositionInfo m)
data Error = Error {errmsg :: String, consumed_flag :: Bool}
class (MonadTry m, Explain Error (ErrorState m), Parser m)
    => ParserD_Try m where
    clearConsumedFlag :: ErrorState m -> Label1 m (ErrorState m)
class (Parser m, Stream (StreamType m))
    => ParserD_St m where
    type StreamType m
    type PositionInfo m -- pos::UInt, and other info like lineno
    -- remain unconsumed stream
    getStream :: m (StreamType m)
    -- set to be parsed, using original PositionInfo
    setStream :: StreamType m -> m ()
    getStreamPos :: m (PositionInfo m)
    setStreamPos :: PositionInfo m -> m ()
    _getStreamPos :: m (PositionInfoEx m)
    _getStreamPos = fmap box getStreamPos
    _setStreamPos :: PositionInfoEx m -> m ()
    _setStreamPos = setStreamPos . unbox
    _getSt :: m (PositionInfoEx m, StreamType m)
    _getSt = do
        p <- _getStreamPos
        s <- getStream
        return (p,s)
    _setSt :: (PositionInfoEx m, StreamType m) -> m ()
    _setSt (p,s) = _setStreamPos p >> setStream s

    updatePositionInfo
        :: Token m -> PositionInfo m -> PositionInfoEx m
    -- two error type: consumed or not
    setConsumedFlagM :: m ()
    clearConsumedFlagM :: m ()
    getConsumedFlagM :: m Bool


    -- _setErrorState :: Error -> m () -- using throwM
    -- _setErrorState Error{errmsg, consumed_flag} = do
    --    if consumed_flag then setConsumedFlagM else return ()
    --    fail errmsg

class (ParserD_St m, ParserD_Try m) => ParserD_ErrSt m where
class   ( Monadx m
        --, MonadContinueOnFail m
        -- , ErrorState m ~ Error
        -- , View Error (ErrorState m)
        --, MonadReset m
        --, MonadSeekable m, Position m ~ UInt
        --, MonadTry m
        --, Explain Error (ErrorState m) -- PositionInfo or more
        )
    => Parser m where
    -- Error = Error {errmsg::String, unexpected::Bool, consumed::Bool}
    unexpected :: String -> m a
    expected :: String -> m a
    unexpected s = fail $ "unexpected: " ++ s
    expected s = fail $ "expected: " ++ s


    lookAhead :: m a -> m a -- not consumed if success
    default lookAhead :: ParserD_St m => m a -> m a
    lookAhead ma = do
        b <- getConsumedFlagM
        st <- _getSt
        a <- ma
        _setSt st
        unless b clearConsumedFlagM
        return a
    notFollowByEx :: (a->String) -> m a -> m ()
    default notFollowByEx :: ParserD_Try m => (a->String) -> m a -> m ()
    notFollowByEx f ma = catchM (ma >>= unexpected . f) $ \err->
        return ()
    -- catchM should clear error state and restore stream and position
    try :: m a -> m a -- keep errmsg
    default try :: ParserD_Try m => m a -> m a
    try ma = catchM ma $ \err -> throwM $
        getLabel (undefined::m()) $
        clearConsumedFlag err
        {- error: pos changed!
        let e@Error{errmsg, consumed_flag} = explain err in
        fail errmsg
        -}
    (<|>) :: m a -> m a -> m a
    default (<|>) :: (ParserD_Try m, ParserD_St m) => m a -> m a -> m a
    ma <|> mb = do
        b <- getConsumedFlagM
        clearConsumedFlagM
        may_restore b . catchM ma $ \err ->
            let e@Error{errmsg, consumed_flag} = explain err in
            if consumed_flag
            then throwM err -- may contain PosInfo err /= e
            else mb where
                may_restore b ma = ma >>< when b setConsumedFlagM
    (<?>) :: m a -> String -> m a
    default (<?>) :: ParserD_Try m => m a -> String -> m a
    -- set errmsg if error not_conumed
    ma <?> expected_msg = catchM ma $ \err ->
        let e@Error{errmsg, consumed_flag} = explain err in
        -- replace errmsg
        if not consumed_flag then expected expected_msg
        -- reset error state
        else throwM err
    anyToken :: m (Token m)
    default anyToken :: ParserD_St m => m (Token m)
    anyToken = do
        s <- getStream
        case uncons s of
            Nothing -> unexpected "EOS" -- end-of-stream
            Just (t, s') -> do
                pos <- getStreamPos
                _setStreamPos $ updatePositionInfo t pos
                setStream s'
                setConsumedFlagM
                return t

    token_cls :: String -> (Token m -> Bool) -> m (Token m)
    token_cls cls f = try $ do
        t <- anyToken
        if f t then return t else expected cls
    showToken :: Token m -> Label1 m String
    default showToken :: Show (Token m) => Token m -> Label1 m String
    showToken = box . show
    oneOf :: (Iterable c, OpMember c, Element c ~ Token m)
          => c -> m (Token m)
    oneOf c = token_cls errmsg (flip member c) where
        errmsg = ("one of" ++) . show $
            map unbox (map showToken $ iter c :: [Label1 m String])
    noneOf :: (Iterable c, OpMember c, Element c ~ Token m)
          => c -> m (Token m)
    noneOf c = try $ do
        t <- anyToken
        if member t c
        then getLabelM (showToken t) >>= unexpected else return t
    notFollowBy :: Show a => m a -> m ()
    notFollowBy = notFollowByEx show

class Parser m => LiteralParser m where
    token_eq :: Token m -> Token m -> Label1 m Bool
    default token_eq
        :: Eq (Token m) => Token m -> Token m -> Label1 m Bool
    token_eq a b = box $ a == b
    token :: Token m -> m (Token m)
    tokens :: [Token m] -> m [Token m]
    tokens_ :: [Token m] -> m ()
    tokens = mapM token
    tokens_ = mapM_ token
    token t = token_cls (unbox (showToken t::Label1 m String))
                        (unbox . \t' -> token_eq t t' :: Label1 m Bool)

class (Parser (p s1), Parser (p s2)) => ParserT p where
    -- how to  pos2 -> pos1??
    -- what if Macro expand ==>> pos??
    stack_parse :: p s1 s2 -> p s2 a -> p s1 a
    tokenize :: (s1 -> (s2, UInt)) -> p s1 s2

{-
-}

