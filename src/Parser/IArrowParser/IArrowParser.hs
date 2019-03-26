{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Parser.IArrowParser.IArrowParser
    ( IArrowParser(..)
    , module ADT.IArrowCatch
    , module ADT.IArrowCC
    , module ADT.IArrowSuccess
    , module ADT.OpArrowPlusBy -- here are (many, many1 ...)
    )
where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Container.IContainer
import ADT.IArrowCatch
--import ADT.IArrowCC
import ADT.IArrowSuccessBy
import ADT.OpArrowPlusBy -- here are (many, many1 ...)
import Seed.ArrowOps (unJustA, assertA, constA, stream2plusZA, switchA)
import Seed.MaybeOps (maybe2either)
import Numeric.Natural
import Data.List hiding (uncons)
import ADT.IException


-- Token: not exported
type Token arr = Element (InputStream arr)
class (Arrow arr, IStream (InputStream arr)
    , IArrowCatch arr, IArrowExit arr, ArrowChoice arr
    , IArrowBiasedPlusSS (ArrowParser2PlusBy arr) arr
    , IArrowBiasedPlus (ArrowParser2PlusBy arr) arr
    , ArrowPlus arr
    ) => IArrowParser arr where
    type InputStream arr
    type ArrowParser2PlusBy arr
    type InputStream arr = String
    look_ahead :: (InputStream arr -> a) -> arr x a
    look_ahead f = look_aheads $ (:[]) . f
    look_aheads :: (InputStream arr -> [a]) -> arr x a -- [] === fail
    look_aheads f = look_ahead f >>> stream2plusZA
    --skip_tokens_le :: Natural -> arr i i
    skip_tokens_le :: arr Natural ()
    skip_tokens_le_ :: Natural -> arr i i
    skip_tokens_le_ n = (constA n >>> skip_tokens_le) &&& id >>^ snd

    -- anyToken??
    get1 :: arr i (Token arr)
    peek1 :: arr i (Token arr)
    peek_le :: Natural -> arr i [Token arr]
    token_ex :: (Token arr -> Bool) -> arr i (Token arr)
    token :: Eq (Token arr) => Token arr -> arr i (Token arr)
    sentence :: Eq (Token arr) => [Token arr] -> arr i [Token arr]
    eof :: arr i ()
    eof = notFollowedBy peek1
    notFollowedBy :: arr i o -> arr i ()
    notFollowedBy a = switchA (detect_successA a) >>> (constA () ||| zeroArrow)

    get1 = peek1 >>> skip_tokens_le_ 1
    peek1 = look_ahead f >>> unJustA where
        f = fmap fst . uncons
    peek_le n = look_ahead $ genericTake n . iter
    token_ex f = get1 >>> assertA f
    token t = token_ex (t==)
    sentence ts = peek_le (genericLength ts) >>> assertA (ts==)
    {-# MINIMAL ((look_ahead | look_aheads), skip_tokens_le) #-}

    -- <|> ==>> <<+>, <$+>
    (<$?>), (<<?>) :: arr i o -> ArrowExceptionType arr -> arr i o
    a <$?> e = a <$+> throwA_ e
    --a <<?> e = a <<+> throwA_ e
    a <<?> e = catchA_ a $ throwA_ e
    costly_try, fast_try :: arr i o -> arr i o
    costly_try a = a <<+> zeroArrow
    fast_try a = a <$+> zeroArrow
    unexpected_err
        :: (IUnexpectedException e, e ~ ArrowExceptionType arr)
        => e -> arr x y
    unexpected_err = throwA_ . to_unexpected
    fail_err
        :: (e ~ ArrowExceptionType arr) => e -> arr x y
    fail_err = throwA_
