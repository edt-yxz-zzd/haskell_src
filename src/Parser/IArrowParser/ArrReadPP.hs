{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- implement by ReadP.P
module Parser.IArrowParser.ArrReadPP (ArrReadPP())
where
import Container.IContainer -- (IContainer(..), IStream(..))
{-
import qualified Prelude as P
-}
import Control.Monad.Fail
import Control.Monad
import Control.Applicative


import Control.Arrow
import Control.Category
import Prelude hiding ((.), id, ReadS)
import Container.IContainer
import ADT.IArrowCatch
import ADT.IArrowCC
import ADT.IArrowSuccess
import ADT.OpArrowPlusBy
import Seed.ArrowOps (unJustA, assertA)
import Seed.MaybeOps (maybe2either)
import Numeric.Natural
import Data.List hiding (uncons)
import Arrows.ArrCpsDetectT
import Arrows.ArrSS
import Parser.IArrowParser.IArrowParser




--infixr 5 +++, <++
type ReadS stream a = stream -> [(a, stream)]
data P stream a
  = Get (Element stream -> P stream a)
  | Look (stream -> P stream a)
  | Fail
  | Result a (P stream a)
  | Final [(a, stream)] -- invariant: list is non-empty!
  deriving Functor

-- Monad, MonadPlus

instance IStream stream => Applicative (P stream) where
  pure x = Result x Fail
  (<*>) = ap

instance IStream stream => MonadPlus (P stream) where
  mzero = empty
  mplus = (<|>)

instance IStream stream => Monad (P stream) where
  (Get f)      >>= k = Get (\c -> f c >>= k)
  (Look f)     >>= k = Look (\s -> f s >>= k)
  Fail         >>= _ = Fail
  (Result x p) >>= k = k x <|> (p >>= k)
  (Final r)    >>= k = final [ys' | (x,s) <- r, ys' <- run (k x) s]
        ------------------ Final ----------------------

  fail _ = Fail

instance IStream stream => MonadFail (P stream) where
  fail _ = Fail

instance IStream stream => Alternative (P stream) where
  empty = Fail

  -- most common case: two gets are combined
  Get f1     <|> Get f2     = Get (\c -> f1 c <|> f2 c)

  -- results are delivered as soon as possible
  Result x p <|> q          = Result x (p <|> q)
  p          <|> Result x q = Result x (p <|> q)

  -- fail disappears
  Fail       <|> p          = p
  p          <|> Fail       = p

  -- two finals are combined
  -- final + look becomes one look and one final (=optimization)
  -- final + sthg else becomes one look and one final
  Final r    <|> Final t    = Final (r ++ t)
  Final r    <|> Look f     = Look (\s -> Final (r ++ run (f s) s))
  Final r    <|> p          = Look (\s -> Final (r ++ run p s))
  Look f     <|> Final r    = Look (\s -> Final (run (f s) s ++ r))
  p          <|> Final r    = Look (\s -> Final (run p s ++ r))

  -- two looks are combined (=optimization)
  -- look + sthg else floats upwards
  Look f     <|> Look g     = Look (\s -> f s <|> g s)
  Look f     <|> p          = Look (\s -> f s <|> p)
  p          <|> Look f     = Look (\s -> p <|> f s)


-- ---------------------------------------------------------------------------
-- Operations over P

final :: [(a, stream)] -> P stream a
-- Maintains invariant for Final constructor
final [] = Fail
final r  = Final r

run :: IStream stream => P stream a -> ReadS stream a
run (Get f)      s = case uncons s of
    Just (c, s') -> run (f c) s'
    Nothing -> []
run (Look f)     s     = run (f s) s
run (Result x p) s     = (x,s) : run p s   --------------- Result --------
run (Final r)    _     = r
run _            _     = []

-- ---------------------------------------------------------------------------


type P2A stream = Kleisli (P stream)
type A stream e r = ArrSS (ArrCpsDetectT e r (P2A stream))
type ArrReadPP stream e r = A stream e r

_look_ahead :: IStream stream => (stream -> o) -> P2A stream i o
_look_aheads :: IStream stream => (stream -> [o]) -> P2A stream i o
_look_ahead s2o = Kleisli $ const p where
    p = Look $ \s -> final [(s2o s, s)]
_look_aheads s2os = Kleisli $ const p where
    p = Look $ \s -> final [(o, s)| o <- s2os s] -- maybe Fail
_skip :: IStream stream => P2A stream Natural ()
_skip = Kleisli n2P_ where
    n2P_ n = Look $ \s -> final [((), stream_drop_le n s)]
instance IStream stream => IArrowParser (A stream e r) where
    type InputStream (A stream e r) = stream
    --look_ahead :: (InputStream arr -> a) -> arr i a
    --skip_tokens_le :: arr Natural ()
    look_ahead = success_impureArrSS . mkArrCpsDetectT . _look_ahead
    look_aheads = impureArrSS . mkArrCpsDetectT . _look_aheads
    skip_tokens_le = success_impureArrSS . mkArrCpsDetectT $ _skip

_look_aheadP2A :: IStream stream => P2A stream i o -> P2A stream i (Maybe o)
_look_aheadP2A (Kleisli i2Po) = Kleisli $ \i-> Look $ \s -> f i2Po i s where
    f i2Po i s = final . g s $ run (i2Po i) s
    g s [] = [(Nothing, s)]
    g s x_s_pairs = [(Just x, s) | (x,_) <- x_s_pairs]
_do_or_nopA :: IStream stream => P2A stream i o -> P2A stream i (Maybe o)
_do_or_nopA (Kleisli i2Po) = Kleisli $ \i-> Look $ \s -> f i2Po i s where
    f i2Po i s = final . g s $ run (i2Po i) s
    g s [] = [(Nothing, s)]
    g _ x_s_pairs = [(Just x, s) | (x,s) <- x_s_pairs]


instance IStream stream => IArrowReset (P2A stream) where
instance IStream stream => OpDetectSuccessA (P2A stream) where
instance IStream stream => OpDoOrNopA (P2A stream) where
    do_or_nopA = _do_or_nopA
instance IStream stream => OpLookAheadA (P2A stream) where
    look_aheadA = _look_aheadP2A




