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
module Parser.IArrowParser.ArrParsec (ArrParsec())
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
import Text.Parsec (ParsecT)
import qualified Text.Parsec as P
import Seed.MonadOps (list2plusM)



type P2A s u m = Kleisli (ParsecT s u m)
type A e r s u m = ArrSS (ArrCpsDetectT e r (P2A s u m))
type ArrParsec e r s u m = A e r s u m

-- StreamM: not exported; a local shorthand
class (IStream s, P.Stream s m (Element s), Monad m) => StreamM s m
instance (IStream s, P.Stream s m (Element s), Monad m) => StreamM s m


_look_ahead :: StreamM s m => (s->o) -> P2A s u m x o
_look_aheads :: StreamM s m => (s->[o]) -> P2A s u m x o
_look_ahead s2o = Kleisli i2Po where
    i2Po _ = fmap s2o P.getInput
--_look_aheads s2os = _look_ahead s2os >>= list2plusM
_look_aheads s2os = Kleisli i2Po where
    i2Po _ = fmap s2os P.getInput >>= list2plusM

instance StreamM s m => IArrowParser (A e r s u m) where
    type InputStream (A e r s u m) = s
    --look_ahead :: (InputStream arr -> a) -> arr i a
    --skip_tokens_le :: arr Natural ()
    look_ahead = success_impureArrSS . mkArrCpsDetectT . _look_ahead
    look_aheads = impureArrSS . mkArrCpsDetectT . _look_aheads
    skip_tokens_le = success_impureArrSS . mkArrCpsDetectT $ _skip where
        _skip = Kleisli n2P_
        n2P_ n = do
            s <- P.getInput
            P.setInput $ stream_drop_le n s
            pos <- P.getPosition
            P.setPosition $ P.incSourceColumn pos (fromIntegral n)

instance StreamM s m => IArrowReset (P2A s u m) where
instance StreamM s m => OpDetectSuccessA (P2A s u m) where
    -- using look_aheadA
instance StreamM s m => OpDoOrNopA (P2A s u m) where
    do_or_nopA (Kleisli i2Po) = Kleisli i2Pmo where
        i2Pmo i = P.try (i2Po i >>= return . Just)
                    P.<|> return Nothing

instance StreamM s m => OpLookAheadA (P2A s u m) where
    look_aheadA (Kleisli i2Po) = Kleisli i2Pmo where
        i2Pmo i = P.try (P.lookAhead (i2Po i >>= return . Just))
                    P.<|> return Nothing







