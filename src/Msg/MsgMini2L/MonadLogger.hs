{-# LANGUAGE FlexibleContexts
    , FlexibleInstances
    , TypeSynonymInstances
    , UndecidableInstances
    , MultiParamTypeClasses
    , DefaultSignatures
    #-}
    -- , OverlappingInstances


module MonadLogger where

import Control.Monad.Writer -- MonadWriter
import Control.Applicative
import qualified Data.Foldable as F
import Data.Monoid
import Prelude hiding (log)


--class MonadWriter s m => MonadLogger a s m | s -> a where
--    log :: a -> MonadLogger a s m
class MonadWriter (f a) m => MonadLogger a f m where
    log :: a -> m ()
    default log :: Applicative f => a -> m ()
    log = tell . pure
logs :: MonadLogger a f m => [a] -> m ()
logs = f_logs -- mconcat . fmap pure

fconcat :: (F.Foldable ls, Monoid a) => ls a -> a
fconcat = F.foldr mappend mempty
f_logs :: (F.Foldable ls, MonadLogger a f' m) => ls a -> m ()
f_logs = F.foldr (\a m -> log a >> m) $ return () -- foldMap??

--instance (MonadWriter (f a) m, Applicative f) => MonadLogger a f m where
--    log = tell . pure
