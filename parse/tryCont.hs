{-# LANGUAGE Arrows #-}


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


-- import Control.Monad.Cont (MonadCont(..))
import Control.Arrow 
import Control.Category hiding (id, (.))
import qualified Control.Category as Cat

{-
-- assume known (arr i a)
--   i     ->    a      ->     r
--   |---known---|---unknown---|
newtype AConT r arr i a = AConT {unAConT :: arr a r -> arr i r}
instance Arrow arr => Arrow (AConT r arr) where
    arr i2a = AConT (arr i2a >>>)
    first (AConT aAr2iAr) = AConT adAr2idAr where
        adAr2idAr adAr = idAr where
            -- aAr2iAr adAr ==>> idAr
            idAr = idArd >>> arr fst
            adArd = adAr &&& arr id >>> arr (\(r,(a,d))->(r,d))
            idArd = adArd >>> first adArd
        {-
            idAr = arr $ \(i,d) ->
                let iAr = aAr2iAr $ (\a->(a,d)) ^>> adAr in
                iAr -< i
        -}
instance Category arr => Category (AConT r arr) where
    id = AConT id
    -- A i a . A a b
    -- f :: arr a r -> arr i r
    -- g :: arr b r -> arr a r
    AConT f . AConT g = AConT (g . f)
-}



usingCallCC :: MonadCont m => Int -> m (Maybe Char)
usingCallCC i = do
    b <- callCC $ \exit -> do
        () <- valid i exit -- exit :: Bool -> m ()
        [x] <- valid2 i exit -- exit :: Bool -> m [...]
        return True -- :: m local_result == m (Maybe Char)
    return . Just $ if b then 'T' else 'F'
    where
        valid2 i exit = do
            exit True
            return []
        valid i exit =
            if i < 0
            -- exit :: Bool -> m ()
            --  callCC_result  local_stmt_result
            then exit False
            else return () -- local stmt "valid" :: m local_result == m b

callCCt :: (Monad m, m'~ContT r m)
    => ((a -> m' b) -> m' a) -> (a->m r) -> m r
-- callCCt exit a2mr = runContT (callCC exit) a2mr
callCCt = runContT . callCC

class Monad m => MonadCont m where
    callCC :: ((a -> m b) -> m a) -> m a
    -- (arr a b -> arr i a) -> arr i a
    -- known (a -> m b) : how to exit
    --      take "a" as result
    --      m b - the local result to quit

    -- callCC $ \exit -> expr
    --      expr :: m callCC_result === m a
    --      exit :: callCC_result -> m local_stmt_result === a -> m b


instance Monad m => MonadCont (ContT r m) where
    callCC a2cb_to_ca = ca where
        -- need [a->mr]
        ca = ContT a2mr_to_mr
        -- a2cb_to_ca :: (a->C ((b->mr)->mr)) -> C ((a->mr)->mr)
        --      drop C:: (a->((b->mr)->mr)) -> (a->mr) -> mr
        -- need [a->(b->mr)->mr, a->mr]
        -- ==>> need [(b->mr)->(a->mr), a->mr]
        -- ==>> need [a->mr] -- since we return a2mr directly
        -- p - plain -- without ContT
        a2pb_to_pa a2pb = runContT . a2cb_to_ca $ ContT . a2pb
        a2mr_to_mr a2mr = a2pb_to_pa a2pb a2mr where
            a2pb = flip $ const a2mr
        -- explain:
        --      view all monad as (a -> m x)
        --      note that (b -> m r) are dropped
        --      replaced by (a -> m r)
        --      where (m b) is the local output type
        --      so, the remain computation outside local function
        --          is cancelled
        --  callCC say 
        --      (a -> m local_output) -- to complete local compute
        --      (a->m *) -> m a -- known how to skip local compute
        --                          if given how to compute "a"
        --  can (ContT r m a) be (arr a r)?
        --      Arrow, neg Arrow, missing Arrow
        {-
        a2Mb2mr_to_mrW_to_a2mr_to_mr a2Mb2mr_to_mrW =
            runContT . a2cb_to_ca $ ContT . a2Mb2mr_to_mrW
        a2mr_to_mr a2mr = a2Mb2mr_to_mrW_to_a2mr_to_mr a2Mb2mr_to_mrW a2mr where
            a2Mb2mr_to_mrW = a_to_b2mr_to_mr
            a_to_b2mr_to_mr = flip b2mr_to_a2mr
            b2mr_to_a2mr = const a2mr
        -}

newtype ContT r m a = ContT {runContT :: (a->m r)->m r}
    -- m r : a computation resulting r
    -- a -> m r : arrow a r
    -- arr a r -> r : a is given; require remain calc
    -- (a->mr)->(()->mr) ==>> contains (()->ma)!!

instance Monad m => Monad (ContT r m) where
    return a = ContT $ \a2mr -> a2mr a -- yeah, I contain an "a"!
    -- need [a2mr]       need [a, b2mr]            need [b2mr]
    ContT a2mr_to_mr >>= a_to_C_b2mr_to_mr = ContT b2mr_to_mr where
        a_to_b2mr_to_mr = runContT . a_to_C_b2mr_to_mr
        b2mr_to_mr = a2mr_to_mr . flip a_to_b2mr_to_mr
        {-
        a_to_b2mr_to_mr = runContT . a_to_C_b2mr_to_mr
        b2mr_to_mr b2mr = mr where
            mr = a2mr_to_mr a2mr
            a2mr a = a_to_b2mr_to_mr a b2mr

        b2mr_to_mr b2mr = a2mr_to_mr a2mr
        a2mr = flip a_to_b2mr_to_mr b2mr

        b2mr_to_mr = a2mr_to_mr . flip a_to_b2mr_to_mr
        -}






