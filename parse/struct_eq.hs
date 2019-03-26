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


import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
-- import Control.Monad.Reader
import Control.Monad.State
import Data.Ratio
import Boxed
import Control.Monad
import MapEx
import Foreign.StablePtr
import qualified Foreign.Ptr as Ptr
import Data.List (tails)

{-
data Ptr a
instance Eq (Ptr a) where
    (==) = undefined
instance Ord (Ptr a) where
    (<) = undefined
instance Show (Ptr a) where
    show = undefined
class OpGetPtr a where
    get_ptr :: a -> Ptr a
instance OpGetPtr a where
    get_ptr = undefined
cast_ptr :: Ptr a -> Ptr ()
cast_ptr = undefined
-}

{-
type Ptr = StablePtr
instance Ord (Ptr a) where
    a < b = castStablePtrToPtr a < castStablePtrToPtr b
    a <= b = castStablePtrToPtr a <= castStablePtrToPtr b
get_ptr :: MonadIO m => a -> m (Ptr a)
get_ptr = liftIO . newStablePtr
cast_ptr :: Ptr a -> Ptr ()
cast_ptr = undefined -- can not define it!!
-}

{-
type Ptr = Ptr.Ptr
get_ptr :: MonadIO m => a -> m (Ptr a)
get_ptr = liftIO . fmap (Ptr.castPtr . castStablePtrToPtr) . newStablePtr
cast_ptr :: Ptr a -> Ptr ()
cast_ptr = Ptr.castPtr
-}

data Ptr a = forall b. Ptr (Ptr.Ptr a) (StablePtr b, String) -- name of b
{-
get_ptr :: MonadIO m => a -> m (Ptr a)
get_ptr a = liftIO $ do
    p <- newStablePtr a
    return $ Ptr (Ptr.castPtr $ castStablePtrToPtr p) p
-}
cast_ptr :: Ptr a -> Ptr ()
cast_ptr (Ptr pa px) = Ptr (Ptr.castPtr pa) px

_gptr :: Ptr a -> Ptr.Ptr a
_gptr (Ptr pa _) = pa
instance Eq (Ptr a) where
    Ptr pa _ == Ptr pb _ = pa == pb
instance Ord (Ptr a) where
    Ptr pa _ < Ptr pb _ = pa < pb
    Ptr pa _ <= Ptr pb _ = pa <= pb
instance Show (Ptr a) where
    -- show = show . _gptr
    show (Ptr p (_, name)) = show (p, name)

newtype Labeled n a = Labeled a
instance Boxed a (Labeled n a) where
    box = Labeled
    unbox (Labeled a) = a

class OpGetTypeName a where
    type_name :: Labeled a String
    get_type_name_ex :: a -> Labeled a String
    get_type_name_ex _ = type_name
    get_type_name :: a -> String
    get_type_name _ = unbox (type_name :: Labeled a String)
class OpGetPtr a where
    get_ptr :: MonadIO m => a -> m (Ptr a)
instance OpGetTypeName a => OpGetPtr a where
    get_ptr a = liftIO $ do
        p <- newStablePtr a
        return $ Ptr (Ptr.castPtr $ castStablePtrToPtr p)
                     (p, get_type_name a)
instance OpGetTypeName Char where
    type_name = box "Char"
instance OpGetTypeName () where
    type_name = box "()"
instance OpGetTypeName [a] where
    type_name = box "[a]"
instance OpGetTypeName (a,b) where
    type_name = box "(,)"




















data StructView x a b
    = StructView2 String a b
    | StructView0 String
    | StructViewL String a
    | StructViewR String b
    {-
                        { constructor :: String
                        , arg1 :: a
                        , arg2 :: b
                        }
    -}
class
    -- (OpStructView (StructViewArg1 x), OpStructView (StructViewArg2 x)) =>
    OpStructView x where
    type StructViewArg1 x
    type StructViewArg2 x
    struct_view  :: (
                    --( OpStructView (StructViewArg1 x)
                    --, OpStructView (StructViewArg2 x)
                    )
        => x -> StructView x (StructViewArg1 x) (StructViewArg2 x)
instance OpStructView () where
    type StructViewArg1 () = ()
    type StructViewArg2 () = ()
    struct_view = const $ StructView0 "()"
instance -- (OpStructView a, OpStructView b) =>
    OpStructView (a, b) where
    type StructViewArg1 (a,b) = a
    type StructViewArg2 (a,b) = b
    struct_view (a, b) = StructView2 "(,)" a b
instance -- (OpStructView a, OpStructView b, OpStructView c) =>
    OpStructView (a, b, c) where
    type StructViewArg1 (a,b,c) = a
    type StructViewArg2 (a,b,c) = (b,c)
    struct_view (a, b, c) = StructView2 "(,,)" a (b,c)
instance OpStructView (Maybe a) where
    type StructViewArg1 (Maybe a) = a
    type StructViewArg2 (Maybe a) = ()
    struct_view (Just a) = StructView2 "Just" a ()
    struct_view Nothing = StructView0 "Nothing"
instance OpStructView [a] where
    type StructViewArg1 [a] = a
    type StructViewArg2 [a] = [a]
    struct_view (a:ls) = StructView2 ":" a ls
    struct_view [] = StructView0 "[]"
instance OpStructView Char where
    type StructViewArg1 Char = ()
    type StructViewArg2 Char = ()
    struct_view = StructView0 . show
instance OpStructView Int where
    type StructViewArg1 Int = ()
    type StructViewArg2 Int = ()
    struct_view = StructView0 . show
instance OpStructView Integer where
    type StructViewArg1 Integer = ()
    type StructViewArg2 Integer = ()
    struct_view = StructView0 . show
instance OpStructView Rational where
    type StructViewArg1 Rational = ()
    type StructViewArg2 Rational = ()
    struct_view = StructView0 . show
    {-
    type StructViewArg1 Rational = Integer
    type StructViewArg2 Rational = Integer
    struct_view x = StructView2 "Ratio" (numerater x) (denominator x)
    -}

instance OpStructView (Either a b) where
    type StructViewArg1 (Either a b) = Maybe a
    type StructViewArg2 (Either a b) = Maybe b
    struct_view (Left a) = StructView2 "Left" (Just a) Nothing
    struct_view (Right b) = StructView2 "Right" Nothing $ Just b


class (OpStructView a) => OpStructViewEx a where
instance    ( OpStructView x
            , OpStructViewEx (StructViewArg1 x)
            , OpStructViewEx (StructViewArg2 x)
            )
    => OpStructViewEx x where
{-
class OpStructEq a where
    struct_eq :: 
-}
isOpStructViewEx :: OpStructViewEx x => x -> x
isOpStructViewEx = id
_1 = isOpStructViewEx ()

class MonadIO m => StructEqAssume m where
    wereAssumeStructEq :: Ptr x -> Ptr x -> m Bool
    localAssumeStructEq :: Ptr x -> Ptr x -> m a -> m a
type VoidP = Ptr ()
-- type EqAssume m = ReaderT (Map VoidP VoidP) m
type EA = Map VoidP (Either Integer VoidP)
    -- ptr -> Right ptr
    -- [[ptr]] -> Left size
newtype EqAssume m a = EqAssume (StateT EA m a)
deriving instance Functor m => Functor (EqAssume m)
deriving instance Monad m => Monad (EqAssume m)
deriving instance MonadIO m => MonadIO (EqAssume m)
instance Boxed (StateT EA m a) (EqAssume m a) where
    box = EqAssume
    unbox (EqAssume a) = a
getEA :: Monad m => EqAssume m EA
getEA = box get
--setEA :: Monad m => EA -> EqAssume m ()
--setEA = box . put
class MonadIO m => Monad_ m
instance MonadIO m => Monad_ m
setEA :: Monad_ m => EA -> EqAssume m ()
setEA d = do
    -- liftIO $ print d
    box $ put d
runEqAssume_ex :: EqAssume m a -> EA -> m (a, EA)
runEqAssume_ex = runStateT . unbox
runEqAssume :: EqAssume m a -> m (a, EA)
runEqAssume = flip runEqAssume_ex M.empty

find_std_EA :: VoidP -> EA -> (Integer, [VoidP]) -- (0,[]) means not exist
find_std_EA q d = f [] q d where
    f ls p d = case M.lookup p d of
        Just (Right p') -> if p' /= q then f (p:ls) p' d else
            error "cycle in dict"
        Just (Left n) -> (n, p:ls)
        Nothing -> if null ls then (0,[]) else
            error "logic error or data/dict error"
find_std :: Monad_ m => VoidP -> EqAssume m (Maybe VoidP)
find_std p = do
    d <- getEA
    let (d', may_np) = find_std_update_EA p d
    setEA d'
    return $ fmap snd may_np
find_std_update_EA :: VoidP -> EA -> (EA, Maybe (Integer, VoidP))
find_std_update_EA p d = (maybe d id may_d, may_np) where
    (may_d, may_np) = find_std_update_EA_ex p d
find_std_update_EA_ex :: VoidP -> EA -> (Maybe EA, Maybe (Integer, VoidP))
find_std_update_EA_ex p d =
    case find_std_EA p d of
        (n, p:_:(ls@(_:_))) ->
            --(Just $ foldr (M.adjust . const $ Right p) d ls, Just (n,p))
            (Just $ foldr (alterOOVC $ Right p) d ls, Just (n,p))
        (n, p:_) ->
            (Nothing, Just (n,p))
        (0, []) -> (Nothing, Nothing)
        _ -> error "logic/data error"



insertIfAbsent :: VoidP -> EA -> EA
insertIfAbsent p d = alterOCVI (Left 1) p d
union_EA :: VoidP -> VoidP -> EA -> EA
union_EA x y d = if x == y then d else
    let d1 = foldr insertIfAbsent d [x,y]
        (d2, Just (nx, x')) = find_std_update_EA x d1
        (d', Just (ny, y')) = find_std_update_EA y d2
    in  if x' == y' then d' else
        if nx <= ny then f nx x' ny y' d' else f ny y' nx x' d'
    where
        -- merge [[x]] to [[y]]
        f nx x ny y d   = M.insert x (Right y)
                        . M.insert y (Left (nx+ny))
                        $ d


instance MonadIO m => StructEqAssume (EqAssume m) where
    wereAssumeStructEq x y = if x == y then return True else do
        x' <- find_std $ cast_ptr x
        y' <- find_std $ cast_ptr y
        liftIO $ do
            print "x y; x' y'"
            print (x, y)
            print (x', y')
        return $ x' /= Nothing && x' == y'
    localAssumeStructEq x y action = do
        d <- getEA
        let x' = cast_ptr x
            y' = cast_ptr y
            d' = union_EA x' y' d
        setEA d' -- local setting
        r <- action
        setEA d -- restore
        return r

rInfo :: StructView x a b -> (String, Int)
rInfo (StructView2 c a b) = (c, 2)
rInfo (StructView0 c) = (c, 0)
rInfo (StructViewL c a) = (c, -1)
rInfo (StructViewR c b) = (c, 1)
class OpGetPtr x => OpStructEq x where
    struct_eq ::( StructEqAssume m
                --, OpGetPtr x
                --, OpStructView x, StructEqAssume m
                --, OpStructEq (StructViewArg1 x)
                --, OpStructEq (StructViewArg2 x)
                )
        => x -> x -> m Bool

instance( OpStructEq (StructViewArg1 x), OpStructEq (StructViewArg2 x)
        , OpStructView x, OpGetPtr x
        )
    => OpStructEq x where
    struct_eq x y = do
        px <- get_ptr x
        py <- get_ptr y
        liftIO $ do
            print px
            print py
        if px == py then return True else do
        b <- wereAssumeStructEq px py
        if b then return True else do
        let sx = struct_view x
            sy = struct_view y
            rx = rInfo sx
            ry = rInfo sy
            csx = snd rx
        if rx /= ry then return False else do
        if csx == 0 then return True else do
        localAssumeStructEq px py $ do
         case csx of
          2 -> do
        -- if csx == 2 then do
            let StructView2 cx x1 x2 = sx
                StructView2 cy y1 y2 = sy
            b <- struct_eq x1 y1
            if not b then return False else do
            px1 <- get_ptr x1
            py1 <- get_ptr y1
            -- localAssumeStructEq (get_ptr x1) (get_ptr y1) $ do
            localAssumeStructEq px1 py1 $ do
                struct_eq x2 y2
          1 -> do
        -- else if csx == 1 then do
            let StructViewR cx x2 = sx
                StructViewR cy y2 = sy
            struct_eq x2 y2
          -1 -> do
        -- else if csx == -1 then do
            let StructViewR cx x1 = sx
                StructViewR cy y1 = sy
            struct_eq x1 y1
          _ -> fail "logic error"
        -- else fail "logic error"

    {-
        where
            px = get_ptr x
            py = get_ptr y
    -}


rs = 'a' : rs
rs' = repeat 'a'

r = fmap snd $ runEqAssume (struct_eq rs rs' :: EqAssume IO Bool)
r2 = runEqAssume (struct_eq (take 10 rs) (take 10 rs') :: EqAssume IO Bool)
f :: OpGetPtr a => a -> IO (Ptr a)
f = get_ptr

ts = tails rs'
ps = mapM f $ take 10 ts



