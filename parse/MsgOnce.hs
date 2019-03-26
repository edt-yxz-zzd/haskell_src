
{-# LANGUAGE FlexibleContexts
    , FlexibleInstances
    , TypeSynonymInstances
    , UndecidableInstances
    , OverlappingInstances
    , TypeFamilies
    , Rank2Types
    , ScopedTypeVariables
    , TypeOperators
    , MultiParamTypeClasses
    , FunctionalDependencies
    , DefaultSignatures
    #-}

module MsgOnce where

import SeedUtils (direct_product, ungroup, ungroup_half)
import qualified Container as C

import qualified Data.Map as M -- (Map)
import qualified Data.Set as S -- (Set)
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.State


process :: MsgProcess i msg a => [msg] -> a -> a
process msgs s = execState m s where
    m = do
        mpush_msgs msgs
        process_msgs

class MsgBuffer msg a | a -> msg where
    push_msg :: msg -> a -> a
    pull_msg :: a -> Maybe (msg, a)

push_msgs :: MsgBuffer msg a => [msg] -> a -> a
push_msgs msgs a = foldr push_msg a msgs
mpush_msg :: (MonadState a m, MsgBuffer msg a) => msg -> m ()
mpush_msg msg = get >>= put . push_msg msg
mpush_msgs :: (MonadState a m, MsgBuffer msg a) => [msg] -> m ()
mpush_msgs msgs = get >>= put . push_msgs msgs
mpull_msg :: (MonadState a m, MsgBuffer msg a) => m (Maybe msg)
mpull_msg = do
    a <- get
    case pull_msg a of
        Nothing -> return Nothing
        Just (msg, a) -> do
            put a
            return $ Just msg



---------------------------------------

dprocess1   :: (MsgProcess i msg a, MonadState a m, Ord i)
            => msg -> m [(msg, Map i msg)]
dprocess1 msg = do
    isets <- process1__to_iimport_sets msg
    let iset = S.unions isets
    push_outss <- forM (S.toList iset) $ \i -> do
        b <- listened i
        if not b then process1__to_pushouts i else return []
    mpush_msgs $ concat push_outss
    i2ex_lsls <- forM isets $ listen msg
    let im_i2ex_ls_ = fmap ((,) msg) $ concat i2ex_lsls

    -- i may not exist! before and after
    i_ls <- process1__to_iexports msg
    im_di2ex_lsls <- forM i_ls $ flip implement msg
    let patch i (im, di2ex) = (im, M.insert i msg di2ex)
        im_i2ex_ls = concat $ im_i2ex_ls_ :
            zipWith (\i -> fmap $ patch i) i_ls im_di2ex_lsls
    return im_i2ex_ls
dprocess2   :: (MsgProcess i msg a, MonadState a m)
            => msg -> Map i msg -> m ()
dprocess2 msg i2ex = do
    outs <- process2__to_outmsgs msg i2ex
    mpush_msgs outs
process_msg :: (MsgProcess i msg a, MonadState a m)
             => msg -> m ()
process_msg msg = do
    im_i2ex_ls <- process1 msg
    forM_ im_i2ex_ls $ \(im, i2ex) -> process2 im i2ex

process_msgs :: (MsgProcess i msg a, MonadState a m)
              => m ()
process_msgs = do
    may <- mpull_msg
    case may of
        Nothing -> return ()
        Just msg -> do
            process_msg msg
            process_msgs

class (MsgBuffer msg a, MsgLocalAction i msg a)
    => MsgProcess i msg a | a -> i msg where
    -- i - interface
    -- msg listen/import iset, join/export i
    process1 -- implemented by process1__XXX
        :: MonadState a m => msg -> m [(msg, Map i msg)]
    default process1
        :: (MonadState a m, Ord i) => msg -> m [(msg, Map i msg)]
    process1 = dprocess1
    -- fire event
    --   input /= (">-", {})
    process2 -- by process2__XXX
        :: MonadState a m => msg -> Map i msg -> m ()
    process2 = dprocess2

    listen
        :: MonadState a m => msg -> Set i -> m [Map i msg]
        -- what happen if listen twice? implement defined
    listened
        :: MonadState a m => i -> m Bool
        -- has been listened? (not whether be implemented)
    implement -- (i,msg) not in {i:msg}
        :: MonadState a m => i -> msg -> m [(msg, Map i msg)]
class MsgLocalAction i msg a | a -> i msg where
    -- if duplicated iset or i ==>> implement defined behavior
    process1__to_iimport_sets
        :: MonadState a m => msg -> m [Set i]
    process1__to_iexports
        :: MonadState a m => msg -> m [i]
    -- in_msg -> ({i_import_set}, {i_export})
    -- the framework will set:
    --      i_export <- in_msg -< i_import_set
    process1__to_pushouts
        :: MonadState a m => i -> m [msg]
    -- new i -> [pushfwd_outmsg]
    -- when import a flesh interface, 
    --  we will initialize its push forward msgs

    process2__to_outmsgs
        :: MonadState a m => msg -> Map i msg -> m [msg]
    -- (importer_msg, {i:exporter_msg}) -> [out_msg]
    --      precondition: importer_msg -< {i} <- {i:exporter_msg}
    -- the framework will put [out_msg] into queue
    -- what about ?? Writer [msg] m ??
    --    but that will harden debug, I think


class MsgProcessObj i msg a | a -> i msg where
    get_i2exporters :: a -> Map i [msg]
    set_i2exporters :: Map i [msg] -> a -> a
    get_iset2importers :: a -> Map (Set i) [msg]
    set_iset2importers :: Map (Set i) [msg] -> a -> a
    get_listened_interfaces :: a -> Set i
    set_listened_interfaces :: Set i -> a -> a
    get_i2isets :: a -> Map i (Set (Set i))
    set_i2isets :: Map i (Set (Set i)) -> a -> a
    get_listened_isets :: a -> Set (Set i)
    set_listened_isets :: Set (Set i) -> a -> a

    update_when_listening_iset :: Ord i => Set i -> a -> a
    update_when_listening_iset iset a =
        let listened_isets = get_listened_isets a
            listened_isets' = S.insert iset listened_isets
            i2isets = get_i2isets a
            i2isets' = foldr (\i -> M.insertWith'   (\_ -> S.insert iset)
                                                    i (S.singleton iset))
                             i2isets $ S.toList iset
            a' = set_listened_isets listened_isets' a
            a'' = set_i2isets i2isets' a'
            a_ = if iset `S.member` listened_isets then a
                 else a''

            listened_iset = get_listened_interfaces a_
            listened_iset' = listened_iset `S.union` iset
        in  set_listened_interfaces listened_iset' a_

    iset2i2ex_ls :: Ord i => Set i -> a -> [Map i msg]
    iset2i2ex_ls iset a =
        let
            i2exs = get_i2exporters a
            i_exs_ls =  [ (i, M.findWithDefault [] i i2exs)
                        | i <- S.toList iset]
            i_ex_lsls = ungroup_half i_exs_ls
            i2ex_ls = fmap M.fromList $ direct_product i_ex_lsls
        in  i2ex_ls
instance ( MsgBuffer msg a, MsgLocalAction i msg a
         , MsgProcessObj i msg a, Ord i)
    => MsgProcess i msg a where
    listened i = do
        a <- get
        return $ i `C.member` get_listened_interfaces a
    listen im iset = do
        a <- get
        let a' = set_iset2importers
                (M.insertWith' (++) iset [im] $ get_iset2importers a) a
            a'' = update_when_listening_iset iset a'
        put a''
        return $ iset2i2ex_ls iset a
    implement i ex = do
        a <- get
        let i2isets = get_i2isets a
            iset2importers = get_iset2importers a
            i2exporters = get_i2exporters a
            i2exporters' = M.insertWith' (++) i [ex] i2exporters
            a' = set_i2exporters i2exporters' a
        put a'
        return  [ (im, di2ex)
                | iset <- S.toList $ M.findWithDefault S.empty i i2isets
                , im <- iset2importers M.! iset
                , let diset = S.delete i iset
                , di2ex <- iset2i2ex_ls diset a]



class (C.Buffer msg q, C.Set msg mset, C.Insert msg mset)
    => MsgBufferObj msg mset q a | a -> q mset where
    get_msg_queue :: a -> q
    set_msg_queue :: q -> a -> a
    get_old_msg_set :: a -> mset
    set_old_msg_set :: mset -> a -> a

instance MsgBufferObj msg mset q a => MsgBuffer msg a where
    push_msg msg a =
        let old_msgs = get_old_msg_set a
            q = get_msg_queue a
            old_msgs' = msg `C.insert` old_msgs
            q' = msg `C.insert` q
            a' = set_old_msg_set old_msgs' . set_msg_queue q' $ a
        in  if msg `C.member` old_msgs
            then a
            else a'
    pull_msg a = case C.pop $ get_msg_queue a of
        Nothing -> Nothing
        Just (msg, q') -> Just (msg, set_msg_queue q' a)

{-instance Ord msg => MsgBuffer msg (Parsing i msg) where
    push_msg msg a = if msg `S.member` old_msgs a
                        then a
                        else a  { queue = msg : queue a
                                , old_msgs = S.insert msg $ old_msgs a}
    pull_msg a = case queue a of
                    [] -> Nothing
                    msg : msgs -> Just (msg, a {queue = msgs})
-}
-- instance Ord msg => MsgProcess i msg (Parsing i msg) where
--    precondition







