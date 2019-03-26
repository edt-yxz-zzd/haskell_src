{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts #-}
{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , Rank2Types
            , KindSignatures
            , ScopedTypeVariables #-}
            -- 


{-

    message process framework

    {interface_object : ({importer}, {exporter})}
    -- interface_object/importer/exporter are all with data
    importer --< face <<- exporter
    importer listens/waits on interfaces
    exporter implements interfaces

    interface_object :: Interface
    importer, exporter :: Msg

    process1 :: s -> msg -> (s, [import], [export], [out_msg])
    s - working state
    import, export :: Interface
    msg, out_msg :: Msg
    export <<- msg --< import
    msg implements export_interface
    msg listens/wants on import_interface

    -- 2 means kind of 2, i.e. two msgs
    process2 :: s -> importer -> Interface -> exporter -> (s, [out_msg])

    put_msg/pop_msg
-}

import Control.Monad.State.Class
import Data.List (foldl')
import Prelude hiding (lookup)
import Container
--import Control.Monad.Trans.State.Strict
import Boxed
import Property

class AsProperty p v m => AsMsgQueue p v m | m -> v p where
    get_queue :: m -> v -- set(queue) == processing
    get_queue x = pget (wrap x :: p m)
    set_queue :: v -> m -> m
    set_queue q x = unwrap $ pset q (wrap x :: p m)
class AsProperty p v m => AsKnownMsgs p v m | m -> v p where
    get_known_msgs :: m -> v -- all msgs
    get_known_msgs x = pget (wrap x :: p m)
    set_known_msgs :: v -> m -> m
    set_known_msgs msgs x = unwrap $ pset msgs (wrap x :: p m)

class AsProperty p v m => AsMsgProcessState p v m | m -> v p where
    get_msg_process_state :: m -> v
    get_msg_process_state x = pget (wrap x :: p m)
    set_msg_process_state :: v -> m -> m
    set_msg_process_state s x = unwrap $ pset s (wrap x :: p m)
class AsProperty p v m => AsMsgProcessInterfaceMap p v m | m -> v p where
    get_msg_process_interfance_map:: m -> v
    get_msg_process_interfance_map x = pget (wrap x :: p m)
    set_msg_process_interfance_map :: v -> m -> m
    set_msg_process_interfance_map s x = unwrap $ pset s (wrap x :: p m)

class AsProperty p v m => AsMsgProcessMsgOnce p v m | m -> v p where
    get_msg_process_msg_once :: m -> v
    get_msg_process_msg_once x = pget (wrap x :: p m)
    set_msg_process_msg_once :: v -> m -> m
    set_msg_process_msg_once s x = unwrap $ pset s (wrap x :: p m)





class (Nullable a set, Member a set, Insert a set, Remove a set,
       Buffer a q, AsKnownMsgs as_msgs set m, AsMsgQueue as_queue q m) 
    => MsgOnce a as_queue q as_msgs set m
    -- NOTE: without "| m -> as_msgs as_queue" will error when using pget
    -- Now I move them into AsMsgQueue and AsKnownMsgs
    where

    -- is_processed :: a -> m -> Bool
    -- is_processed msg obj = contains msg $ get_processed obj
    -- too slow : is_processing :: a -> m -> Bool
    is_known_msg :: a -> m -> Bool
    is_known_msg msg m = contains msg $ get_known_msgs m
    put_msg :: a -> m -> m
    put_msg msg obj = if is_known_msg msg obj then obj else
        set_queue (insert msg $ get_queue obj) obj
    pop_msg :: m -> Maybe (a, m)
    pop_msg obj = pop (get_queue obj) >>= \(msg, q) -> 
        return (msg, set_queue q obj)

    put_msgs :: [a] -> m -> m
    put_msgs msgs obj = foldl' (flip put_msg) obj msgs



instance (Nullable a set, Member a set, Insert a set, Remove a set,
       Buffer a q, AsKnownMsgs as_msgs set m, AsMsgQueue as_queue q m) 
    => MsgOnce a as_queue q as_msgs set m where

class MonadState s m => MsgProcessState i msg s m | m -> i msg s where
    -- process1 :: s -> msg -> (s, {import}, {export}, {out_msg})
    process1 :: msg -> m ([i], [i], [msg])
    -- process2 :: s -> importer->Interface->exporter -> (s, {out_msg})
    process2 :: msg -> i -> msg -> m [msg]
class (MsgOnce msg as_queue q as_msgs set mo,
    Iterable msg set, DynMap pair i ([msg], [msg]) map,
    -- AsMsgProcessState as_state s m, 
    AsMsgProcessMsgOnce as_mo mo mp,
    AsMsgProcessInterfaceMap as_i2ios map mp
    )
    => MsgProcess msg as_queue q as_msgs set
        as_i2ios pair i map as_mo mo mp
    -- s - state; q - queue; i - Interface
    | mp -> mo pair i map where

    process_one_msg :: MsgProcessState i msg s m => m mp -> m mp
    process_one_msg mmp = do
        mp <- mmp
        let mo_ = get_msg_process_msg_once mp
        case pop_msg mo_ of
         Nothing -> return mp
         Just (msg, mo) -> do
            (imports, exports, msgs1) <- process1 msg
            let (mp', iie_lsls) = updateI mp msg exports imports
            msgs2 <- updateS $ concat iie_lsls
            let mo' = put_msgs msgs1 . put_msgs msgs2 $ mo
            return $ set_msg_process_msg_once mo' mp'
        where
            updateI mp msg exports imports =
                let
                    i2ios = get_msg_process_interfance_map mp
                    (iie_lsls', i2ios') = add_exports msg exports i2ios []
                    (iie_lsls'', i2ios'') = add_imports msg imports i2ios' iie_lsls'
                    mp' = set_msg_process_interfance_map i2ios'' mp
                in (mp', iie_lsls'')

            update msgss ((importer, interface, exporter):iie_ls) = do
                msgs' <- process2 importer interface exporter
                update (msgs':msgss) iie_ls
            update a [] = return a
            updateS iie_ls = do
                msgss <- update [] iie_ls
                return $ concat msgss

            -- msg implement exports
            -- lookup_i2ios i i2ios = 
            --    maybe (empty, empty) snd (lookup i i2ios)

            add_export msg (iie_lsls, i2ios) iexport =
                -- not contains msg exporters -- msg must be new!
                -- update_default_before iexport (empty, empty)
                --     (\(ims, exs)->(ims, msg:exs)) i2ios
                --  but we needs ims!
                let (importers, i2ios') = update_call_default_before
                        iexport (empty, empty)
                        (\(ims, exs)->(ims, (ims, msg:exs))) i2ios
                -- let (importers, exporters) = lookup_i2ios iexport i2ios
                --    i2ios' = set_item (iexport, (importers, msg : exporters)) i2ios
                in  (map (\im->(im, iexport, msg)) importers : iie_lsls,
                     i2ios')
            add_exports msg exports i2ios iie_lsls =
                foldl' (add_export msg) (iie_lsls, i2ios) exports
            add_import msg (iie_lsls, i2ios) iimport =
                let (exporters, i2ios') = update_call_default_before
                        iimport (empty, empty)
                        (\(ims, exs)->(exs, (msg:ims, exs))) i2ios
                in  (map (\ex->(msg, iimport, ex)) exporters : iie_lsls,
                     i2ios')
            add_imports msg imports i2ios iie_lsls =
                foldl' (add_import msg) (iie_lsls, i2ios) imports

{-
ghci bug:
*Container> :t maybe
maybe :: b -> (a -> b) -> Maybe a -> b
-}









{-
pull :: (MsgOnce s msg q set mo, Monad m) => StateT mo m (Maybe msg)
pull = do 
    mo <- get
    case pop_msg mo of
        Nothing -> return Nothing
        Just (msg, mo) -> do
            put mo
            return (Just msg)

push :: (MsgOnce s msg q set mo, Monad m) => msg -> StateT mo m ()
push msg = modify $ put_msg msg
-}


