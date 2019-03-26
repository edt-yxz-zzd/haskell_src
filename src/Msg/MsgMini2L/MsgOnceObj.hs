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


module MsgOnceObj where
import MsgOnce
import qualified Container as C
import ToDoc
import NamedTuple
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

data MsgOnceObj x q i msg = MsgOnceObj
    { queue :: q
    , old_msgs :: Set msg
    , iset2importers :: Map (Set i) [msg]
    , i2exporters :: Map i [msg]
    , listened_interfaces :: Set i
    , i2isets :: Map i (Set (Set i))
    , listened_isets :: Set (Set i)
    , other_data :: x
    }
    deriving (Eq, Ord, Show, Read)


instance (ToDoc x, ToDoc q, ToDoc i, ToDoc msg)
    => ToDoc (MsgOnceObj x q i msg) where
    to_doc MsgOnceObj { queue = queue
                    , old_msgs = old_msgs
                    , iset2importers = iset2importers
                    , i2exporters = i2exporters
                    , listened_interfaces = listened_interfaces
                    , i2isets = i2isets
                    , listened_isets = listened_isets
                    , other_data = other_data
                    } =
        to_doc ("queue" -: queue //
                "old_msgs" -: old_msgs //
                "iset2importers" -: iset2importers //
                "i2exporters" -: i2exporters //
                "listened_interfaces" -: listened_interfaces //
                "i2isets" -: i2isets //
                "listened_isets" -: listened_isets ///
                "other_data" -: other_data
                )



class Msg msg where
    msg_root :: msg -- ">-"
newMsgOnceObj :: (Msg msg, C.Buffer msg q) => x -> MsgOnceObj x q i msg
newMsgOnceObj x = MsgOnceObj
                    { queue = C.singleton msg_root
                    , old_msgs = S.empty
                    , iset2importers = M.empty
                    , i2exporters = M.empty
                    , listened_interfaces = S.empty
                    , i2isets = M.empty
                    , listened_isets = S.empty
                    , other_data = x
                    }

instance (Ord msg, C.Buffer msg q)
    => MsgBufferObj msg (Set msg) q (MsgOnceObj x q i msg) where
    get_msg_queue = queue
    set_msg_queue q p = p { queue = q }
    get_old_msg_set = old_msgs
    set_old_msg_set mset p = p { old_msgs = mset }
instance MsgProcessObj i msg (MsgOnceObj x q i msg) where
    get_i2exporters = i2exporters
    set_i2exporters m p = p { i2exporters = m }
    get_iset2importers = iset2importers
    set_iset2importers m p = p { iset2importers = m }
    get_listened_interfaces = listened_interfaces
    set_listened_interfaces m p = p { listened_interfaces = m }
    get_i2isets = i2isets
    set_i2isets m p = p { i2isets = m }
    get_listened_isets = listened_isets
    set_listened_isets m p = p { listened_isets = m }

{- i msg
import Text.PrettyPrint (text)
import MsgOnce
import MsgOnceObj
import ToDoc

instance Show (I st sym pos tk)
    => ToDoc (I st sym pos tk) where
    to_doc = text . show
instance Show (Msg st sym pos tk)
    => ToDoc (Msg st sym pos tk) where
    to_doc = text . show

instance Msg (Msg st sym pos tk) where
    msg_root = MRoot

type Obj i msg = MsgOnceObj () [msg] i msg
type WObj st sym pos tk = Obj (I st sym pos tk) (Msg st sym pos tk)


instance ( i~I st sym pos tk, msg~Msg st sym pos tk
         , Ord i, Ord msg, Token sym tk)
    => MsgLocalAction i msg (Obj i msg) where
-- -}

