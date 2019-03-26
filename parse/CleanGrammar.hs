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



{- # LANGUAGE  DatatypeContexts
            , ExplicitForAll
 #-}

module CleanGrammar
    ( clean_grammar
    , GrammarNT
    , Grammar(..)
    , Symbol
    , mkGrammar
    )
where
import ToDoc
import MsgOnceObj
import MsgOnce
import Text.PrettyPrint (text)
----
import qualified Data.Map as M -- (Map)
import qualified Data.Set as S -- (Set)
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.State


------------ terminals, starts, rules
type Symbol = Either -- Symbol n t
data Grammar sym n t = Grammar
    { terminals :: [t]
    , starts :: [n]
    , rules :: [(n, [sym])]
    }
    deriving (Show, Read, Eq, Ord)
type GrammarNT n t = Grammar (Either n t) n t

clean_grammar :: (Ord n, Ord t, sym~Symbol n t, grammar~Grammar sym n t)
    => grammar -> grammar
clean_grammar g = output2grammar r where
    r = process (grammar2msgs g) newCleanObj
    {-
    m = do
        mpush_msgs $ grammar2msgs g
        process_msgs
    r = execState m newCleanObj
    -}
grammar2msgs Grammar {terminals=ts, starts=ss, rules=rs}
    =  [MToken t | t<-ts]
    ++ [MStart s | s<-ss]
    ++ [MRule n syms syms | (n, syms)<-rs]
-- output2grammar CleanObj {i2exporters = i2exs, ..} = undefined
output2grammar :: (Ord n, Ord t, Ord name, sym~Symbol n t)
    => WCleanObj n name t -> Grammar sym n t
output2grammar clean_obj = g where
    i2exs = i2exporters clean_obj
    f i = M.findWithDefault [] i i2exs
    ss = [n | MOutput_Start n <- f IOutput_XStart]
    ts = [t | MOutput_Sym (Right t) <- f IOutput_XSym]
    rs = [(n, syms) | MOutput_Rule n name syms <- f IOutput_XRule]
    -- name = [sym]
    g = Grammar {terminals=ts, starts=ss, rules=rs}

mkGrammar :: (t~n, s~t, sym~Symbol n t, Ord s)
    => [t] -> [n] -> [[s]] -> Grammar sym n t
mkGrammar ts ss rs = Grammar {terminals=ts, starts=ss, rules=rs'} where
    rules' = filter (not . null) rs
    ts' = S.fromList ts
    f s = if S.member s ts' then Right s else Left s
    rs' = fmap (\(n:ls) -> (n, fmap f ls)) rules'
grammar = mkGrammar (words "a b z") (words "S") $ fmap words
    [ "S    a A"
    , "S    D b"
    , "A    B C"
    , "B    a a"
    , "C    A S"
    , "C"
    , "C    D"
    ]
out_grammar = clean_grammar grammar





-- data (forall sym n name t. sym ~ Symbol n t) => IClean n name t
data IClean n name t
    = IOutput_XRule
    | IOutput_XStart
    | IOutput_XSym -- sym
    | IProductiveLeftXNameRights n
    | IProductive_Sym (Symbol n t) -- sym
    | IXRule
    | IXStart
    | IXToken
    deriving (Show, Read, Eq, Ord)
    -- where type Sym = Symbol n t
data MClean n name t
    = MRoot -- ">-",
    | MOutput_Rule n name [Symbol n t]
    | MOutput_Start n
    | MOutput_Sym (Symbol n t)
    | MProductive (Symbol n t)
    | MProductiveRule n name [Symbol n t]
    | MReachable (Symbol n t)
    | MReachableLeft n
    | MRule n name [Symbol n t]
    | MRuleTail n name [Symbol n t] [Symbol n t]
    | MStart n
    | MToken t
    deriving (Show, Read, Eq, Ord)
instance Show (IClean n name sym)
    => ToDoc (IClean n name sym) where
    to_doc = text . show
instance Show (MClean n name sym)
    => ToDoc (MClean n name sym) where
    to_doc = text . show

-- type CleanObj n name sym = MsgOnceObj () 
type Other = ()
instance Msg (MClean n name sym) where
    msg_root = MRoot

type CleanObj i msg = MsgOnceObj Other [msg] i msg
type WCleanObj n name sym =
    CleanObj (IClean n name sym) (MClean n name sym)



newCleanObj :: WCleanObj n name sym
newCleanObj = newMsgOnceObj ()


-- -}
-- -}
instance ( i~IClean n name sym, msg~MClean n name sym
         , Ord i, Ord msg, Ord name)
    => MsgLocalAction i msg (CleanObj i msg) where
  process1__to_iimport_sets msg = return . fmap S.fromList $ case msg of
    MRoot -> [[]]
    MProductiveRule n name syms -> [[]]
    MReachable sym -> [[]]
    MReachableLeft n -> [[IProductiveLeftXNameRights n]]
    MRule n name syms -> [[]]
    MRuleTail n name syms (sym:_) -> [[IProductive_Sym sym]]
    MRuleTail n name syms [] -> [[]]
    MStart n -> [[IProductive_Sym (Left n)]]
    MToken t -> [[]]
    _ -> []
  process1__to_iexports msg = return $ case msg of
    MOutput_Rule n name syms -> [IOutput_XRule]
    MOutput_Start n -> [IOutput_XStart]
    MOutput_Sym sym -> [IOutput_XSym]
    MProductive sym -> [IProductive_Sym sym]
    MProductiveRule n name syms -> [IProductiveLeftXNameRights n]
    MRule n name syms -> [IXRule]
    MStart n -> [IXStart]
    MToken t -> [IXToken]
    _ -> []
  process1__to_pushouts i = return $ case i of
    _ -> []
  process2__to_outmsgs im i2ex =
    let items = M.toAscList i2ex
        keys = fmap fst items
        exs = fmap snd items
    in  return $ case im of
      MRoot -> case keys of
        [] -> case exs of
          [] -> [] -- [MRule n name syms, MStart n, MToken t]
      MProductiveRule n name syms -> case keys of
        [] -> case exs of
          [] -> [MProductive (Left n)]
      MReachable sym -> case keys of
        [] -> case exs of
          [] -> [MOutput_Sym sym] ++ case sym of
            Left n -> [MReachableLeft n]
            _ -> []
      MReachableLeft n -> case keys of
        [IProductiveLeftXNameRights n] -> case exs of
          [MProductiveRule n name syms] ->
            MOutput_Rule n name syms : map MReachable syms
      MRule n name syms -> case keys of
        [] -> case exs of
          [] -> [MRuleTail n name syms syms]
      MRuleTail n name syms [] -> case keys of
        [] -> case exs of
          [] -> [MProductiveRule n name syms]
      MRuleTail n name syms (sym:syms') -> case keys of
        [IProductive_Sym sym] -> case exs of
          [MProductive sym] -> [MRuleTail n name syms syms']
      MStart n -> case keys of
        [IProductive_Sym sym] -> case exs of
          [MProductive sym] -> [MOutput_Start n, MReachable sym]
      MToken t -> case keys of
        [] -> case exs of
          [] -> [MProductive (Right t)]















