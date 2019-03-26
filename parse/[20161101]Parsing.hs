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


import MsgOnce
import qualified Container as C
import ToDoc
import NamedTuple
import Text.PrettyPrint (text)
import SeedUtils (group_pairs, findD)

import qualified Data.Map as M -- (Map)
import qualified Data.Set as S -- (Set)
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.State
-- import System.Console.GetOpt
import System.Environment
-- import System.IO
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import Text.Parsec hiding (parse)
import Text.Parsec.String
import SeedUtils_Parsec hiding (keyword)

{-
    try_grammar.txt: "S = S S | a |"
    try_grammar_src.txt: "aaaaa"
runghc Parsing.hs try_grammar.txt S < try_grammar_src.txt

-}
main = do
    ls <- getArgs
    when (null ls) . error $
        "usage: <grammar_fname> [start_symbol...] '<' <source_fname>\n" ++
        "grammar ::= (SYM '=' SYM* ('|' SYM)*)*\n"
    let (grammar_fname:start_syms) = ls
    -- withFile grammar_fname ReadMode $ \h ->
    {-
    bytes <- B.readFile grammar_fname
    let t = E.decodeUtf8 bytes
        s = T.unpack t
    -}
    r <- parse_grammar grammar_fname
    case r of
      Left err -> print err
      Right grammar ->
        let rules = [(left, rights)
                    | FileRule left rightss <- grammar
                    , rights <- rightss]
        in  do
            -- print rules
            t <- getContents
            let g = esparse (start_syms, rules) t
                    :: Grammar (Integer, String, Integer)
            print $ to_doc g
-- A = B C | D |
type FileGrammar = [FileRule]
data FileRule = FileRule String [[String]]
grammar = many rule >>< eof
rule = do
    left <- left_part
    rightss <- right_part
    return $ FileRule left rightss
left_part = word >>< keyword_ "="
right_word = try $ word >>< notFollowedBy (char '=')
right_part = sepBy (many right_word) $ keyword_ "|"
word = wrap $ many1 wordchar
wrap = (>>< ignores)
ignores = spaces
wordchar = notFollowedBy space >> noneOf "=|"
keyword_ = wrap . try . string
parse_grammar :: String -> IO (Either ParseError FileGrammar)
    -- [(String, [[String]])]
parse_grammar fname = parseFromFile grammar fname

-------------------------

class Token sym tk | tk -> sym where
    token2sym :: tk -> sym
data Parsing i msg = Parsing    { queue :: [msg]
                                , old_msgs :: Set msg
                                , iset2importers :: Map (Set i) [msg]
                                , i2exporters :: Map i [msg]
                                , listened_interfaces :: Set i
                                , i2isets :: Map i (Set (Set i))
                                , listened_isets :: Set (Set i)
                                }
    deriving (Eq, Ord, Show, Read)


instance (ToDoc i, ToDoc msg) => ToDoc (Parsing i msg) where
    to_doc Parsing  { queue = queue, old_msgs = old_msgs
                    , iset2importers = iset2importers
                    , i2exporters = i2exporters
                    , listened_interfaces = listened_interfaces
                    , i2isets = i2isets
                    , listened_isets = listened_isets} =
        to_doc ("queue" -: queue //
                "old_msgs" -: old_msgs //
                "iset2importers" -: iset2importers //
                "i2exporters" -: i2exporters //
                "listened_interfaces" -: listened_interfaces //
                "i2isets" -: i2isets ///
                "listened_isets" -: listened_isets
                )
instance Show (ParsingI st sym pos tk)
    => ToDoc (ParsingI st sym pos tk) where
    to_doc = text . show
instance Show (ParsingMsg st sym pos tk)
    => ToDoc (ParsingMsg st sym pos tk) where
    to_doc = text . show

type WParsing st sym pos tk =
    Parsing (ParsingI st sym pos tk) (ParsingMsg st sym pos tk)

newParsing :: WParsing st sym pos tk
newParsing = Parsing{ queue = [MRoot]
                    , old_msgs = S.empty
                    , iset2importers = M.empty
                    , i2exporters = M.empty
                    , listened_interfaces = S.empty
                    , i2isets = M.empty
                    , listened_isets = S.empty
                    }

add_rule :: forall a m i msg st sym name pos tk .
            ( MonadState a m, MsgProcess i msg a
            , Ord i, Ord msg, Ord sym, Ord name, Ord pos, Ord tk
            , st ~ (sym, name, Integer)
            , a ~ WParsing st sym pos tk
            )
            => sym -> name -> [sym] -> m ()
add_rule left name rights =
    let len = fromIntegral $ length rights
        states = [(left, name, i) | i <- [0..len]]
    in  do
    mpush_msgs $[ MSym2StPrim left (left, name, 0)
                , MCFG_NFSM_FinalStPrim (left, name, len)] ++
                [ MCFG_NFSM_TransitionPrim st sym st'
                | (st, sym, st') <- zip3 states rights $ tail states]
add_start_symbol ::
            ( MonadState a m, MsgProcess i msg a
            , Ord st, Ord sym, Ord pos, Ord tk
            , a ~ WParsing st sym pos tk
            )
            => sym -> m ()
add_start_symbol sym = mpush_msgs [MStartSymPrim sym]

feed_ls ::  ( MonadState a m, MsgProcess i msg a
            , Ord st, Ord sym, Ord pos, Ord tk, Token sym tk
            , Integral pos
            , a ~ WParsing st sym pos tk
            )
            => pos -> [tk] -> m pos
feed_ls begin tokens = let end = begin + fromIntegral (length tokens) in do
    mpush_msgs  [ MTokenNFSM_TransitionPrim pos tk (pos+1)
                | (tk, pos) <- zip tokens [begin..end-1]]
    return end
feed_all :: ( MonadState a m, MsgProcess i msg a
            , Ord st, Ord sym, Ord pos, Ord tk, Token sym tk
            , Integral pos
            , a ~ WParsing st sym pos tk
            )
            => [tk] -> m ()
feed_all tokens = let begin = 0 in do
    end <- feed_ls begin tokens
    mpush_msgs  [ MTokenNFSM_FinalPosPrim end
                , MTokenNFSM_InitialPosPrim begin]
parser ::   ( MonadState a m, MsgProcess i msg a
            , Ord name, Ord sym, Ord pos, Ord tk, Token sym tk
            , st ~ (sym, name, Integer)
            --, Integral pos
            , a ~ WParsing st sym pos tk
            )
            => [sym] -> [(sym, name, [sym])] -> m ()
parser start_syms rules = do
    forM_ start_syms add_start_symbol
    forM_ rules $ \(left, name, rights) -> add_rule left name rights
    process_msgs
parse ::    ( MsgProcess i msg a
            , Ord name, Ord sym, Ord pos, Ord tk, Token sym tk
            , st ~ (sym, name, Integer)
            , Integral pos
            , a ~ WParsing st sym pos tk
            )
            => [sym] -> [(sym, name, [sym])] -> [tk] -> a
parse start_syms rules tokens = execState m newParsing where
    m = do
        parser start_syms rules
        feed_all tokens
        process_msgs

instance Token String String where
    token2sym = id
instance Token String Char where
    token2sym = (:[])
rrrr = parse    ["S"]
                [ ("S", "S-1", words "S S")
                , ("S", "S-2", words "a")
                , ("S", "S-3", words "")]
                "aaaaa"
exxx = i2exporters rrrr M.! IOutput_XSym2St
sxxx = i2exporters rrrr M.! IOutput_XStartSym
main1 = print $ to_doc rrrr



sparse ::   ( MsgProcess i msg a
            , Ord name, Ord sym, Ord pos, Ord tk, Token sym tk
            , st ~ (sym, name, Integer)
            , name ~ [sym]
            , Integral pos
            , a ~ WParsing st sym pos tk
            )
            => [sym] -> [(sym, [sym])] -> [tk] -> a
sparse start_syms rules tokens = parse start_syms rules' tokens where
    rules' = [(left, rights, rights) | (left, rights) <- rules]


esGrammar = (["S"], [("S", ["S", "S"]), ("S", ["a"]), ("S", [])])
src = "aaaaa"
rrr = esparse esGrammar src :: Grammar (Integer, String, Integer)
-- (i, "S", j) = (i, "S", k) (k, "S", j) for i <= k <= j
--             = ; if i == j
--             = (i, "a", j) if j = i+1
-- count (i, "S", i+n)  = count (0, "S", n)
--                      = [n>=0](n+1) + [n==0] + [n==1]
--                      = [n>=0](n+1) + [n<2]
-- total n = sum sum count (i,j) {0<=i<=j}{0<=j<=n}
--          = total (n-1) + sum count (i,n) {0<=i<=n}
--          = total (n-1) + sum count (0,n-i) {0<=i<=n}
--          = total (n-1) + sum count (0,i) {0<=i<=n}
--          = total (n-1) + sum [i>=0](i+1) + [i<2] {0<=i<=n}
--          = total (n-1) + sum (i+1) {0<=i<=n} + sum [i<2] {0<=i<=n}
--          = total (n-1) + [n>=0] (n+1)(n+2)/2 + [n==0] + [n>0]2
--          = sum [i>=0] (i+1)(i+2)/2 + [i==0] + [i>0]2 {0<=i<=n}
--              0:1, 1:4, 2:10, 3:20
--          = [n>=0] (n+1)(n+2)(n+3)/6 + [n>=0] + [n>0](2n)
--          = [n>=0] ((n+1)(n+2)(n+3)/6 + 1 + 2n)
--          = [n>=0] ((nn+3n+2)(n+3) + 6+12n)/6
--          = [n>=0] (nnn+3nn+2n + 3nn+9n+6 + 6+12n)/6
--          = [n>=0] (nnn+6nn+23n + 12)/6
_total n
    | n < 0 = 0
    | otherwise = (n*n*n + 6*n*n + 23*n + 12) `div` 6
bxxx = _total (length src) == length (snd rrr)
main2 = print $ to_doc rrr
-----------------(start_syms, [(left, [right])])
type Grammar sym = ([sym], [(sym, [sym])])
unique_and_sort :: Ord a => [a] -> [a]
unique_and_sort = S.toAscList . S.fromList
esparse ::  ( MsgProcess i msg a
            , Ord name, Ord sym, Ord pos, Ord tk, Token sym tk
            , st ~ (sym, name, Integer)
            , name ~ [sym]
            , Integral pos
            , a ~ WParsing st sym pos tk
            , sym' ~ (pos, sym, pos)
            , st' ~ (pos, st, pos)
            )
            => Grammar sym -> [tk] -> Grammar sym'
esparse (start_syms, rules) tokens =
    (unique_and_sort start_syms', unique_and_sort rules') where
    p = sparse start_syms rules tokens
    i2exs = i2exporters p
    start_syms' = [ sym | MOutput_StartSym sym
                    <- findD IOutput_XStartSym i2exs]
    sym_sts' = [(sym, st) | MOutput_Sym2St sym st
                    <- findD IOutput_XSym2St i2exs]
    --init_sts' = fmap snd sym_sts'
    sym2sts' = group_pairs sym_sts'
    st2next_sts' = group_pairs
        [(st0', (next', st1')) | MOutput_St2NextSt st0' next' st1'
            <- findD IOutput_XSt2NextSt i2exs]
    st2next2sts' = M.map group_pairs st2next_sts'
    st2symss0' = M.fromList [(st', [[]]) | MOutput_FinalSt st'
                            <- findD IOutput_XFinalSt i2exs]
    (sym2symss', st2symss') = execState (fill_syms start_syms')
                                        (M.empty, st2symss0')
    sym_syms_ls' = [(sym', syms')
        | (sym', symss') <- M.toList sym2symss', syms' <- symss']
    rules' = sym_syms_ls'
    -------------------
    fill_st init_st' = do
        st2symss' <- get
        unless (M.member init_st' st2symss') $ do
            let next_sts' = st2next_sts' M.! init_st' 
            forM_ (fmap snd next_sts') fill_st
            st2symss'' <- get
            let symss' = [ next':syms'
                    | (next', st') <- next_sts'
                    , syms' <- st2symss'' M.! st']
            put $ M.insert init_st' symss' st2symss''

    fill_sts = mapM_ fill_st

    fill_sym sym' = do
        (sym2symss', st2symss') <- get
        unless (M.member sym' sym2symss') $ do
            -- bug : sts' = sym2sts' M.! sym'
            -- since sym' may be a terminal
            -- i.e. by MOutput_Sym2Token
            let sts' = findD sym' sym2sts'
                st2symss'' = execState (fill_sts sts') st2symss'
                symss' = concat
                        [st2symss'' M.! st' | st' <- sts']
                sym2symss'' = M.insert sym' symss' sym2symss'
            put (sym2symss'', st2symss'')
            fill_syms $ concat symss'
    fill_syms = mapM_ fill_sym




{-
"srcs" -: {"MCFG_NFSM_FinalStPrim",
            "MCFG_NFSM_NullTransitionPrim",
            "MCFG_NFSM_TransitionPrim",
            "MStartSymPrim",
            "MSym2StPrim",
            "MTokenNFSM_FinalPosPrim",
            "MTokenNFSM_InitialPosPrim",
            "MTokenNFSM_NullTransitionPrim",
            "MTokenNFSM_TransitionPrim"}

-}

-- state, symbol, position, token
data ParsingI st sym pos tk
    = ICFG_NFSM_NullTransitionEnd st
    | IFinalSt st
    | IOutput_XFinalSt
    | IOutput_XSt2NextSt
    | IOutput_XStartSym
    | IOutput_XSym2St
    | IOutput_XSym2Token
    | IPosStXPos pos st
    | IPosSymXPos pos sym
    | IStNextXSt st sym
    | IStXNext st
    | ISym2XSt sym
    | ITokenNFSM_FinalPos pos
    | ITokenNFSM_NullTransitionEnd pos
    | ITokenNFSM_Transition_PosSymXPos pos sym
    | ITokenNFSM_XInitialPos
    | IXStartSym
    deriving (Eq, Ord, Show, Read)

data ParsingMsg st sym pos tk
    = MRoot
    | MCFG_NFSM_FinalSt st
    | MCFG_NFSM_FinalStPrim st
    | MCFG_NFSM_NewSt st
    | MCFG_NFSM_NullTransition st st
    | MCFG_NFSM_NullTransitionPrim st st
    | MCFG_NFSM_Transition st sym st
    | MCFG_NFSM_TransitionPrim st sym st
    | MMainPosSymXPos pos sym
    | MMaybeOutput_StartSym (pos, sym, pos)
    | MOutput_FinalSt (pos, st, pos)
    | MOutput_St2NextSt (pos, st, pos) (pos, sym, pos) (pos, st, pos)
    | MOutput_StartSym (pos, sym, pos)
    | MOutput_Sym2St (pos, sym, pos) (pos, st, pos)
    | MOutput_Sym2Token (pos, sym, pos) (pos, tk, pos)
    | MPosStNextPosStXPos pos st sym pos st
    | MPosStNextPosXSt pos st sym pos
    | MPosStNextXPos pos st sym
    | MPosStPos pos st pos
    | MPosStXPos pos st
    | MPosSymPos pos sym pos
    | MPosSymStXPos pos sym st
    | MPosSymXPos pos sym
    | MStNext st sym
    | MStartSymPrim sym
    | MSym2StPrim sym st
    | MTokenNFSM_FinalPos pos
    | MTokenNFSM_FinalPosPrim pos
    | MTokenNFSM_InitialPosPrim pos
    | MTokenNFSM_NewPos pos
    | MTokenNFSM_NullTransition pos pos
    | MTokenNFSM_NullTransitionPrim pos pos
    | MTokenNFSM_Transition pos tk pos
    | MTokenNFSM_TransitionPrim pos tk pos
    deriving (Eq, Ord, Show, Read)
{-
"all_msgs_in_use" -: {">-",
                       "MCFG_NFSM_FinalSt",
                       "MCFG_NFSM_FinalStPrim",
                       "MCFG_NFSM_NewSt",
                       "MCFG_NFSM_NullTransition",
                       "MCFG_NFSM_NullTransitionPrim",
                       "MCFG_NFSM_Transition",
                       "MCFG_NFSM_TransitionPrim",
                       "MMainPosSymXPos",
                       "MMaybeOutput_StartSym",
                       "MOutput_FinalSt",
                       "MOutput_St2NextSt",
                       "MOutput_StartSym",
                       "MOutput_Sym2St",
                       "MOutput_Sym2Token",
                       "MPosStNextPosStXPos",
                       "MPosStNextPosXSt",
                       "MPosStNextXPos",
                       "MPosStPos",
                       "MPosStXPos",
                       "MPosSymPos",
                       "MPosSymStXPos",
                       "MPosSymXPos",
                       "MStNext",
                       "MStartSymPrim",
                       "MSym2StPrim",
                       "MTokenNFSM_FinalPos",
                       "MTokenNFSM_FinalPosPrim",
                       "MTokenNFSM_InitialPosPrim",
                       "MTokenNFSM_NewPos",
                       "MTokenNFSM_NullTransition",
                       "MTokenNFSM_NullTransitionPrim",
                       "MTokenNFSM_Transition",
                       "MTokenNFSM_TransitionPrim"},

"decl_iset" -: {"ICFG_NFSM_NullTransitionEnd",
                "IFinalSt",
                "IOutput_XFinalSt",
                "IOutput_XSt2NextSt",
                "IOutput_XStartSym",
                "IOutput_XSym2St",
                "IOutput_XSym2Token",
                "IPosStXPos",
                "IPosSymXPos",
                "IStNextXSt",
                "IStXNext",
                "ISym2XSt",
                "ITokenNFSM_FinalPos",
                "ITokenNFSM_NullTransitionEnd",
                "ITokenNFSM_Transition_PosSymXPos",
                "ITokenNFSM_XInitialPos",
                "IXStartSym"},

-}
instance Ord msg => MsgBufferObj msg (Set msg) [msg] (Parsing i msg) where
    get_msg_queue = queue
    set_msg_queue q p = p { queue = q }
    get_old_msg_set = old_msgs
    set_old_msg_set mset p = p { old_msgs = mset }
instance MsgProcessObj i msg (Parsing i msg) where
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

instance ( i~ParsingI st sym pos tk, msg~ParsingMsg st sym pos tk
         , Ord i, Ord msg, Token sym tk)
    => MsgLocalAction i msg (Parsing i msg) where
    -- push msg ">-" first


    -- if duplicated iset or i ==>> implement defined behavior
    process1__to_iimport_sets msg =
        -- :: MonadState a m => msg -> m [Set i]
      return . fmap S.fromList $ case msg of
        MRoot -> [[], [ITokenNFSM_XInitialPos, IXStartSym]]
        MCFG_NFSM_FinalStPrim st1 -> [[], [ICFG_NFSM_NullTransitionEnd st1]]
        MCFG_NFSM_NewSt st2 -> [[]]
        MCFG_NFSM_NullTransitionPrim st1 st2 -> [[], [ICFG_NFSM_NullTransitionEnd st1]]
        MCFG_NFSM_Transition st0 sym st2 -> [[]]
        MCFG_NFSM_TransitionPrim st1 sym st2 -> [[], [ICFG_NFSM_NullTransitionEnd st1]]
        MMainPosSymXPos pos sym -> [[IPosSymXPos pos sym]]
        MMaybeOutput_StartSym (pos, sym, pos') -> [[ITokenNFSM_FinalPos pos']]
        MPosStNextPosStXPos pos st next pos_ st' -> [[IPosStXPos pos_ st']]
        MPosStNextPosXSt pos st next pos_ -> [[IStNextXSt st next]]
        MPosStNextXPos pos st next -> [[IPosSymXPos pos next]]
        MPosStXPos pos st -> [[IFinalSt st], [IStXNext st]]
        MPosSymStXPos pos sym initial_st -> [[IPosStXPos pos initial_st]]
        MPosSymXPos pos sym -> [[ISym2XSt sym], [ITokenNFSM_Transition_PosSymXPos pos sym]]
        MTokenNFSM_FinalPosPrim pos1 -> [[], [ITokenNFSM_NullTransitionEnd pos1]]
        MTokenNFSM_InitialPosPrim pos -> [[]]
        MTokenNFSM_NewPos pos2 -> [[]]
        MTokenNFSM_NullTransitionPrim pos1 pos2 -> [[], [ITokenNFSM_NullTransitionEnd pos1]]
        MTokenNFSM_TransitionPrim pos1 token pos2 -> [[], [ITokenNFSM_NullTransitionEnd pos1]]
        _ -> []

    process1__to_iexports msg =
        -- :: MonadState a m => msg -> m [i]
      return $ case msg of
        MCFG_NFSM_FinalSt st -> [IFinalSt st]
        MCFG_NFSM_NullTransition st0 st1 -> [ICFG_NFSM_NullTransitionEnd st1]
        MCFG_NFSM_Transition st0 sym st2 -> [IStNextXSt st0 sym]
        MOutput_FinalSt (pos, st, pos') -> [IOutput_XFinalSt]
        MOutput_St2NextSt (_pos, st, _pos') (pos, sym, _pos_) (pos_, st', pos') -> [IOutput_XSt2NextSt]
        MOutput_StartSym (pos, sym, pos') -> [IOutput_XStartSym]
        MOutput_Sym2St (_pos, sym, _pos') (pos, st, pos') -> [IOutput_XSym2St]
        MOutput_Sym2Token (_pos, sym, _pos') (pos, token, pos') -> [IOutput_XSym2Token]
        MPosStPos pos_ st' pos' -> [IPosStXPos pos_ st']
        MPosSymPos pos sym pos' -> [IPosSymXPos pos sym]
        MStNext st sym -> [IStXNext st]
        MStartSymPrim sym -> [IXStartSym]
        MSym2StPrim sym initial_st -> [ISym2XSt sym]
        MTokenNFSM_FinalPos pos -> [ITokenNFSM_FinalPos pos]
        MTokenNFSM_InitialPosPrim pos -> [ITokenNFSM_XInitialPos]
        MTokenNFSM_NullTransition pos0 pos1 -> [ITokenNFSM_NullTransitionEnd pos1]
        MTokenNFSM_Transition pos0 token pos1 -> [ITokenNFSM_Transition_PosSymXPos pos0 (token2sym token)]
        _ -> []

    process1__to_pushouts i =
        -- :: MonadState a m => i -> m [msg]
      return $ case i of
        IPosStXPos pos st -> [MPosStXPos pos st]
        IPosSymXPos pos sym -> [MPosSymXPos pos sym]
        _ -> []


    process2__to_outmsgs im i2ex =
        -- :: MonadState a m => msg -> Map i msg -> m [msg]
    {-
      let items = M.toAscList i2ex
          keys = fmap fst items
          exs = fmap snd items
      in  return $ case im of
        MRoot -> case keys of
          [] -> [] -- error "src msg should be push by hand"
          [ITokenNFSM_XInitialPos, IXStartSym] -> case exs of
            [MTokenNFSM_InitialPosPrim pos, MStartSymPrim sym] ->
                [MMainPosSymXPos pos sym]

-- -}

     let items = M.toAscList i2ex
         keys = fmap fst items
         exs = fmap snd items
     in  return $ case im of
      MRoot -> case keys of
        [] -> [] -- error
        [ITokenNFSM_XInitialPos, IXStartSym] -> case exs of
          [MTokenNFSM_InitialPosPrim pos, MStartSymPrim sym] -> [MMainPosSymXPos pos sym]
      MCFG_NFSM_FinalStPrim st1 -> case keys of
        [] -> case exs of
          [] -> [MCFG_NFSM_NewSt st1]
        [ICFG_NFSM_NullTransitionEnd _st1] -> case exs of
          [MCFG_NFSM_NullTransition st0 __st1] -> [MCFG_NFSM_FinalSt st0]
      MCFG_NFSM_NewSt st -> case keys of
        [] -> case exs of
          [] -> [MCFG_NFSM_NullTransition st st]
      MCFG_NFSM_NullTransitionPrim st1 st2 -> case keys of
        [] -> case exs of
          [] -> [MCFG_NFSM_NewSt st1, MCFG_NFSM_NewSt st2]
        [ICFG_NFSM_NullTransitionEnd st1] -> case exs of
          [MCFG_NFSM_NullTransition st0 _st1] -> [MCFG_NFSM_NullTransition st0 st2]
      MCFG_NFSM_Transition st0 sym st2 -> case keys of
        [] -> case exs of
          [] -> [MStNext st0 sym]
      MCFG_NFSM_TransitionPrim st1 sym st2 -> case keys of
        [] -> case exs of
          [] -> [MCFG_NFSM_NewSt st1, MCFG_NFSM_NewSt st2]
        [ICFG_NFSM_NullTransitionEnd st1] -> case exs of
          [MCFG_NFSM_NullTransition st0 _st1] -> [MCFG_NFSM_Transition st0 sym st2]
      MMainPosSymXPos pos sym -> case keys of
        [IPosSymXPos _pos _sym] -> case exs of
          [MPosSymPos __pos __sym pos'] -> [MMaybeOutput_StartSym (pos, sym, pos')]
      MMaybeOutput_StartSym (pos, sym, pos') -> case keys of
        [ITokenNFSM_FinalPos _pos'] -> case exs of
          [MTokenNFSM_FinalPos __pos'] -> [MOutput_StartSym (pos, sym, pos')]
      MPosStNextPosStXPos pos st next pos_ st' -> case keys of
        [IPosStXPos _pos_ _st'] -> case exs of
          [MPosStPos _pos_ _st' pos'] -> [MOutput_St2NextSt (pos, st, pos') (pos, next, pos_) (pos_, st', pos')
                , MPosStPos pos st pos'] -- bug: MPosStPos pos_ st' pos'
      MPosStNextPosXSt pos st next pos_ -> case keys of
        [IStNextXSt _st _next] -> case exs of
          [MCFG_NFSM_Transition st _next st'] -> [MPosStNextPosStXPos pos st next pos_ st']
      MPosStNextXPos pos st next -> case keys of
        [IPosSymXPos _pos _next] -> case exs of
          [MPosSymPos __pos __next pos'] -> [MPosStNextPosXSt pos st next pos']
      MPosStXPos pos st -> case keys of
        [IFinalSt _st] -> case exs of
          [MCFG_NFSM_FinalSt __st] -> [MOutput_FinalSt (pos, st, pos), MPosStPos pos st pos]
        [IStXNext _st] -> case exs of
          [MStNext _st next] -> [MPosStNextXPos pos st next]
      MPosSymStXPos pos sym initial_st -> case keys of
        [IPosStXPos _pos _initial_st] -> case exs of
          [MPosStPos __pos __initial_st pos'] -> [MOutput_Sym2St (pos, sym, pos') (pos, initial_st, pos'), MPosSymPos pos sym pos']
      MPosSymXPos pos sym -> case keys of
        [ISym2XSt _sym] -> case exs of
          [MSym2StPrim _sym initial_st] -> [MPosSymStXPos pos sym initial_st]
        [ITokenNFSM_Transition_PosSymXPos _pos _sym] -> case exs of
          [MTokenNFSM_Transition _pos token pos'] -> [MOutput_Sym2Token (pos, sym, pos') (pos, token, pos'), MPosSymPos pos sym pos']
      MTokenNFSM_FinalPosPrim pos1 -> case keys of
        [] -> case exs of
          [] -> [MTokenNFSM_NewPos pos1]
        [ITokenNFSM_NullTransitionEnd pos1] -> case exs of
          [MTokenNFSM_NullTransition pos0 pos1] -> [MTokenNFSM_FinalPos pos0]
      MTokenNFSM_InitialPosPrim pos -> case keys of
        [] -> case exs of
          [] -> [MTokenNFSM_NewPos pos]
      MTokenNFSM_NewPos pos -> case keys of
        [] -> case exs of
          [] -> [MTokenNFSM_NullTransition pos pos]
      MTokenNFSM_NullTransitionPrim pos1 pos2 -> case keys of
        [] -> case exs of
          [] -> [MTokenNFSM_NewPos pos1, MTokenNFSM_NewPos pos2]
        [ITokenNFSM_NullTransitionEnd _pos1] -> case exs of
          [MTokenNFSM_NullTransition pos0 _pos1] -> [MTokenNFSM_NullTransition pos0 pos2]
      MTokenNFSM_TransitionPrim pos1 token pos2 -> case keys of
        [] -> case exs of
          [] -> [MTokenNFSM_NewPos pos1, MTokenNFSM_NewPos pos2]
        [ITokenNFSM_NullTransitionEnd _pos1] -> case exs of
          [MTokenNFSM_NullTransition pos0 _pos1] -> [MTokenNFSM_Transition pos0 token pos2]


