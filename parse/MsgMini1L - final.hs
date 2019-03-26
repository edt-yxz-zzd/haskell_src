{-# LANGUAGE FlexibleContexts
    , FlexibleInstances
    , TypeSynonymInstances
    , UndecidableInstances
    , OverlappingInstances
    , TypeFamilies
    , Rank2Types
    , ScopedTypeVariables
    , TypeOperators
    #-}


-- see "parser design.txt"
-- common mistake:
--      1) once word = r"\S+" => r"(?![MI])\S+" => not any keyword
--      2) many (try xxx) v.s. many xxx
--      3) try (string xxx) v.s string xxx
{-
*Main> P.parse (many $ char 'a' >> char 'a') "" "aaa"
Left (line 1, column 4):
unexpected end of input
expecting "a"

*Main> P.parse (many $ string "ab") "" "a"
Left (line 1, column 1):
unexpected end of input
expecting "ab"

*Main> P.parse (skipMany $ string "ab") "" "a"
Left (line 1, column 1):
unexpected end of input
expecting "ab"

-}

---- short hand
-- i - interface; 
-- im - importer; ex - exporter  -- Msg
-- ims - importers; exs - exporters;  -- [] or Set
-- stmt - statement
------- run script: runghc MsgMiniL.hs  Earley.MsgMini.txt


import NamedTuple
import SeedUtils_Parsec
import MonadLogger
import ToDoc
import Control.Monad
import Prelude hiding (log)
import Text.PrettyPrint hiding (char, space)
import Text.Parsec hiding (parse) -- , many, many1)
import Text.Parsec.String
import qualified Text.Parsec as P
import System.Environment
import qualified Data.Map as M -- (Map)
import qualified Data.Set as S -- (Set)
import Data.Map (Map)
import Data.Set (Set)
import SeedUtils
    ( easy_productive_closure
    , group_pairs
    , group_pairs__set
    , merge_x_ys_pairs__set
    , swap_pair_in_ls
    )

main = do
    fnames <- getArgs
    show_files fnames

show_files = sequence_ . fmap show_file
show_file fname = do
    print fname
    r <- parse fname
    -- print r
    case r of 
        Left _ -> print r
        Right ls -> do
            print $ to_doc ls
            c <- compile ls
            print $ to_doc c
    -- show_files fs

-- parse :: String -> Main
parse = parseFromFile msg_mini_main




{-
 - use Python to comile and verify instead of Haskell
 -      not need - list comprehension
---------------- compile
-- c - compiled
-}



instance ToDoc TopStmt where
    to_doc (DirectOutput msg msgs) = py_call2doc "DirectOutput" (A2 msg msgs)
    to_doc (ImportView msg stmts) = py_call2doc "ImportView" (A2 msg stmts)
    to_doc (InterfaceDecl ims i exs) =
        py_call2doc "InterfaceDecl" (A3 ims i exs)
    to_doc (SourceMsgsDecl msgs) = py_call2doc "SourceMsgsDecl" $ A1 msgs
    to_doc (PureOutputInterfacesDecl stmts_wo) = py_call2doc "PureOutputInterfacesDecl" $ A1 stmts_wo
instance ToDoc ImplementedByStmt where
    to_doc (ImplementedByStmt msg msgs) = py_call2doc "ImplementedByStmt" (A2 msg msgs)
instance ToDoc ImportStmt where
    to_doc (ImportStmt i stmts) = py_call2doc "ImportStmt" (A2 i stmts)
instance ToDoc ImportStmtWO where
    to_doc (ImportStmtWO i exs) = py_call2doc "ImportStmtWO" (A2 i exs)

instance ToDoc Msg where
    to_doc (Msg c ls) = py_call2doc "Msg" $ A2 c ls
instance ToDoc Interface where
    to_doc (Interface c ls) = py_call2doc "Interface" $ A2 c ls




-------------------
--
--

compile :: ( Monad m, MConstructor ~ msg, IConstructor ~ i
           , Set MConstructor ~ msgs
           --, m ~ IO
           ) =>
            --
    MsgMiniL -> m ( msgs :-: msgs :-: msgs
                :-: msgs :-: msgs :-: msgs :-: msgs :-: Int
                :-: Map msg msgs :-: Map (msg, i, msg) msgs
                :-: Map i msgs :-: Map i msgs :-: Map i msgs
                :-: Set i :-:: Int :$: String)
            {-}
    MsgMiniL -> m ( ((msgs, msgs, msgs), (msgs, msgs), (msgs, msgs))
                  , (Map msg msgs, Map (msg, i, msg) msgs)
                  , (Map i msgs, Map i msgs)
                  )
            -- -}
compile m = do
    srcs <- to_src_msgs m
    m2outs <- to_m2outs m
    im_i_ex2outs <- to_im_i_ex2outs m
    let rules = make_rules m2outs im_i_ex2outs
    let (fines, bads) = easy_productive_closure rules srcs
    unless (S.null bads) . fail $ "nonreachable msgs: " ++ show bads

    let all_msgs = fines
    i2importers <- to_i2importers m
    i2exporters <- to_i2exporters m
    pure_i2exporters <- to_pure_i2exporters m
    let err1_pure_interfaces =  M.keysSet i2exporters S.\\
                                M.keysSet i2importers
    unless (S.null err1_pure_interfaces) $
        fail $  "no decl pure interface (i.e. without importer)" ++
                show err1_pure_interfaces
    let err2_pure_interfaces =  M.keysSet i2exporters `S.intersection`
                                M.keysSet pure_i2exporters
    unless (S.null err2_pure_interfaces) $
        fail $  "decl as pure interface but with importer" ++
                show err2_pure_interfaces
    let err_exs = merge_values pure_i2exporters S.\\ all_msgs
    unless (S.null err_exs) $
        fail $  "unknown(i.e. nonreachable) exporters in pure interface decl" ++
                show err_exs
    let b = M.size i2importers == M.size i2exporters &&
            M.keysSet i2importers == M.keysSet i2exporters &&
            M.keysSet im_i_ex2outs == (S.fromList . concat .
              M.elems $ M.intersectionWithKey
                (\i ims exs -> [(im, i, ex) |
                    im <- S.toList ims, ex <- S.toList exs])
                i2importers i2exporters)
    unless b $ fail "logic-error"
    let result1 = ( srcs, pure_i2exporters
                  , (i2importers, i2exporters)
                  , m2outs, im_i_ex2outs
                  , all_msgs)
    return result1

    -- pure_outs = all_outs - importers - exporters -- may include srcs
    -- pure_srcs = all_msgs - all_outs
    let importers = merge_values i2importers
    let exporters = merge_values i2exporters
    let all_outs = merge_values m2outs `S.union` merge_values im_i_ex2outs
    let pure_outs = all_outs S.\\ importers S.\\ exporters
    let pure_srcs = all_msgs S.\\ all_outs
    let all_interfaces = M.keysSet i2exporters
    let result2 = ( (   (srcs, pure_srcs, pure_outs)
                    ,   (importers, exporters)
                    ,   (all_outs, all_msgs)
                    )
                  , ( m2outs, im_i_ex2outs)
                  , ( i2importers, i2exporters
                    , pure_i2exporters, all_interfaces
                    )
                  )
    return result2

    let result3 = ( "srcs" -: srcs //
                    "pure_srcs" -: pure_srcs //
                    "pure_outs" -: pure_outs //
                    "importers" -: importers //
                    "exporters" -: exporters //
                    "all_outs" -: all_outs //
                    "all_msgs" -: all_msgs //
                    "all_msgs_size" -: S.size all_msgs //
                    "msg2outs" -: m2outs //
                    "im_i_ex2outs" -: im_i_ex2outs //
                    "i2importers" -: i2importers //
                    "i2exporters" -: i2exporters //
                    "pure_i2exporters" -: pure_i2exporters //
                    "all_interfaces" -: all_interfaces ///
                    "all_interfaces_size" -: S.size all_interfaces
                    )
    -- print result3
    return result3
  where
    make_rules m2outs im_i_ex2outs = group_pairs $
        ones m2outs ++ pairs im_i_ex2outs
    ones m2outs = [(out, [m]) |
        (m, outs) <- M.toList m2outs, out <- S.toList outs]
    pairs im_i_ex2outs = [(out, [im, ex]) |
        ((im, _, ex), outs) <- M.toList im_i_ex2outs, out <- S.toList outs]
    merge_values :: Ord a => Map k (Set a) -> Set a
    merge_values = S.unions . M.elems


to_src_msgs :: Monad m =>
    MsgMiniL -> m (Set MConstructor)
to_src_msgs m = return srcs where
    srcs = S.fromList [get_msg_ctor msg |
                        SourceMsgsDecl msgs <- m, msg <- msgs]

to_m2outs :: Monad m =>
    MsgMiniL -> m (Map MConstructor (Set MConstructor))
to_m2outs m = return m2outs where
    m_out_ls = [(get_msg_ctor msg, get_msg_ctor out) |
                DirectOutput msg outs <- m, out <- outs]
    m2outs = group_pairs__set m_out_ls
to_pure_i2exporters :: Monad m =>
    MsgMiniL -> m (Map  IConstructor (Set MConstructor))
to_pure_i2exporters m = r where
    -- @return exs cannot be empty
    pis_decl =  [ (get_interface_ctor i, get_msg_ctors exs)
                | PureOutputInterfacesDecl stmts_wo <- m
                , ImportStmtWO i exs <- stmts_wo
                ]
    pure_i2exporters = merge_x_ys_pairs__set pis_decl
    i2null = M.filter S.null pure_i2exporters
    r = if M.null i2null
        then return pure_i2exporters
        else fail $ "logic error: pure interface with no exporters; "
                  ++ show (M.keysSet i2null)

to_im_i_ex2outs :: Monad m =>
    MsgMiniL -> m (Map  (MConstructor, IConstructor, MConstructor)
                        (Set MConstructor))
to_im_i_ex2outs m = return im_i_ex2outs where
    -- @return outs may be empty
    im_views = [(im, stmts) | ImportView im stmts <- m]
    qfs_ = flatten_import_view im_views
    fs_ = [((get_msg_ctor im, get_interface_ctor i, get_msg_ctor ex),
             get_msg_ctors outs) |
           (im, i, ex, outs) <- qfs_]
    im_i_ex2outs = merge_x_ys_pairs__set fs_


--[im, [i, [ex, [out]]]]
flatten_import_view :: [(Importer, [ImportStmt])] -> [(Importer, Interface, Exporter, [OutMsg])]
flatten_import_view = concat . fmap f where
    f (im, stmts) = [(im, i, ex, outs) |
                        ImportStmt i exstmts <- stmts,
                        ImplementedByStmt ex outs <- exstmts]



to_i2importers :: Monad m =>
    MsgMiniL -> m (Map IConstructor (Set MConstructor))
to_i2importers m = r where
    -- get i_decl.ims
    -- fitler out null
    -- merge i->ims
    -- get im_view
    -- flatten
    -- project to [(im, i)]
    -- swap and group i->ims
    -- i->compare ims
    i_ims_ls = [(get_interface_ctor i, get_msg_ctors ims) |
                InterfaceDecl ims i _ <- m, not $ null ims]
    decl_i2ims = merge_x_ys_pairs__set i_ims_ls
    im_views = [(im, stmts) | ImportView im stmts <- m]
    qfs_ = flatten_import_view im_views
    fs_ = [(get_msg_ctor im, get_interface_ctor i) |
           (im, i, _, _) <- qfs_]
    view_i2ims = group_pairs__set $ swap_pair_in_ls fs_
    -- i->decl_ims ==>> view_ims should exist and eq
    r = if M.isSubmapOf decl_i2ims view_i2ims
        then return view_i2ims
        else fail err_str
    err_str = "mismatch: interface -> importers\n" ++
              err1_str ++ err2_str
    err1_i2ims = M.difference decl_i2ims view_i2ims
    err2_i2may_i_ims_ims = M.intersectionWithKey
        (\i ims ims' -> if ims /= ims' then Just (i, ims, ims') 
                        else Nothing)
        decl_i2ims view_i2ims
    err2_i2err_i_ims_ims = M.mapMaybe id err2_i2may_i_ims_ims
    err1_str =  if M.null err1_i2ims then ""
                else "decl without imported:\n" ++
                     show err1_i2ims ++ "\n"
    err2_str =  if M.null err2_i2err_i_ims_ims then ""
                else "decl /= imported:\n" ++
                     show err2_i2err_i_ims_ims ++ "\n"


to_i2exporters :: Monad m =>
    MsgMiniL -> m (Map IConstructor (Set MConstructor))
to_i2exporters m = r where
    -- get i_decl.exs
    -- merge i->exs
    -- get im_view
    -- flatten
    -- merge (im, i)->exs
    -- ==>> i->(im,exs)
    -- i->compare exs
    i_exs_ls = [(get_interface_ctor i, get_msg_ctors exs) |
                InterfaceDecl _ i exs <- m]
    decl_i2exs = merge_x_ys_pairs__set i_exs_ls
    im_views = [(im, stmts) | ImportView im stmts <- m]
    qfs_ = flatten_import_view im_views
    fs_ = [((get_msg_ctor im, get_interface_ctor i), get_msg_ctor ex) |
           (im, i, ex, _) <- qfs_]
    fs2_ = M.toList $ group_pairs fs_ -- :: [((im, i), [ex])]
    fs = [(i, (im, S.fromList exs)) | ((im, i), exs) <- fs2_]
    i2im_exs_ls = group_pairs fs -- {i:[(im, exs)]}
    default_i2exs = M.map (snd . head) i2im_exs_ls
    i2exs = M.union decl_i2exs default_i2exs -- set default if no decl
    i2err_im_exs_ls = M.intersectionWith
            (\ls exs -> [(im, exs') | (im, exs') <- ls, exs' /= exs])
            i2im_exs_ls i2exs
    i2errs = M.filter (not . null) i2err_im_exs_ls
    i2i_exs_errs = M.intersectionWithKey (,,) i2exs i2errs
    i_exs_errs_ls = M.elems i2i_exs_errs -- [(i, {ex}, [(im, {ex})])]
    err_str = "mismatch: interface -> exporters\n" ++ show i_exs_errs_ls
    r = if null i_exs_errs_ls
        then return i2exs
        else fail err_str


get_msg_ctors :: Functor f => f Msg -> f MConstructor
get_interface_ctors :: Functor f => f Interface -> f IConstructor
get_msg_ctors = fmap get_msg_ctor
get_interface_ctors = fmap get_interface_ctor
get_msg_ctor (Msg t _) = t
get_interface_ctor (Interface t _) = t
{-
msg2ctor :: Msg -> MConstructor
msg2ctor (Msg ctor _) = ctor
interface2ctor :: Interface -> IConstructor
interface2ctor (Interface ctor _) = ctor
-}



---------------- parse
type MsgMiniL = [TopStmt]
data TopStmt    = DirectOutput Msg [OutMsg]
                | ImportView Importer [ImportStmt]
                | PureOutputInterfacesDecl [ImportStmtWO]
                | InterfaceDecl [Importer] Interface [Exporter]
                | SourceMsgsDecl [SrcMsg]
    deriving(Show, Read, Eq, Ord)

type SrcMsg = Msg
type OutMsg = Msg
type Exporter = Msg
type Importer = Msg
data ImportStmt = ImportStmt Interface [ImplementedByStmt]
    deriving(Show, Read, Eq, Ord)
data ImportStmtWO = ImportStmtWO Interface [Exporter]
    deriving(Show, Read, Eq, Ord)
data ImplementedByStmt = ImplementedByStmt Exporter [OutMsg]
    deriving(Show, Read, Eq, Ord)

data Msg = Msg MConstructor [Word]
    deriving(Show, Read, Eq, Ord)
data Interface = Interface IConstructor [Word]
    deriving(Show, Read, Eq, Ord)

type MConstructor = String
type IConstructor = String
type Word = String





---------------------------------------
any_keyword :: Stream s m Char => ParsecT s u m String
line_comment, mlines_comment, ignore_, ignores
    :: Stream s m Char => ParsecT s u m ()
wrap :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a

word, word_ :: Stream s m Char => ParsecT s u m String
word_prefix
    :: Stream s m Char => ParsecT s u m a -> ParsecT s u m String
word_notprefix
    :: (Stream s m Char, Show a) => ParsecT s u m a -> ParsecT s u m String



line_comment = skip $ keyword "--" >> skip_line
mlines_comment = skip $ keyword "{-" >>
    manyTill anyChar (try $ space >> keyword "-}")
ignore_ = skipMany1 space <|> line_comment <|> mlines_comment
ignores = skipMany ignore_ 


wrap a = discard_right a ignores


any_keyword = choice $ fmap keyword $ words "; -< <- --> >-"
word_ = wrap $ notwell_end >> notFollowedBy any_keyword >>
        -- not_ahead well_end >>
        -- notFollowedBy well_end >>
    manyTill anyChar (try well_end)
word_prefix prefix = lookAhead prefix >> word_
word_notprefix notprefix = notFollowedBy notprefix >> word_
---------------------------------------


-- -< <- --> ; >- -- {- -}
-- NOTE: "-<" and "-->" overlap

msg_mini_main :: Stream s m Char => ParsecT s u m MsgMiniL
top_stmt, direct_output, import_view
    , interface_decl, source_msgs_decl, pure_output_interfaces_decl
    :: Stream s m Char => ParsecT s u m TopStmt
import_stmt :: Stream s m Char => ParsecT s u m ImportStmt
import_stmt_wo :: Stream s m Char => ParsecT s u m ImportStmtWO
implementedby_stmt :: Stream s m Char => ParsecT s u m ImplementedByStmt
op_outmsg, op_exporter, op_importer, msg
    :: Stream s m Char => ParsecT s u m Msg
op_interface, interface
    :: Stream s m Char => ParsecT s u m Interface
m_constructor, i_constructor
    :: Stream s m Char => ParsecT s u m String


msg_mini_main = between ignores eof . many $
                    wrap (keyword ";") >> top_stmt
top_stmt    =   direct_output
            <|> interface_decl
            <|> source_msgs_decl
            <|> import_view
            <|> pure_output_interfaces_decl
            <?> "top_stmt"

source_msgs_decl = wrap (keyword "-->") >> many1 op_outmsg
                    >>= return . SourceMsgsDecl
pure_output_interfaces_decl = wrap (keyword "-<") >> many1 import_stmt_wo
                    >>= return . PureOutputInterfacesDecl
interface_decl = do
    i <- interface
    exs <- many1 op_exporter
    ims <- many0 op_importer
    return $ InterfaceDecl ims i exs
direct_output = do
    m <- try $ msg >>< lookAhead (keyword "-->") -- to avoid import_view
    outs <- many1 op_outmsg
    return $ DirectOutput m outs
import_view = do
    m <- try $ msg >>< lookAhead (keyword "-<") -- to avoid direct_output
    stmts <- many1 import_stmt
    return $ ImportView m stmts


import_stmt = do
    i <- op_interface
    stmts <- many1 implementedby_stmt
    return $ ImportStmt i stmts
import_stmt_wo = do
    i <- op_interface
    exs <- many1 op_exporter
    return $ ImportStmtWO i exs

implementedby_stmt = do
    m <- op_exporter
    outs <- many0 $ try op_outmsg -- add "try op..."??
    return $ ImplementedByStmt m outs





op_outmsg = wrap (keyword "-->") >> msg
op_exporter = wrap (keyword "<-") >> msg
op_importer = wrap (keyword ">-") >> msg
op_interface = wrap (keyword "-<") >> interface
msg = do
    mc <- m_constructor
    ws <- many0 word
    return $ Msg mc ws
interface = do
    ic <- i_constructor
    ws <- many0 word
    return $ Interface ic ws

m_constructor = word_prefix $ char 'M'
i_constructor = word_prefix $ char 'I'
word = word_notprefix $ oneOf "MI"




{-

MsgMini version1 final grammar

MsgMiniL : OpTopStmt*           -- MsgMiniL = MsgMiniL [TopStmt]
OpTopStmt : TOP_STMT_HEAD TopStmt
TopStmt
    : SourceMsgsDecl            -- TopStmt  = SourceMsgsDecl [SrcMsg]
    | InterfaceDecl             --          | InterfaceDecl [Importer] Interface [Exporter]
    | DirectOutput              --          | DirectOutput Msg [OutMsg]
    | ImportView                --          | ImportView Importer [ImportStmt]
    | PureOutputInterfacesDecl  --          | PureOutputInterfacesDecl [ImportStmtWO] -- WO -- without output stmt

SourceMsgsDecl : OUTPUT OpOutMsg+ -- two OUTPUT at beginning
InterfaceDecl : Interface OpExporter+ OpImporter*
DirectOutput : Msg OpOutMsg+
ImportView : Importer ImportStmt+
PureOutputInterfacesDecl : IMPORT ImportStmtWO+ -- two IMPORT at beginning
ImportStmt : OpInterface ImplementedByStmt+     -- ImportStmt = ImportStmt Interface [ImplementedByStmt]
ImportStmtWO : OpInterface OpExporter+          -- ImportStmtWO = ImportStmtWO Interface [Exporter]
ImplementedByStmt : OpExporter OpOutMsg*        -- ImplementedByStmt = ImplementedByStmt Exporter [OutMsg]


OpOutMsg : OUTPUT OutMsg
OpExporter : EXPORTED_BY Exporter
OpImporter : IMPORTED_BY Importer
OpInterface : IMPORT Interface

OutMsg : Msg
Importer : Msg
Exporter : Msg
Msg : MConstructor [Word]           -- Msg = Msg MConstructor [Word]
Interface : IConstructor [Word]     -- Interface = Interface IConstructor [Word]
                                    -- MConstructor = IConstructor = Word = String

-- belows should be well ended, i.e. followed by space or eof
MConstructor    -- r"M\S*"
IConstructor    -- r"I\S*"
Word            -- r"[^MI\s]\S*" and not keyword
OUTPUT          -- '-->'
IMPORT          -- '-<'
EXPORTED_BY     -- '<-'
IMPORTED_BY     -- '>-'
TOP_STMT_HEAD   -- ';' or no indent (i.e. top_stmt has no indent)
                --      , but what is a newline? what is a indent? '\r'?
                --      free-style is more easy since we agree what is a space.
# comment: "--" "{-" "-}"
# NOTE: "---", "{-}" and "{--" are not comment or comment begin;
# no embed comment


-------
    -----------
    verify MsgMiniL source (here msg means msg type):
        same interface type SHOULD be always followed same exporter set
            for any interface i:
                interface_decl i -> {exporter}
                for any import_view importer -> i -> {exporter}
                all these {exporter} should be same
            ==>> forall i: {importer-<i} * {i<-exporter} will be handled
        interface_decl's importers subclaus if presented:
            each importer in subclaus SHOULD import the interface type
            for any interface i:
                interface_decl i -> {importer}
                for importer in (i->){importer}:
                    import_view importer -> i
        normal interface is not pure interface
            i.e. normal one with importers, but pure without
        ?? |{i | importer -< i <- exporter}| <= 1
            no more than one interface connect two msgs??
        all msg types SHOULD be reachable by source msg types
            msg is reachable if
                at least one of the following conditions holds:
            1) msg is source msg
            2) reachable_msg1 --> msg
            3) reachable_msg1 -< i <- reachable_msg2 --> msg
            we can treat "... --> msg" as "msg ::= ..."
            and "source_msg ::= ;" ==>> all msgs are productive

-}
{-
MsgMini version1 related classes: -- ver2 try to merge process1/2
    class MsgBuffer msg a where
        push_msg :: msg -> a -> a
        pull_msg :: a -> Maybe (msg, a)
    class MsgBuffer msg a => MsgProcess i msg a where
        -- i - interface
        process1 :: msg -> a -> a -- implemented by step_process1
        process2 :: msg -> i -> msg -> a -> a -- by step_process2

        step_process1 :: MonadState a m => msg -> m ([i], [i], [msg])
        -- in_msg -> ({i_import}, {i_export}, [out_msg])
        -- the framework will set:
        --      i_export <- in_msg -< i_import
        -- and put [out_msg] into queue
        -- what about ?? Writer [msg] m ??
        --    but that will harden debug, I think.

        step_process2 :: MonadState a m => msg -> i -> msg -> m [msg]
        -- (importer_msg, i, exporter_msg) -> [out_msg]
        --      precondition: importer_msg -< i <- exporter_msg
        -- the framework will put [out_msg] into queue

-}
-- -}
-- -}
-- -}
-- -}

