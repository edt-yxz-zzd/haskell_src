{-# LANGUAGE FlexibleContexts
    , FlexibleInstances
    , TypeSynonymInstances
    , UndecidableInstances
    , OverlappingInstances
    , TypeFamilies
    #-}


-- see "parser design.txt"
-- common mistake: once word = r"\S+" => r"(?![MI])\S+" => not any keyword
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
    print r
    case r of 
        Left _ -> print ""
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
instance ToDoc ImplementedByStmt where
    to_doc (ImplementedByStmt msg msgs) = py_call2doc "ImplementedByStmt" (A2 msg msgs)
instance ToDoc ImportStmt where
    to_doc (ImportStmt i stmts) = py_call2doc "ImportStmt" (A2 i stmts)

instance ToDoc Msg where
    to_doc (Msg c ls) = py_call2doc "Msg" $ A2 c ls
instance ToDoc Interface where
    to_doc (Interface c ls) = py_call2doc "Interface" $ A2 c ls




-------------------
--
--


compile :: (Monad m, MConstructor ~ msg, IConstructor ~ i,
            Set MConstructor ~ msgs) =>
    MsgMiniL -> m ( msgs,
                    Map msg msgs,
                    Map (msg, i, msg) msgs,
                    Map i msgs,
                    Map i msgs)
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
    let b = M.size i2importers == M.size i2exporters &&
            M.keysSet i2importers == M.keysSet i2exporters &&
            M.keysSet im_i_ex2outs == (S.fromList . concat .
              M.elems $ M.intersectionWithKey
                (\i ims exs -> [(im, i, ex) |
                    im <- S.toList ims, ex <- S.toList exs])
                i2importers i2exporters)
    unless b $ fail "logic-error"
    return (srcs, m2outs, im_i_ex2outs, i2importers, i2exporters)
  where
    make_rules m2outs im_i_ex2outs = group_pairs $
        ones m2outs ++ pairs im_i_ex2outs
    ones m2outs = [(out, [m]) |
        (m, outs) <- M.toList m2outs, out <- S.toList outs]
    pairs im_i_ex2outs = [(out, [im, ex]) |
        ((im, _, ex), outs) <- M.toList im_i_ex2outs, out <- S.toList outs]


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
                | InterfaceDecl [Importer] Interface [Exporter]
                | SourceMsgsDecl [SrcMsg]
    deriving(Show, Read, Eq, Ord)

type SrcMsg = Msg
type OutMsg = Msg
type Exporter = Msg
type Importer = Msg
data ImportStmt = ImportStmt Interface [ImplementedByStmt]
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
top_stmt, direct_output, import_view, interface_decl, source_msgs_decl
    :: Stream s m Char => ParsecT s u m TopStmt
import_stmt :: Stream s m Char => ParsecT s u m ImportStmt
implementedby_stmt :: Stream s m Char => ParsecT s u m ImplementedByStmt
op_outmsg, op_exporter, op_importer, msg
    :: Stream s m Char => ParsecT s u m Msg
op_interface, interface
    :: Stream s m Char => ParsecT s u m Interface
m_constructor, i_constructor
    :: Stream s m Char => ParsecT s u m String


msg_mini_main = between ignores eof . many $
                    wrap (keyword ";") >> top_stmt
top_stmt    =   try direct_output
            <|> try interface_decl
            <|> try source_msgs_decl
            <|> import_view
            <?> "top_stmt"
direct_output = wrap $ do
    m <- msg
    outs <- many1 op_outmsg
    return $ DirectOutput m outs

source_msgs_decl = wrap (keyword "-->") >> many1 op_outmsg
                    >>= return . SourceMsgsDecl

interface_decl = do
    i <- interface
    exs <- many1 op_exporter
    ims <- many0 op_importer
    return $ InterfaceDecl ims i exs
import_view = do
    m <- msg
    stmts <- many1 import_stmt
    return $ ImportView m stmts


import_stmt = do
    i <- op_interface
    stmts <- many1 implementedby_stmt
    return $ ImportStmt i stmts

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





