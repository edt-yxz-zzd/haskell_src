{-# LANGUAGE FlexibleContexts
    , FlexibleInstances
    , TypeSynonymInstances
    , UndecidableInstances
    , OverlappingInstances
    #-}


-- see "parser design.txt"

import ToDoc
import Text.PrettyPrint hiding (char) -- for toPyStr
import Text.Parsec hiding (parse)
import Text.Parsec.String
import System.Environment

main = do
    fnames <- getArgs
    show_files fnames

show_files [] = return ()
show_files (fname:fs) = do
    print fname
    r <- parse fname
    print r
    case r of 
        Left _ -> print ""
        Right ls -> print $ to_doc ls
    show_files fs

-- parse :: String -> Main
parse = parseFromFile msg_mini_main




{-
 - use Python to comile and verify instead of Haskell
 -
---------------- compile
-- c - compiled
data MainC = MainC  { sources :: MsgTypes -- [] ==>> not present
                    , i2decl_importers :: Map InterfaceType MsgTypes
                    , i2importers :: Map InterfaceType MsgTypes -- growing
                    , i2exporters :: Map InterfaceType MsgTypes
                    , one2outs :: Map MsgType MsgTypes
                    , two2outs :: Map (MsgType, MsgType) MsgTypes
                    }
type MsgType = Type
type InterfaceType = Type
type Type = Atom
type MsgTypes = Set MsgType

data StateC =   { result :: MainC
                , input :: Main
                , other :: ()
                }
mkStateC :: Main -> StateC
mkStateC m = StateC { result = MainC{ sources = S.empty
                                    , i2decl_importers = M.empty
                                    , i2importers = M.empty -- step by step
                                    , i2exporters = M.empty
                                    , one2outs = M.empty
                                    , two2outs = M.empty}
                    , input = m
                    , other = ()}
compile :: Main -> Either String MainC
compile m = MS.runState handle_top_stmts $ mkStateC m
handle_top_stmts :: MonadState StateC m => Main -> m ()
handle_top_stmts m = do
    case m of
        [] -> get
        stmt:ls -> do
            handle_top_stmt stmt
            handle_top_stmts ls
handle_top_stmt stmt = do
    case stmt of
        SourceMsgsDecl [] -> get -- impossible
        SourceMsgsDecl msgs -> do
            s <- get
            srcs = sources $ result s
            if not $ S.null srcs 
            then fail "duplicate SourceMsgsDecl"
            else let srcs' = S.fromList srcs
                     r' = result s {sources = src'}
                     s' = s {result = r'}
                     put s'
        InterfaceDecl ims i exs -> do
            s <- get
            let r = result s
                i2exs = i2exporters r
                i2ims = i2importers r
                i2exs' = S.fromList exs
                i2ims' = S.fromList ims
            in  if i2exs /= i2exs' && not (M.null i2exs)
                then fail "exporters mismatch @InterfaceDecl" ++ show i
                else if not M.null i2ims 
-}




{-
main2doc :: Main -> Doc
main2doc ls = fsep $ fmap topstmt2doc ls
topstmt2doc (Output1 msg msgs) = text "Output1" <> parens (msg2doc)
-}

instance ToDoc TopStmt where
    to_doc (Output1 msg msgs) = py_call2doc "Output1" (A2 msg msgs)
    to_doc (ImportView msg stmts) = py_call2doc "ImportView" (A2 msg stmts)
    to_doc (InterfaceDecl ims i exs) =
        py_call2doc "InterfaceDecl" (A3 ims i exs)
    to_doc (SourceMsgsDecl msgs) = py_call2doc "SourceMsgsDecl" $ A1 msgs
instance ToDoc ImplementStmt where
    to_doc (ImplementStmt msg msgs) = py_call2doc "ImplementStmt" (A2 msg msgs)
instance ToDoc ImportStmt where
    to_doc (ImportStmt i stmts) = py_call2doc "ImportStmt" (A2 i stmts)

instance ToDoc Atom where
    to_doc (Word s) = py_call2doc "Word" $ A1 s
    to_doc (GroupR e) = py_call2doc "GroupR" $ A1 e
    to_doc (GroupB e) = py_call2doc "GroupB" $ A1 e




---------------- parse
type Main = [TopStmt]
data TopStmt    = Output1 Msg [OutMsg]
                | ImportView Importer [ImportStmt]
                | InterfaceDecl [Importer] Interface [Exporter]
                | SourceMsgsDecl [Msg]
    deriving(Show, Read, Eq, Ord)


type OutMsg = Msg
type Exporter = Msg
type Importer = Msg
data ImplementStmt = ImplementStmt Exporter [OutMsg]
    deriving(Show, Read, Eq, Ord)
data ImportStmt = ImportStmt Interface [ImplementStmt]
    deriving(Show, Read, Eq, Ord)

type Msg = Expr
type Interface = Expr
type Expr = [Atom]
data Atom = Word String | GroupR Expr | GroupB Expr
    deriving(Show, Read, Eq, Ord)

keyword :: Stream s m Char => String -> ParsecT s u m String
msg_mini_main :: Stream s m Char => ParsecT s u m Main
top_stmt, output1, import_view, interface_decl, source_msgs_decl
    :: Stream s m Char => ParsecT s u m TopStmt
out_msgs, msgs1, export_msgs1, import_msgs1, source_msgs1
    :: Stream s m Char => ParsecT s u m [Msg]
expr, msg, interface :: Stream s m Char => ParsecT s u m Expr
atom, word, groupR, groupB :: Stream s m Char => ParsecT s u m Atom
import_stmt :: Stream s m Char => ParsecT s u m ImportStmt
implement_stmt :: Stream s m Char => ParsecT s u m ImplementStmt


-- -< <- --> ; , ( ) [ ] >-
-- NOTE: "-<" and "-->" overlap
keyword str = between spaces spaces $ string str

msg_mini_main = many $ keyword ";" >> top_stmt
top_stmt    =   try output1
            <|> try interface_decl
            <|> source_msgs_decl
            <|> import_view
            <?> "top_stmt"
output1 = do
    m <- msg
    outs <- out_msgs
    return $ Output1 m outs
source_msgs1 = between (keyword "-->") (keyword "-->") msgs1
source_msgs_decl = source_msgs1 >>= return . SourceMsgsDecl

interface_decl = do
    i <- interface
    string "<-"
    exs <- export_msgs1
    ims <- option [] (string ">-" >> import_msgs1)
    return $ InterfaceDecl ims i exs
import_view = do
    m <- msg
    stmts <- many1 import_stmt
    return $ ImportView m stmts

import_stmt = do
    string "-<"
    i <- interface
    stmts <- many1 implement_stmt
    return $ ImportStmt i stmts

implement_stmt = do
    string "<-"
    m <- msg
    -- lookAhead $ string "-->" -- string not consume when fail??
    outs <- option [] $ try out_msgs
    return $ ImplementStmt m outs

out_msgs = string "-->" >> msgs1
msgs1 = sepBy1 msg $ string ","
import_msgs1 = msgs1
export_msgs1 = msgs1
msg = expr
interface = expr





expr = many1 $ between spaces spaces atom
atom = word <|> groupR <|> groupB <?> "atom"
word = many1 alphaNum >>= return . Word
groupR = between (char '(') (char ')') expr >>= return . GroupR
groupB = between (char '[') (char ']') expr >>= return . GroupB





