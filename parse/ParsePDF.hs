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


{-
    PDF Reference (6ed) Adobe Portable Document Format(v1.7)(2006)
        ::3.1 Lexical Conventions
        ::3.2 Objects
        ::3.4 File Structure
        ::G.5 Outline Hierarchy Example
        ::G.7 Structured Elements That Describe Hierarchical Lists
-}

-- import Text.Parsec.ByteString.Lazy
-- import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.Combinator
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word
-- import Control.Monad.State.Lazy as MS
import Data.Char (chr, ord)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Ratio ((%))



most :: Stream s m t => Integer -> ParsecT s u m a -> ParsecT s u m [a]
most n p = if n <= 0 then return [] else option [] $ do
    a <- p
    ls <- most (n-1) p
    return $ a:ls
newtype ByteS = ByteS { unByteS :: ByteString }
type S = ByteS -- Stream
type U = () -- userdata
type T = Word8 -- token
type Bytes = [T]
-- data M = M { pos :: Int, tk :: T } -- m
-- type M = MS.State Int


instance Monad m => Stream S m T where
    uncons = return . (fmap $ fmap ByteS) . BS.uncons . unByteS
a :: Stream S Maybe T => Int
a = 0


char2word8 :: Char -> T
char2word8 c = if n <= maxT
                then fromIntegral n
                else error $
                    "char2word8 overflow: ord "
                    ++ show c ++ "== " ++ show n
                    ++ "> " ++ show maxT
            where n = ord c
                  maxT = fromIntegral (maxBound :: T)
word8_to_char :: Integral i => i -> Char
word8_to_char = chr . fromIntegral
showByte :: T -> String
showByte = show . word8_to_char
read_byte :: Stream s m T => (T -> Maybe a) -> ParsecT s u m a
read_byte f = tokenPrim showByte nextPos f where
    -- testT t = if t == w8 then f t else Nothing
    nextPos pos t ts  = updatePosChar pos $ word8_to_char t
just_eq :: Eq a => a -> (a -> Maybe a)
just_eq a = \b -> if a == b then Just b else Nothing
byte :: Stream s m T => T -> ParsecT s u m T
byte t = read_byte (just_eq t) <?> show (word8_to_char t)
byte_if :: Stream s m T => String -> (T->Bool) -> ParsecT s u m T
byte_if s f = (<?> s) . read_byte $ \t->if f t then Just t else Nothing

showBytes :: [T] -> String
showBytes = show . BS.pack
bytes :: Stream s m T => [T] -> ParsecT s u m [T]
bytes = tokens showBytes nextPos where
    nextPos pos ts = updatePosString pos $ fmap word8_to_char ts
    -- isPrefixOf (unByteS bs) 


b_0, b_7, b_9, b_A, b_F, b_a, b_f :: T
(b_0, b_7, b_9, b_A, b_F, b_a, b_f) =
    (f '0', f '7', f '9', f 'A', f 'F', f 'a', f 'f')
    where f = fromIntegral . ord

-- regular, delimiter, and white-space
data CharClass = Regular | Delimiter | WhiteSpace
-- delimiter characters(, ), <, >, [, ], {, }, /, and %are special
delimiters :: Set T
delimiters = S.fromList $ fmap (fromIntegral . ord)
    "()<>[]{}/%"
{-
DECIMAL HEXADECIMAL OCTAL NAME
0 00 000 Null (NUL)
9 09 011 Tab (HT)
10 0A 012 Line feed (LF)
12 0C 014 Form feed (FF)
13 0D 015 Carriage return (CR)
32 20 040 Space (SP)
-}
data WhiteSpace
    = Null
    | Tab
    | LineFeed
    | FormFeed
    | CarriageReturn
    | Space
data Newline = LF | CR -- | CRLF
data EOL_Marker = Newline | CRLF -- end of line

nNull = 0
nTab = 9
nLineFeed = 10
nFormFeed = 12
nCarriageReturn = 13
nSpace = 32
word8_whitespace_dict :: Map T WhiteSpace
word8_whitespace_dict = M.fromList
    [ (nNull, Null)
    , (nTab, Tab)
    , (nLineFeed, LineFeed)
    , (nFormFeed, FormFeed)
    , (nCarriageReturn, CarriageReturn)
    , (nSpace, Space)
    ]


isRegular :: T -> Bool
isRegular t = not (isDelimiter t || isWhiteSpace t)
isDelimiter :: T -> Bool
isDelimiter t = S.member t delimiters
isWhiteSpace :: T -> Bool
isWhiteSpace t = M.member t word8_whitespace_dict
isNewline :: T -> Bool
isNewline t = t == nLineFeed || t == nCarriageReturn
byte2white_space :: T -> Maybe WhiteSpace
byte2white_space t = M.lookup t word8_whitespace_dict
{-
byte2white_space t
    | t == 0 = Just Null
    | t == 9 = Just Tab
    | t == 10 = Just LineFeed
    | t == 12 = Just FormFeed
    | t == 13 = Just CarriageReturn
    | t == 32 = Just Space
    | otherwise = Nothing
-}


skip :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
void :: Stream s m t => ParsecT s u m ()
void = return ()
skip = (>> void)
char :: Stream s m T => Char -> ParsecT s u m T
char c = byte (char2word8 c) <?> show c
string, keyword :: Stream s m T => String -> ParsecT s u m [T]
string s = bytes (map char2word8 s) <?> show s
keyword s = string s >>< well_end
cr :: Stream s m T => ParsecT s u m T
cr = byte nCarriageReturn <?> "CR"
lf :: Stream s m T => ParsecT s u m T
lf = byte nLineFeed <?> "LF"
crlf :: Stream s m T => ParsecT s u m [T]
crlf = bytes [nCarriageReturn, nLineFeed] <?> "CRLF"
eol :: Stream s m T => ParsecT s u m [T]
eol = fmap return lf <|> crlf <|> fmap return cr
    <?> "end-of-line(LF/CRLF/CR)"
isCommentChar :: T -> Bool
isCommentChar = not . isNewline
comment_char :: Stream s m T => ParsecT s u m T
comment_char = byte_if "comment_char (not newline)" isCommentChar
comment :: Stream s m T => ParsecT s u m [T]
comment = between (char '%') eol $ many comment_char
-- %PDF-n.m and %%EOF


well_end :: Stream s m T => ParsecT s u m ()
well_end = eof <|> notFollowedBy (byte_if "regular" isRegular)
    <?> "well_end"
(>><) :: Stream s m t
        => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m a
a >>< b = do
    r <- a
    b
    return r


type Number = Either Integer Rational
type Name = Bytes -- assume utf8
type Str = Bytes
type Array = [PdfObj]
type Dict = Map Name PdfObj
type StreamType = (Dict, Bytes)
type PInt = Integer
type UInt = Integer
type Ref = (PInt, UInt)
data PdfObj
    = BoolObj Bool
    | NumObj Number
    | StringObj Str
    | NameObj Name
    | ArrayObj Array
    | DictObj Dict -- d[k]==null <==> k not in d
    | StreamObj StreamType
    | NullObj
    | NullableRef Ref -- missing <==> null
pdf_obj :: Stream s m T => ParsecT s u m PdfObj
pdf_obj = choice[ try ref >>= return . NullableRef
                , try bool_obj >>= return . BoolObj
                , try number_obj >>= return . NumObj
                , try string_obj >>= return . StringObj
                , try name_obj >>= return . NameObj
                , try array_obj >>= return . ArrayObj
                , try dict_obj >>= return . DictObj
                , try stream_obj >>= return . StreamObj
                , try null_obj >> return NullObj
                ]
        <?> "pdf_obj"



ref_pair :: Stream s m T => ParsecT s u m (PInt, UInt)
ref :: Stream s m T => ParsecT s u m Ref
ref_pair = do
    p <- pint
    spaces1
    u <- uint >>< well_end
    return (p, u)
ref = do
    r <- ref_pair
    spaces1
    keyword "R"
    return r

null_obj :: Stream s m T => ParsecT s u m ()
null_obj =  ( keyword "null" >> return ()
            ) <?> "null_obj"
bool_obj :: Stream s m T => ParsecT s u m Bool
bool_obj = true <|> false <?> "bool_obj"
true :: Stream s m T => ParsecT s u m Bool
true = keyword "true" >> return True
false :: Stream s m T => ParsecT s u m Bool
false = keyword "false" >> return False

{-
data PrefixState -- current state and any good longer states
    = Bad_NoMore
    | Good_NoMore
    | Bad_Maybe
    | Good_Maybe
class PrefixStateCls t st | st -> t where
    initial :: st
    toPrefixState :: st -> PrefixState
    trans :: t -> st -> st
    -- trans _ xxx_NoMore === Bad_NoMore
    getLongestPrefix 

longest_bytes :: Stream s m T => st -> (T -> st -> st) -> ParsecT s u m [T]
-}
isDigit :: T -> Bool
isDigit t = b_0 <= t && t <= b_9
may_sign :: Stream s m T => ParsecT s u m Bool
may_sign = option False neg_sign <?> "sign"
neg_sign :: Stream s m T => ParsecT s u m Bool
neg_sign = (char '+' >> return False) <|> (char '-' >> return True)
    <?> "+/-"
digit :: Stream s m T => ParsecT s u m T
digit = byte_if "digit" isDigit
digits :: Stream s m T => ParsecT s u m [T]
digits = many digit


with_sign :: Stream s m T => ParsecT s u m a -> ParsecT s u m (Bool, a)
with_sign p = do
    neg <- may_sign
    a <- p
    return (neg, a)
signed :: (Stream s m T, Num a) => ParsecT s u m a -> ParsecT s u m a
signed p = do
    (neg, a) <- with_sign p
    return $ if neg then negate a else a
uint, pint :: Stream s m T => ParsecT s u m Integer
uint = many1 digit >>= return . read . map word8_to_char
pint = try $ do
    u <- uint
    if u == 0 then fail "pint: meet 0" else return u
int :: Stream s m T => ParsecT s u m Integer
int = signed uint >>< well_end

dot_ufrac :: Stream s m T => ParsecT s u m Rational
ufrac :: Stream s m T => ParsecT s u m Rational
frac :: Stream s m T => ParsecT s u m Rational
number_obj :: Stream s m T => ParsecT s u m Number
dot_ufrac = do
    char '.'
    ds <- many1 digit
    return $ mkFrac [] ds
ufrac = dot_ufrac
    <|> (do
        i <- uint
        frac <- option 0 dot_ufrac
        return $ fromIntegral i + frac
        )
    <?> "unsigned fraction"
frac = signed ufrac >>< well_end
mkFrac :: [T] -> [T] -> Rational
mkFrac uint frac = i % d where
    i = read $ '0' : map word8_to_char (uint ++ frac) :: Integer
    len = length frac
    d = read $ '1' : replicate len '0' :: Integer



number_obj = choice [int >>= return . Left, frac >>= return . Right]
    <?> "number_obj"


string_obj :: Stream s m T => ParsecT s u m Str
string_obj = literal_string
     <|> hex_string
     <?> "string_obj"
hex_string :: Stream s m T => ParsecT s u m Bytes
full_hex_val__pad0, two_hex_digit_val :: Stream s m T => ParsecT s u m T
hex_pair__pad0 :: Stream s m T => ParsecT s u m (T,T)
hex_digit :: Stream s m T => ParsecT s u m T
hex_digit_val :: Stream s m T => ParsecT s u m T
hex_digit2val :: T -> T
hex_digit2val h
    | isDigit h = h - b_0
    | h <= b_F = h - b_A + 10
    | otherwise = h - b_a + 10
hex_digit_val = hex_digit >>= return . hex_digit2val
two_hex_digit2val :: T -> T -> T
two_hex_digit2val h1 h2 = h1*16 + h2
two_hex_digit_val = do
    h1 <- hex_digit_val
    h2 <- hex_digit_val
    return $ two_hex_digit2val h1 h2

in_rng :: Ord b => (a->b) -> (a,a) -> b -> Bool
in_rng f (s,t) b = f s <= b && b <= f t
isHexDigit :: T -> Bool
isHexDigit t = isDigit t || f (b_A, b_F) || f (b_a, b_f) where
    f rng = in_rng id rng t
hex_digit = byte_if "hex_digit" isHexDigit
hex_string = between (char '<') (char '>') $ many full_hex_val__pad0
hex_pair__pad0 = do
    h1 <- hex_digit
    h2 <- option b_0 hex_digit
    return (h1, h2)
full_hex_val__pad0 = hex_pair__pad0 >>= return . uncurry two_hex_digit2val


literal_string :: Stream s m T => ParsecT s u m Bytes
literal_char :: Stream s m T => ParsecT s u m Bytes
escape_char, escape_eol, no_escape :: Stream s m T => ParsecT s u m Bytes
std_literal_eol :: Stream s m T => ParsecT s u m Bytes
escape_char1 :: Stream s m T => ParsecT s u m T
escape_oct :: Stream s m T => ParsecT s u m T
literal_string = between (char '(') (char ')') $
    fmap concat $ many literal_char
literal_char = escape_char <|> literal_string <|> std_literal_eol
    <|> fmap return anyToken
    <?> "literal_char"
std_literal_eol = eol >> return [nLineFeed]
escape_char1 = try $ do
    b <- anyToken
    let c = word8_to_char b
    maybe (fail "escape_char1 fail") return $ M.lookup c escape_char2byte
isOctDigit :: T -> Bool
isOctDigit t = b_0 <= t && t <= b_7
oct_digit :: Stream s m T => ParsecT s u m T
oct_digit = byte_if "oct_digit" isOctDigit
oct_digits2uint :: [Integer] -> Integer
oct_digits2uint ls = f 0 ls where
    f i (o:ls) = f (i*8 + o) ls
    f i [] = i
int2word8 :: Integer -> T
int2word8 i = if 0 <= i && i <= maxT then fromIntegral i else
    error $ "int2word8 " ++ show i where
    maxT = fromIntegral (maxBound :: T)
escape_oct = lookAhead oct_digit >> most 3 oct_digit >>=
    return . int2word8 . oct_digits2uint . map (toInteger . \i -> i - b_0)
escape_eol = eol >> return []
no_escape = return []
{-
SEQUENCE MEANING
\n Line feed (LF)
\r Carriage return (CR)
\t Horizontal tab (HT)
\b Backspace (BS)
\f Form feed (FF)
\( Left parenthesis
\) Right parenthesis
\\ Backslash
\d{1,3} Character code ddd(octal)
\<eol> ignore \<eol>
\? - ignore \ but ? remain
<eol> -> '\n'

-}
escape_char2byte :: Map Char T
escape_char2byte = M.map char2word8 escape_char2char
escape_char2char = M.fromList
    [ ('n', '\n')
    , ('r', '\r')
    , ('t', '\t')
    , ('b', '\b')
    , ('f', '\f')
    , ('(', '(')
    , (')', ')')
    , ('\\', '\\')
    ]
escape_char = do
    char '\\'
    fmap return escape_char1
        <|> fmap return escape_oct
        <|> escape_eol
        <|> no_escape



name_obj :: Stream s m T => ParsecT s u m Name
name_char, escape_name_char :: Stream s m T => ParsecT s u m T
regular_char :: Stream s m T => ParsecT s u m T
name_obj = do
    char '/'
    many name_char -- maybe []
name_char = try escape_name_char <|> regular_char <?> "name_char"
regular_char = byte_if "regular_char" isRegular
escape_name_char = do
    char '#'
    two_hex_digit_val






space :: Stream s m T => ParsecT s u m T
spaceEx :: Stream s m T => ParsecT s u m [T]
space = byte_if "white space" isWhiteSpace
spaceEx = comment <|> fmap return space <?> "white space"
spaces, spaces1 :: Stream s m T => ParsecT s u m [T]
spaces = fmap concat $ many spaceEx
spaces1 = fmap concat $ many1 spaceEx
array_obj :: Stream s m T => ParsecT s u m Array
array_obj = between (char '[' >> spaces) (spaces >> char ']') $
    sepBy pdf_obj spaces1



dict_obj :: Stream s m T => ParsecT s u m Dict
dict_obj = between (string "<<" >> spaces) (spaces >> string ">>") $
    fmap f $ sepBy dict_entry spaces1 where
    f = M.fromList . filter (not . ignored_entry)

type DictEntry = (Name, PdfObj)
dict_entry :: Stream s m T => ParsecT s u m DictEntry
dict_entry = do
    key <- name_obj
    value <- pdf_obj
    return (key, value)
ignored_entry :: DictEntry -> Bool
ignored_entry (_, NullObj) = True
ignored_entry _ = False




stream_obj :: Stream s m T => ParsecT s u m StreamType
stream_obj = do
    d <- dict_obj
    spaces
    keyword "stream"
    eol
    let NumObj (Left len) = d M.! map char2word8 "/Length"
    s <- count (fromIntegral len) anyToken
    option [] eol
    keyword "endstream"
    return (d, s)









type IndirectObj = (Ref, PdfObj)
indirect_obj :: Stream s m T => ParsecT s u m IndirectObj
indirect_obj = do
    r <- ref_pair
    spaces1
    keyword "obj"
    spaces
    o <- pdf_obj
    spaces
    keyword "endobj"
    return (r, o)


type Header = (UInt, UInt)
type XrefTable = [XrefSection]
type XrefSection = [XrefSubsection]
data XrefSubsection = XS{ obj_num_begin :: UInt
                        , num_objs :: UInt
                        , xref_entries :: [XrefEntry]
                        }
data XrefEntry = XE { address :: UInt
                    -- space
                    , gen_num :: UInt
                    -- space
                    , using :: Bool
                    -- len(spaces + eol) == 2
                    }
data Trailer = Trailer  { trailer_dict :: Dict
                        , xref_start_addr :: UInt
                        -- %%EOF
                        }
data PDF = PDF  { head_info :: Header
                , objs_info :: [IndirectObj]
                , xref_info :: XrefTable
                , trailer_info :: Trailer
                }
header :: Stream s m T => ParsecT s u m Header
body :: Stream s m T => ParsecT s u m [IndirectObj]
xref_table :: Stream s m T => ParsecT s u m XrefTable
xref_section :: Stream s m T => ParsecT s u m XrefSection
xref_subsection :: Stream s m T => ParsecT s u m XrefSubsection
xref_using_mark :: Stream s m T => ParsecT s u m Bool
xref_entry :: Stream s m T => ParsecT s u m XrefEntry
trailer :: Stream s m T => ParsecT s u m Trailer
pdf_file :: Stream s m T => ParsecT s u m PDF

header = do
    string "%PDF-"
    v1 <- uint
    char '.'
    v2 <- uint
    eol
    return (v1, v2)
body = sepBy indirect_obj spaces
xref_table = many xref_section
xref_section = keyword "xref" >> eol >> many xref_subsection
xref_subsection = do
    beg_num <- uint
    char ' '
    size <- uint
    eol
    ls <- many xref_entry
    return XS   { obj_num_begin = beg_num
                , num_objs = size
                , xref_entries = ls
                }

xref_using_mark = choice
    [ keyword "n" >> return True
    , keyword "f" >> return False
    ]
xref_entry = do
    addr <- n_bytes2uint 10
    char ' '
    g <- n_bytes2uint 5
    char ' '
    using <- xref_using_mark
    skip crlf <|> try (skip $ char ' ' >> (lf <|> cr)) <?> "2 bytes eol"
    return XE { address = addr, gen_num = g, using = using}
    where
      n_bytes2uint n = count n digit >>= return . read . map word8_to_char

trailer = do
    keyword "trailer"
    spaces
    d <- dict_obj
    spaces
    keyword "startxref"
    eol
    start <- uint
    string "%%EOF"
    eof
    return Trailer {trailer_dict = d, xref_start_addr = start}

pdf_file = do
    h <- header
    spaces
    indirect_objs <- body
    spaces
    xref <- xref_table
    spaces
    tr <- trailer
    return PDF  { head_info = h
                , objs_info = indirect_objs
                , xref_info = xref
                , trailer_info = tr
                }





--}
--}
--}
--}
