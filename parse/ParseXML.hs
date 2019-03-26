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

module ParseXML
where
import Text.Parsec (Stream, ParsecT)
import qualified Text.Parsec as Pc
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Data.Char
import Control.Monad hiding (void)


void :: Monad m => m ()
void = return ()
pairM :: Monad m => m a -> m b -> m (a,b)
pairM ma mb = do
    a <- ma
    b <- mb
    return (a,b)
eitherM :: (Stream s m t, ParsecT s u m ~ p)
        => p a -> p b -> p (Either a b)
eitherM ma mb = try (ma >>= return . Left) <|> (mb >>= return . Right)


type T = Char
-- #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
-- NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
-- NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
-- PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
-- PubidChar_ ::= PubidChar - "'"
isXmlChar, isXmlNameStartChar, isXmlNameChar
    , isXmlPubidChar, isXmlPubidChar_
    :: Char -> Bool
isXmlChar = f . ord where
    f c | c < 0x20 = elem c [0x9, 0xA, 0xD]
        | c <= 0xD7FF = 0x20 <= c
        | c <= 0xFFFD = 0xE000 <= c
        | c <= 0x10FFFF = 0x10000 <= c
        | otherwise = error "logic error"
isXmlNameChar = undefined
isXmlNameStartChar = undefined
isXmlPubidChar = undefined
isXmlPubidChar_ = undefined

-- may begin with xmlS:
--      Misc VersionInfo DeclSep NDataDecl AttDef
--      EncodingDecl SDDecl



testResult :: Monad m => (a->Bool) -> (a->String) -> m a -> m a
testResult test err ma = do
    a <- ma
    unless (test a) $
        fail $ err a
    return a
anyChar, _anyChar, _Char0A, space, newline
    , xmlChar, xmlNameStartChar, xmlNameChar
    , xmlPubidChar, xmlPubidChar_
    --, xmlCharRef
    :: Stream s m T => ParsecT s u m T
spaces0, spaces1, xmlS
    :: Stream s m T => ParsecT s u m String
{-
xmlCharRef = hex <|> dec <?> "xmlCharRef" where -- hex before dec
    -- not "&#X"??
    hexS = between (string "&#x") (char ';') $ many1 Pc.hexDigit
    decS = between (string "&#") (char ';') $ many1 Pc.digit
    hex = hexS >>= f . ("0x"++)
    dec = decS >>= f
    f = return . int2char . read
int2char :: Integer -> Char
int2char i = if 0 <= i && i <= max_char then chr $ fromInteger i else
    error $ "convert int to char fail: " ++ show i where
    max_char = toInteger $ ord (maxBound::Char)
-}



-- handle '\x0D\x0A?' -> '\x0A' or not
xmlNameChar = testResult isXmlNameChar (errXmlChar "NameChar") Pc.anyChar
xmlNameStartChar = testResult isXmlNameStartChar (errXmlChar "NameStartChar") Pc.anyChar
xmlPubidChar = testResult isXmlPubidChar (errXmlChar "PubidChar") Pc.anyChar
xmlPubidChar_ = testResult isXmlPubidChar_ (errXmlChar "PubidChar_") Pc.anyChar
_anyChar = testResult isXmlChar (errXmlChar "char") Pc.anyChar
_Char0A = Pc.char '\x0A'
anyChar = do
    c <- _anyChar
    if c /= '\x0D' then return c else do
    _Char0A <|> return '\x0A'
errXmlChar :: String -> Char -> String
errXmlChar cls c = "not a valid XML " ++ cls ++ ": " ++ show c

char :: Stream s m T => Char -> ParsecT s u m T
string :: Stream s m T => String -> ParsecT s u m String
oneOf, noneOf :: AMT [Char] Char
newline = char '\x0A' <?> "newline"
char c = if c == '\x0D' then error "char '\\x0D'" else
         if isXmlChar c then Pc.char c else fail $ errXmlChar "char" c
string s = if all isXmlChar s then Pc.string s else
    fail $ errXmlChar "char" . head $ filter (not . isXmlChar) s
oneOf s = if elem '\x0D' s then error "oneOf \"\\x0D...\"" else
          if all isXmlChar s then Pc.oneOf s else
          fail $ errXmlChar "char" . head $ filter (not . isXmlChar) s
noneOf s = if elem '\x0D' s then error "noneOf \"\\x0D...\"" else
           if elem '\x0A' s then notNewline else withNewline where
    notNewline = notFollowedBy newline >> Pc.noneOf s
    withNewline = newline <|> Pc.noneOf s

-- #x20 | #x9 | #xD | #xA
space = Pc.oneOf "\x20\x09\x0D\x0A"
spaces0 = many space
spaces1 = many1 space
xmlS = spaces1
xmlChar = anyChar

newtype Name = Name String
data EntityRef = EntityRef Name
data PEReference = PEReference Name
newtype Reference = Reference (Either EntityRef CharRef)
newtype CharRef = CharRef Integer
--, xmlNames -- tokenized attribute values after normalization
xmlName :: Stream s m T => ParsecT s u m Name
xmlEntityRef :: Stream s m T => ParsecT s u m EntityRef
xmlPEReference :: Stream s m T => ParsecT s u m PEReference
xmlCharRef :: Stream s m T => ParsecT s u m CharRef
xmlReference :: Stream s m T => ParsecT s u m Reference
xmlName = do
    h <- xmlNameStartChar
    ts <- many xmlNameChar
    return . Name $ h:ts
xmlEntityRef = fmap EntityRef $ between (char '&') (char ';') xmlName
xmlPEReference = fmap PEReference $ between (char '%') (char ';') xmlName
xmlCharRef = hex <|> dec <?> "xmlCharRef" where -- hex before dec
    -- not "&#X"??
    hexS = between (string "&#x") (char ';') $ many1 Pc.hexDigit
    decS = between (string "&#") (char ';') $ many1 Pc.digit
    hex = hexS >>= f . ("0x"++)
    dec = decS >>= f
    f = return . CharRef . read
xmlReference = fmap f xmlEntityRef <|> fmap g xmlCharRef
    <?> "xmlReference" where
    f = Reference . Left
    g = Reference . Right






type MT t = Stream s m T => ParsecT s u m t
type AMT a t = Stream s m T => a -> ParsecT s u m t
type MTMT a t = Stream s m T => ParsecT s u m a -> ParsecT s u m t
type AMTMT b a t = Stream s m T => b -> ParsecT s u m a -> ParsecT s u m t
type AAMTMT c b a t = Stream s m T => c -> b -> ParsecT s u m a -> ParsecT s u m t

-- Literals
data EntityValue_Elem
    = EVE_S String
    | EVE_PER PEReference
    | EVE_R Reference
newtype EntityValue = EntityValue [EntityValue_Elem]
data AttValue_Elem = AVE_S String | AVE_R Reference
newtype AttValue = AttValue [AttValue_Elem]
xmlEntityValue_Elem :: AMT Char EntityValue_Elem
xmlEntityValue :: MT EntityValue
xmlAttValue_Elem :: AMT Char AttValue_Elem
xmlAttValue :: MT AttValue
xmlEntityValue_Elem quot = str <|> pe <|> ref <?> "xmlEntityValue_Elem"
    where
        str = fmap EVE_S $ many1 . noneOf $ quot:"%&"
        pe = fmap EVE_PER xmlPEReference
        ref = fmap EVE_R xmlReference
xmlAttValue_Elem quot = str <|> ref <?> "xmlAttValue_Elem"
    where
        str = fmap AVE_S $ many1 . noneOf $ quot:"<&"
        ref = fmap AVE_R xmlReference


quoted :: Stream s m T => Char -> ParsecT s u m a -> ParsecT s u m a
quoted2 :: Stream s m T => ParsecT s u m a -> ParsecT s u m a
quoted c = between (char c) (char c)
quoted2 a = quoted '"' a <|> quoted '\'' a
quotList :: Stream s m T => (Char -> ParsecT s u m a) -> ParsecT s u m [a]
quotList ch2ma = f '\'' <|> f '"' where
    f c = quoted c . many $ ch2ma c
quotList_ex
    :: Stream s m T
    => (Char -> ParsecT s u m a) -> ([a]->b) -> String -> ParsecT s u m b
quotList_ex ch2ma f name = fmap f (quotList ch2ma <?> name)
xmlEntityValue = quotList_ex xmlEntityValue_Elem EntityValue "xmlEntityValue"
xmlAttValue = quotList_ex xmlAttValue_Elem AttValue "xmlAttValue"



newtype SystemLiteral = SystemLiteral String
newtype PubidLiteral = PubidLiteral String
xmlSystemLiteral :: MT SystemLiteral
xmlPubidLiteral :: MT PubidLiteral
xmlSystemLiteral = quotList_ex f SystemLiteral "xmlSystemLiteral" where
    f quot = noneOf [quot]
xmlPubidLiteral = quotList_ex f PubidLiteral "xmlPubidLiteral" where
    f quot = if quot /= '\'' then xmlPubidChar else xmlPubidChar_



-- error:
--      what if mb fail?
(>><) :: Monad m => m a -> m b -> m a
ma >>< mb = do
    a <- ma
    mb
    return a
{-
-}

-- Text = [CharData | MakeUp]
newtype CharData = CharData String
newtype CData = CData String -- inside MakeUp ; contains no markups
newtype CDSect = CDSect String -- one MakeUp
newtype Comment = Comment String
newtype PITarget = PITarget Name -- except "(?i)Xml"
data PI = PI PITarget String
data Misc = Misc_PI PI | Misc_C Comment | Misc_S String
xmlCDStart, xmlCDEnd :: MT String
xmlCharData :: MT CharData
xmlCData :: MT CData
xmlCDSect :: MT CDSect
xmlComment :: MT Comment
xmlPITarget :: MT PITarget
xmlPI :: MT PI
xmlMisc :: MT Misc
xmlCDStart = string "<![CDATA["
xmlCDEnd = string "]]>"
xmlCharData = fmap CharData $ many char_in_CharData where
    char_in_CharData = notFollowedBy (string "]]>") >> noneOf "<&"
xmlCData = fmap CData $ many char_in_CData where
    char_in_CData = notFollowedBy (string "]]>") >> anyChar
xmlCDSect = between xmlCDStart xmlCDEnd xmlCData >>= \(CData s) ->
    return $ CDSect s
xmlComment = fmap Comment . between (string "<!--") (string "-->") $
    many char_in_Comment where
    char_in_Comment = notFollowedBy (string "--") >> anyChar
xmlPITarget = do
    Name id <- xmlName
    if map toLower id == "xml" then fail "PITarget should not be \"xml\""
    else return $ PITarget (Name id)
xmlPI = between (string "<?") (string "?>") $ do
        id <- xmlPITarget
        str <- (lookAhead space >> many char_in_PI) <|> return ""
        return $ PI id str
    where
        char_in_PI = notFollowedBy (string "?>") >> anyChar
xmlMisc = pi <|> comment <|> s <?> "xmlMisc" where
    pi = xmlPI >>= return . Misc_PI
    comment = xmlComment >>= return . Misc_C
    s = xmlS >>= return . Misc_S




-- prolog
data VersionNum = VersionNum Integer String
newtype VersionInfo = VersionInfo VersionNum
xmlVersionNum :: MT VersionNum
xmlVersionInfo :: MT VersionInfo
xmlVersionNum = try $ do
    string "1." >> many1 Pc.digit >>= return . VersionNum 1
xmlEq :: MT ()
xmlEq = try (spaces0 >> char '=' >> spaces0 >> void) <?> "xmlEq"
xmlVersionInfo = xmlS >> string "version" >> xmlEq >>
    quoted2 xmlVersionNum >>= return . VersionInfo















data ExternalID = ExternalID (Maybe PubidLiteral) SystemLiteral
newtype NDataDecl = NDataDecl Name -- Notation Decl
newtype PublicID = PublicID PubidLiteral
data NotationDecl = NotationDecl Name (Either ExternalID PublicID)
xmlExternalID :: MT ExternalID
xmlNDataDecl :: MT NDataDecl
xmlPublicID :: MT PublicID
xmlNotationDecl :: MT NotationDecl


xmlExternalID = pub <?> "xmlExternalID" where
    pub = do
        may_p <- (string "SYSTEM" >> return Nothing)
             <|> (string "PUBLIC" >> xmlS >> xmlPubidLiteral >>= return . Just)
        s <- xmlS >> xmlSystemLiteral
        return $ ExternalID may_p s
xmlNDataDecl = xmlS >> string "NDATA" >> xmlS >> xmlName >>=
    return . NDataDecl
xmlPublicID = string "PUBLIC" >> xmlS >> xmlPubidLiteral >>=
    return . PublicID
xmlNotationDecl = between (string "<!NOTATION") (spaces0 >> char '>') $ do
    name <- xmlS >> xmlName
    -- pub is a proper prefix of ext!
    id <- xmlS >> (try ext <|> pub <?> "ExternalID | PublicID")
    return $ NotationDecl name id where
    ext = xmlExternalID >>= return . Left
    pub = xmlPublicID >>= return . Right





list1 :: AMTMT Char a [a] -- ( a ? a ? ...)
list1 c ma = between (char '(' >> spaces0) (char ')') .
    sepBy1 (ma >>< spaces0) $ (char c >> spaces0)
listGtN :: AAMTMT Int Char a [a] -- ( a ? a ? ...)
listGtN n c ma = between (char '(' >> spaces0) (char ')') $ do
    let ma' = ma >>< (spaces0 >> char c >> spaces0)
    ls <- count n ma'
    ls2 <- sepBy1 (ma >>< spaces0) $ (char c >> spaces0)
    return $ ls ++ ls2
enumList :: MTMT a [a] -- ( a | a | ...)
enumList = list1 '|'
{-
enumList ma = between (char '(' >> spaces0) (spaces0 >> char ')') .
    sepBy1 ma $ try (spaces0 >> char '|' >> spaces0)
-}

newtype Nmtoken = Nmtoken String
newtype Enumeration = Enumeration [Nmtoken]
newtype NotationType = NotationType [Name]
newtype EnumeratedType = EnumeratedType (Either NotationType Enumeration)
xmlNmtoken :: MT Nmtoken
xmlEnumeration :: MT Enumeration
xmlNotationType :: MT NotationType
xmlEnumeratedType :: MT EnumeratedType
xmlNmtoken = fmap Nmtoken $ many1 xmlNameChar
xmlEnumeration = fmap Enumeration $ enumList xmlNmtoken
xmlNotationType = string "NOTATION" >> xmlS >> names where
    names = fmap NotationType $ enumList xmlName
xmlEnumeratedType = n <|> e <?> "xmlEnumeratedType" where
    n = xmlNotationType >>= f . Left
    e = xmlEnumeration >>= f . Right
    f = return . EnumeratedType


data TokenizedType
    = ID | IDREF | IDREFS
    | ENTITY | ENTITIES
    | NMTOKEN | NMTOKENS
    deriving (Read, Show)
data AttType = AT_StringType | AT_T TokenizedType | AT_E EnumeratedType

xmlStringType :: MT ()
xmlTokenizedType :: MT TokenizedType
xmlAttType :: MT AttType
xmlStringType = string "CDATA" >> void
xmlTokenizedType = fmap read . choice $ map string ws where
    ws = words "IDREFS IDREF ID ENTITIES ENTITY NMTOKENS NMTOKEN"
xmlAttType = s <|> t <|> e <?> "xmlAttType" where
    s = xmlStringType >> return AT_StringType
    t = xmlTokenizedType >>= return . AT_T
    e = xmlEnumeratedType >>= return . AT_E



data AttDef = AttDef Name AttType DefaultDecl
data AttlistDecl = AttlistDecl Name [AttDef]
_xmlAttDef :: MT AttDef
xmlAttlistDecl :: MT AttlistDecl
_xmlAttDef = do
    -- I donot like to have a xmlS at begin! will be removed
    -- name <- xmlS >> xmlName
    name <- xmlName
    att <- xmlS >> xmlAttType
    decl <- xmlS >> xmlDefaultDecl
    return $ AttDef name att decl
xmlAttlistDecl = between (string "<!ATTLIST") (spaces0 >> char '>') $ do
    name <- xmlS >> xmlName
    -- assume xmlAttDef not begin with S
    atts <- xmlS >> sepBy _xmlAttDef xmlS
    return $ AttlistDecl name atts






newtype PEDef = PEDef (Either EntityValue ExternalID)
data EntityDef  = EntityDef_V EntityValue
                | EntityDef_X ExternalID (Maybe NDataDecl)
    -- EntityDef (Either EntityValue (ExternalID, Maybe NDataDecl))
xmlPEDef :: MT PEDef
xmlEntityDef :: MT EntityDef
xmlPEDef = ev <|> eid <?> "xmlPEDef" where
    ev = xmlEntityValue >>= f . Left
    eid = xmlExternalID >>= f . Right
    f = return . PEDef
xmlEntityDef = ev <|> ex <?> "xmlEntityDef" where
    ev = xmlEntityValue >>= return . EntityDef_V
    ex = do
        x <- xmlExternalID
        m <- optionMaybe xmlNDataDecl
        return $ EntityDef_X x m

data EntityDecl = EntityDecl_G GEDecl | EntityDecl_P PEDecl
data GEDecl = GEDecl Name EntityDef
data PEDecl = PEDecl Name PEDef
_xmlGEDecl :: MT GEDecl
_xmlGEDecl = do
    name <- xmlName
    e <- xmlS >> xmlEntityDef
    return $ GEDecl name e
_xmlPEDecl :: MT PEDecl
_xmlPEDecl = do
    char '%' >> xmlS
    name <- xmlName
    p <- xmlS >> xmlPEDef
    return $ PEDecl name p
xmlEntityDecl :: MT EntityDecl
xmlEntityDecl = between (string "<!ENTITY") (spaces0 >> char '>') $ do
    g <|> p <?> "xmlEntityDecl" where
    g = _xmlGEDecl >>= return . EntityDecl_G
    p = _xmlPEDecl >>= return . EntityDecl_P







data Mixed = Mixed (Maybe [Name])
xmlMixed :: MT Mixed
xmlMixed = try (char '(' >> spaces0 >> string "#PCDATA" >> spaces0) >> do
    p0 <|> ps <?> "xmlMixed" where
    p0 = char ')' >> (return $ Mixed Nothing)
    ps = fmap (Mixed . Just) $
        many (char '|' >> spaces0 >> xmlName >>< spaces0)

data Multi = M01 | M0s | M1s | M11
data X_children_arg = X_children_c X_choice | X_children_s X_seq
data X_children = X_children X_children_arg Multi
data X_cp_arg = X_cp_n Name | X_cp_c X_choice | X_cp_s X_seq
data X_cp = X_cp X_cp_arg Multi
newtype X_choice = X_choice [X_cp] -- >= 2
newtype X_seq = X_seq [X_cp] -- >= 1
multi :: MT Multi
xmlX_children :: MT X_children
xmlX_cp :: MT X_cp
xmlX_choice :: MT X_choice
xmlX_seq :: MT X_seq
multi = (oneOf "?*+" >>= return . f) <|> return M11 where
    f '?' = M01
    f '*' = M0s
    f '+' = M1s
xmlX_children = c <|> s <?> "xmlX_children" where
    c = xmlX_choice >>= f . X_children_c
    s = xmlX_seq >>= f . X_children_s
    f cs = do
        m <- multi
        return $ X_children cs m
xmlX_cp = n <|> try c <|> s <?> "xmlX_cp" where
    n = xmlName >>= f . X_cp_n
    c = xmlX_choice >>= f . X_cp_c
    s = xmlX_seq >>= f . X_cp_s
    f ncs = do
        m <- multi
        return $ X_cp ncs m
xmlX_choice = listGtN 1 '|' xmlX_cp >>= return . X_choice
xmlX_seq = list1 ',' xmlX_cp >>= return . X_seq




data X_contentspec
    = EMPTY | ANY
    | X_contentspec_m Mixed | X_contentspec_cs X_children
data X_elementdecl = X_elementdecl Name X_contentspec
xmlX_contentspec :: MT X_contentspec
xmlX_elementdecl :: MT X_elementdecl
xmlX_contentspec = choice [e, a, m, cs] <?> "xmlX_contentspec" where
    e = string "EMPTY" >> return EMPTY
    a = string "ANY" >> return ANY
    m = xmlMixed >>= return . X_contentspec_m
    cs = xmlX_children >>= return . X_contentspec_cs
xmlX_elementdecl = between (string "<!ELEMENT") (spaces0 >> char '>') $ do
    name <- xmlS >> xmlName
    cs <- xmlS >> xmlX_contentspec
    return $ X_elementdecl name cs








-- DTD
newtype DeclSep = DeclSep (Maybe PEReference)
xmlDeclSep :: MT DeclSep
xmlDeclSep = s <|> pe <?> "xmlDeclSep" where
    s = xmlS >> return (DeclSep Nothing)
    pe = xmlPEReference >>= return . DeclSep . Just
data X_markupdecl
    = X_markupdecl_el X_elementdecl
    | X_markupdecl_ad AttlistDecl
    | X_markupdecl_ed EntityDecl
    | X_markupdecl_nd NotationDecl
    | X_markupdecl_pi PI
    | X_markupdecl_cm Comment
xmlX_markupdecl :: MT X_markupdecl
xmlX_markupdecl = choice [el, ad, ed, nd, pi, cm] <?> "xmlX_markupdecl"
  where
    el = xmlX_elementdecl >>= f . X_markupdecl_el
    ad = xmlAttlistDecl >>= f . X_markupdecl_ad
    ed = xmlEntityDecl >>= f . X_markupdecl_ed
    nd = xmlNotationDecl >>= f . X_markupdecl_nd
    pi = xmlPI >>= f . X_markupdecl_pi
    cm = xmlComment >>= f . X_markupdecl_cm
    f = return


newtype X_intSubset = X_intSubset [Either X_markupdecl DeclSep]
data X_doctypedecl
    = X_doctypedecl Name (Maybe ExternalID) (Maybe X_intSubset)
xmlX_intSubset :: MT X_intSubset
xmlX_doctypedecl :: MT X_doctypedecl
xmlX_intSubset = fmap X_intSubset . many $
    eitherM xmlX_markupdecl xmlDeclSep
xmlX_doctypedecl = between (string "<!DOCTYPE") (spaces0 >> char '>') $ do
    name <- xmlS >> xmlName >>< spaces0
    ext <- optionMaybe xmlExternalID >>< spaces0
    set <- optionMaybe $ between (char '[') (char ']') xmlX_intSubset
    return $ X_doctypedecl name ext set






-- prolog
newtype EncName = EncName String
newtype EncodingDecl = EncodingDecl EncName
newtype SDDecl = SDDecl Bool
data XMLDecl = XMLDecl VersionInfo (Maybe EncodingDecl) (Maybe SDDecl)
xmlEncName :: MT EncName
xmlEncodingDecl :: MT EncodingDecl
xmlSDDecl :: MT SDDecl
xmlXMLDecl :: MT XMLDecl
xmlEncName = undefined
-- [81]   EncName   ::=   [A-Za-z] ([A-Za-z0-9._] | '-')*
xmlEncodingDecl = xmlS >> string "encoding" >> xmlEq >>
    quoted2 xmlEncName >>= return . EncodingDecl
xmlSDDecl = xmlS >> string "standalone" >> xmlEq >>
    quoted2 yes_no >>= return . SDDecl where
    yes_no= (string "yes" >> return True)
        <|> (string "no" >> return False)
        <?> "yes or no"
xmlXMLDecl = between (string "<?xml") (spaces0 >> string "?>") $ do
    ver <- xmlVersionInfo
    enc <- optionMaybe xmlEncodingDecl
    sdd <- optionMaybe xmlSDDecl
    return $ XMLDecl ver enc sdd


data X_prolog
    = X_prolog (Maybe XMLDecl) [Misc] (Maybe (X_doctypedecl, [Misc]))
xmlX_prolog :: MT X_prolog
xmlX_prolog = do
    dec <- optionMaybe xmlXMLDecl
    ms <- many xmlMisc
    d_m <- optionMaybe pair
    return $ X_prolog dec ms d_m where
    pair = pairM xmlX_doctypedecl $ many xmlMisc

{-
    document ::= prolog element Misc*

-}
data DefaultDecl = REQUIRED | IMPLIED | FIXED AttValue
xmlDefaultDecl :: MT DefaultDecl
xmlDefaultDecl = r <|> i <|> f <?> "xmlDefaultDecl" where
    r = string "#REQUIRED" >> return REQUIRED
    i = string "#IMPLIED" >> return IMPLIED
    f = optional (string "#FIXED" >> xmlS) >> xmlAttValue >>=
        return . FIXED
