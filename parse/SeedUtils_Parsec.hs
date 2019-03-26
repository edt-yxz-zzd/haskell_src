{-# LANGUAGE FlexibleContexts
    , FlexibleInstances
    , TypeSynonymInstances
    , UndecidableInstances
    , OverlappingInstances
    , TypeFamilies
    #-}

module SeedUtils_Parsec
    ( module SeedUtils_Parsec
    , (>><), (>>=<)
    )
where

import SeedUtils ((>><), (>>=<))
import Text.Parsec hiding (parse) --, many, many1)
import Control.Monad


-- test : P.parse word "<xxx>" "aaaaa"
-- bug: why "notFollowedBy well_end" not work??
--      both succeed on " "
--      "notFollowedBy eof" - bug!

many0 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
many0 = many


-- see SeedUtils ">><" and ">>=<"
discard_right a b = do
    r <- a
    _ <- b
    return r
skip :: Monad m => m a -> m ()
skip = (>> return ())
-- notFollowedBy fail, how can I write my own version??
-- not_ahead :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
-- not_ahead p = (p >> unexpected "??") <|> return ()

space_or_eof, spaces1_or_eof, eol, skip_line,
    well_end, not_eof, not_space, notwell_end
    :: Stream s m Char => ParsecT s u m ()
keyword :: Stream s m Char => String -> ParsecT s u m String


---------------


space_or_eof = skip space <|> eof
                <?> "space or eof (i.e. notFollowedBy nonspace)"
spaces1_or_eof = skipMany1 space <|> eof
                <?> "space or eof (i.e. notFollowedBy nonspace)"
eol = skip newline <|> eof <?> "end of line"
skip_line = skip . manyTill anyChar $ try eol
well_end = lookAhead space_or_eof
not_eof = skip $ lookAhead anyChar
not_space = notFollowedBy space
notwell_end = not_eof >> not_space -- why ERROR: notFollowedBy well_end
keyword str = try $ discard_right (string str) well_end  -- donot wrap it



{-  ------------- example ---------
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
    -- no <?> "ignore"
ignores = skipMany $ ignore_ -- without "try"! try ignore_


wrap a = discard_right a ignores


any_keyword = choice $ fmap keyword $ words "; -< <- --> >-"
word_ = wrap $ notwell_end >> notFollowedBy any_keyword >>
        -- not_ahead well_end >>
        -- notFollowedBy well_end >>
    manyTill anyChar (try well_end)
word_prefix prefix = lookAhead prefix >> word_
word_notprefix notprefix = notFollowedBy notprefix >> word_
---------------------------------------

-}














----------------------  literal
string_literal, string_mid
    , unsigned_dec_int, unsigned_dec_zero, unsigned_dec_positive_int
    , unsigned_hex, unsigned_oct, unsigned_int_literal
    :: Stream s m Char => ParsecT s u m String
unsigned_dec_zero = keyword "0" >>< well_end
unsigned_dec_positive_int = notFollowedBy (char '0')
                            >> many1 digit >>< well_end

unsigned_dec_int = unsigned_dec_zero
                    <|> unsigned_dec_positive_int
                    <?> "unsigned decimal nonnegative integer"
unsigned_hex = liftM concat $ sequence
        [keyword "0x"<|>keyword "0X", many1 hexDigit]
unsigned_oct = liftM concat $ sequence
        [keyword "0o"<|>keyword "0O", many1 octDigit]
unsigned_int_literal = unsigned_dec_int
                    <|> unsigned_hex
                    <|> unsigned_oct
                    <?> "unsigned_int_literal"
string_literal = do
    ss <- between (char '\"') (char '\"') $ many string_mid
    let s = concat ss
    return $ "\"" ++ s ++ "\""
string_mid  =   sequence [noneOf "\\\"\n\r"]
            <|> sequence [char '\\', noneOf "\n\r"]
            <?> "string should not contain "
                ++ "eof or eol (i.e. \'\\n\' or \'\\r\')"






