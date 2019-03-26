
{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts #-}

import qualified Text.Parsec as P
import Text.Regex
-- import Data.List ((!!))

class TokenInter t ch n st | t->ch, t->st, t->n where
    -- ch - char type. i.e. Char
    -- st - value type. i.e. substring
    -- n - token name type.
    -- t - Token type. i.e. token
    getValue :: t->st
    getName :: t->n
    -- getPosBeg :: t-> Integer
    -- getPosEnd :: t-> Integer
    -- getPosEnd t = getPosBeg t + (fromIntegral . length . getSeq) t
    getSeq :: t-> [ch]
    -- getSrcPos, 
    getBeg, getEnd :: t -> P.SourcePos

data Token ch n st = Token  { name :: n
                            , value :: st
                            , begin, end :: P.SourcePos
                            , subseq :: [ch]
                            -- , srcpos :: P.SourcePos
                            } deriving(Show, Eq, Ord)

instance TokenInter (Token ch n st) ch n st where
    getValue = value
    getName = name
    getBeg = begin
    getEnd = end
    getSeq = subseq
    --getSrcPos = srcpos

token :: (TokenInter t ch n st, Eq n, Monad m, Show t) =>
            n -> P.ParsecT [t] u m st
token n = P.tokenPrim show nextPos testToken
    where nextPos _ t _ = getEnd t
          testToken t = if n == getName t
            then Just (getValue t)
            else Nothing

rsubregex :: (Monad m) => Int -> Regex -> String -> P.ParsecT String u m String
rsubregex idx r err = do
    s <- P.getInput
    case matchBeg r s of
        Just (self, _, subs) -> do
            P.count (length self) P.anyChar
            return (if idx /= -1 then subs !! idx else self) 
        _ -> fail . show $ err
rregex :: (Monad m) => Regex -> String -> P.ParsecT String u m String
rregex = rsubregex (-1)

type RegT u m = P.ParsecT String u m String
regex :: Monad m => String -> String -> RegT u m
regex = rregex . mkRegex
matchBeg r s = case matchRegexAll r s of
    Just (beg, self, tail, subs) -> if null beg
        then Just (self, tail, subs)
        else Nothing
    _ -> Nothing



data EdgeType = DefaultE | BackwordE | ForwordE | UndirectedE
data GraphGrammar = Node | Color | Weight | Newline | Indent | Dedent | Edge

-- type Monad m => RegT = P.ParsecT String u m String
spaces, word, newline, comment, edge :: Monad m => RegT u m
spaces = regex "((?![\\n\\r])\\s)+" "spaces"
word = regex "\\S+" "word"
newline = regex "\\n\\r|\\r\\n?|\\n" "newline"
comment = regex "# .*" "comment"
-- no (?...) syntax : edge = regex "[:<>-](?=\\S|$)" "edge"
tokenize = 5


indent_rex = mkRegex "(    )"
indent :: (Monad m) => P.ParsecT String u m String
indent = rregex indent_rex "NOT indent"
r = matchRegexAll (mkRegex "(    )") "    "
c4 :: P.Stream s m Char => P.ParsecT s u m [Char]
c4 = P.count 4 P.anyChar
c1 :: P.Stream s m Char => P.ParsecT s u m Char
c1 = P.anyChar
f = P.parse (P.many . P.try $ indent ) "<try>" "    "
g = P.parse indent "....>" "    s"

run r s = P.parse r "...>>>" s

t = run edge "<"

