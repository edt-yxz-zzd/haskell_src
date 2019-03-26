{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.Parser
where

import Data.List.NonEmpty
import Control.Arrow
import Seed.ArrowOps
import Data.Natural

{-
type Result a = Either (Error a) (NonEmpty (State a))
class CFGParser a where
    type State a
    type Token a
    type Error a
    feed :: a -> State a -> Token a -> Result a
    unique :: a -> [Result a] -> Result a
    finish :: a -> State a -> Bool
    shortest, longest :: a -> [Token a] -> Result a
    (<|>), (<&>) :: a -> a -> a
    <?> :: a -> String -> a
    unexpected :: String -> a -> a
    (>>>) :: a -> a -> a
-}


list1x2list :: (a, [a]) -> [a]
list1x2list (h, ts) = h:ts

class ArrowPlus arr => CFGParser arr where
    -- (>>>), (<+>)
    type Token arr
    parse :: arr () o -> [Token arr] -> [o]


{-
    any :: arr i (Token arr)
    eof :: arr i ()
    eof = any >>> zeroArrow <+>
--}
--}
--}
--}
