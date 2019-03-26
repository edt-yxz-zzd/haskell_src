
{-

tree like filesystem
    both nodes contain value
    nonleaf (e.g. folder) may have no children
-}

module Tree
    ( Tree (..)
    , Forest
    , node_value
    , forsest_rule2forest
    , rpn2forest
    )
where

import SeedUtils (isLeft)
import Data.List (foldl')
import ToDoc

type Symbol = Either
type Rule n t = (n, [Symbol n t])
data Tree n t = Leaf t | Branch n [Tree n t]
    deriving (Show, Read, Eq, Ord)
type Forest n t = [Tree n t]
node_value :: Tree n t -> Either n t
node_value (Leaf t) = Right t
node_value (Branch n _) = Left n
instance (ToDoc n, ToDoc t) => ToDoc (Tree n t) where
    to_doc (Leaf t) = py_call2doc "Leaf" $ A1 t
    to_doc (Branch n cs) = py_call2doc "Branch" $ A2 n cs


{-
rsingleton [a] = a
rsingleton _ = error "not singleton"
count f = foldl' (\i a -> if f a then i+1 else i) 0
-}
forsest_rule2forest :: (Eq n, Eq t)
    => Forest n t -> Rule n t -> Forest n t
forsest_rule2forest trees (n, syms) = r where
    -- bug: len = length syms -- not all syms; only nonterminals
    -- bug: len = count (not . isLeft) syms -- Right is terminal!!
    nonterminals = filter isLeft syms
    len = length nonterminals
    (branches, tail) = splitAt len trees
    children = f branches syms where
        f cs (Right t : ss) = Leaf t : f cs ss
        f (b:cs) (Left _ : ss) = b : f cs ss
        f [] [] = []
        f _ _ = error "logic error: should be avoided by below error stmts"
    r = if length branches < len
        then error $ "len branches < len syms.nonterminals"
        else if fmap node_value branches /= nonterminals
        then error "branches.values /= syms.nonterminals"
        else Branch n children : tail
rpn2forest ::  (Eq n, Eq t) => [Rule n t] -> Forest n t
rpn2forest = foldl' forsest_rule2forest []




