{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, 
             RankNTypes, MultiParamTypeClasses,
             ImpredicativeTypes, IncoherentInstances #-}
{- -XFlexibleInstances -}

-- import Data.Monad

import Monoid2Monad


class Optional a b where
    toMaybe :: b -> Maybe a -- required
    -- toList s = [s]

data None = None
instance Optional a None where
    toMaybe _ = Nothing

instance Optional a a where
    toMaybe s = Just s


-- toList :: Optional a x => x -> [a]
-- toList x = foldr [] $ toMaybe x

data Tree a = Leaf a | Node a [Tree a]
    deriving (Show)
instance Functor Tree where
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node a ls) = Node (f a) $ (fmap $ fmap f) ls
class Monad m => MTree m where
    node :: a -> m [Tree a] -> m (Tree a)
    node x = (return . Node x =<<)
    -- node x mt = mt >>= return . Node x
    leaf :: a -> m (Tree a)
    leaf = return . Leaf

{-
type MT = ToMonad [String] -- a MTree
block :: Monad m => name -> m [Tree name] -> m (Tree name)
block = node

build :: String -> String -> MT [Tree String] -> MT [Tree String]
build ctor rule subs = unTMState . fmap (const [name]) . TMState $ block name subs
-}


type RuleName name= Maybe name
type RefName name = [RuleName name]
type SubName name = RuleName name
-- type CtorName name = name
data GrammarRule ctor n cfg 
    = Config cfg -- Token name -- | Rule namex [namex]
    | Rule ctor (RuleName n) -- [SubName n]
    | Ref (RefName n)
    deriving (Show)
    -- where namex = Maybe name

type M = Maybe
instance MTree M -- why needs it??
type GMT a = M (Tree a)
type GMTL a = M [Tree a]
type MT ctor n cfg = GMT (GrammarRule ctor n cfg)
type MTL ctor n cfg = GMTL (GrammarRule ctor n cfg)
build_rule :: ctor -> n -> MTL ctor n cfg -> MT ctor n cfg 
build_rule ctor n mls = node (Rule ctor (toMaybe n)) mls -- $ node n mls

build_cfg :: cfg -> MT ctor n cfg
build_cfg cfg = leaf $ Config cfg

-- build_ref :: [forall x. Optional n x => x] -> [Maybe n]
-- build_ref = fmap toMaybe


mt2mtl :: MT ctor n cfg -> MTL ctor n cfg
mt2mtl mt = do
    tree <- mt
    return [tree]

setCtor ctor name = mt2mtl . build_rule ctor name
choice = setCtor "choice"
sequnc = setCtor "sequence"
chc = choice
sqc = sequnc

ref = undefined
xx :: MTL String String Int
xx = choice "hhh" $ do
    choice "ggg" (return [])



-- data RuleType name config = Choice name [(Int, name)] config
-- type RuleInfo name config = 
{-
choice :: (Optional name a, Monad m) => 
            a -> [forall b. Optional name b => b] 
            -> m (GrammarRule name)
choice name subnames = return (Rule (toMaybe name) $ map toMaybe subnames)
-}
{-
block :: (Optional name a, Monad m) => 
            a -> m [SubName name] -> m (GrammarRule name)
block name action = do
    subnames <- action
    return . Rule (toMaybe name) $ subnames

-- choice = undefined

-- c = block "A" do -- error ??
-- c = block "A" . return =<< do
--        block "B" $ return [Just "xx"]
        -- block None $ return [Nothing]

-}


