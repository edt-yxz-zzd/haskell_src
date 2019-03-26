

import Data.List (transpose)
type Name = String
data IterSentences = T Name | N Name [Sentence]
type Sentence = [IterSentences]
{-
    S = S
    S = a S
    S = a
    a is T
-}
take_heads :: Integer -> [[a]] -> ([a], [[a]])
take_heads i lsls | i <= 0 = ([], lsls)
take_heads i lsls = f i lsls where
    f 0 lsls = ([], lsls)
    f i ((a:ls):lsls) = (a:hs, ls:lsls') where
        (hs, lsls') = f (pred i) lsls
    f i ([]:lsls) = f (pred i) lsls
    f i [] = ([], [])
table2list :: [[a]] -> [a]
table2list lsls = f 1 lsls where
    f _ [] = []
    f i lsls = hs ++ f (succ i) lsls' where
        (hs, lsls') = take_heads i lsls

product2 :: (a->b->c) -> [a] -> [b] -> [[c]]
product2 op ls bs = [map (op a) bs |a <- ls]
product2_ :: (a->b->c) -> [a] -> [b] -> [c]
product2_ op ls bs = table2list $ product2 op ls bs
products :: (a->b->b) -> [[a]] -> [b] -> [b]
products op ass bs = foldr (product2_ op) bs ass
_product :: [IterSentences] -> [Sentence]
_product ss = products (++) (map to_sentences ss) [[]]
to_sentences :: IterSentences -> [Sentence]
to_sentences a@(T name) = [a]:[]
to_sentences a@(N name ss) = [a]:ss
to_sentences_ = tail . to_sentences
to_name :: IterSentences -> Name
to_name (T name) = name
to_name (N name ss) = name
to_names :: Sentence -> [Name]
to_names = map to_name
s1 = N "S" $ _product [s]
s2 = N "S" $ _product [a,s]
s3 = N "S" $ _product [a]
a = T "a"
s = N "S" $ concat $ transpose . map to_sentences_ $ [s1,s2,s3]

notN (N _ _) = False
notN _ = True
r = map to_names . filter (all notN) $ to_sentences_ s
pr = do
    print $ take 100 r
    --print $ take 1000 $ table2list [[0..] | i <- [0..]]


