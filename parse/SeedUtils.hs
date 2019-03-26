{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE UndecidableInstances #-}



module SeedUtils where
import qualified Data.List as L
import qualified Data.Map as M -- (Map)
import qualified Data.Set as S -- (Set)
import Data.Map (Map)
import Data.Set (Set)
import Prelude hiding (fromInteger)
import qualified Prelude as P

-- overflow version of P.fromInteger
fromInteger :: P.Integral n => Integer -> n
fromInteger i = let n = P.fromInteger i in
    if i /= P.toInteger n then error "overflow: fromInteger" else n



class Default a where
    default_ :: a
    -- Monoid a :: mempty :: a
    -- Empty e c :: empty :: c
    -- MonadPlus m :: mzero :: m a
findD :: (Ord k, Default a) => k -> Map k a -> a
findD = M.findWithDefault default_
instance Default [a] where
    default_ = []
instance Default (Maybe a) where
    default_ = Nothing
instance Default (Set a) where
    default_ = S.empty
instance Default (Map k a) where
    default_ = M.empty
-- Int = 0 for sum; 1 for product

-------------- monad
(>><) :: Monad m => m a -> m b -> m a -- like >> but discard b
a >>< b = do
    r <- a
    b
    return r
(>>=<) :: Monad m => m a -> (a -> m b) -> m a -- like >>= discard b
a >>=< f = do
    r <- a
    f r
    return r
mvoid :: Monad m => m ()
mvoid = return ()
-------------------------

-- see "Prelude.asTypeOf"
-- sametype :: a -> a -> ()
-- sametype _ _ = ()
type_as :: (b~a) => b -> a -> a -- return second
type_as _ = id


-- see "Data.Function.on"
lift2 :: (a->b) -> (b->b->c) -> (a->a->c)
lift2 a2b op a0 a1 = a2b a0 `op` a2b a1


-- see "Data.Maybe.listToMaybe"
-- see "Container.safe_head"
{-
safe_head :: [a] -> Maybe a
safe_head (a : _) = Just a
safe_head [] = Nothing
-}


class Test a where
    test :: a -> Bool
instance Test Bool where
    test a = a
instance Test (Maybe a) where
    test Nothing = False
    test _ = True
instance Test [a] where
    test = not . null
    --test [] = False
    --test _ = True
instance Test (Set a) where
    test = not . S.null
instance Test Integer where
    test = (0 /=)



--------------- Either
isLeft (Left _) = True
isLeft _ = False
isRight = not . isLeft
unLeft (Left a) = a
unLeft _ = error "unLeft Right!"
unRight (Right b) = b
unRight _ = error "unRight Left!"
mayLeft (Left a) = Just a
mayLeft _ = Nothing
mayRight (Right b) = Just b
mayRight _ = Nothing





--------------- maybe
class Classify b out | out -> b where
    just :: b -> out

justif b a = if b then Just a else Nothing
jcheck :: (a->Bool) -> a -> Maybe a
jcheck f a = justif (f a) a
--jcheck :: (a->Bool) -> Maybe a -> Maybe a
--jcheck f m = m >>= check f
instance Test a => Classify a (Maybe a) where
    just = jcheck test

-- see "Data.Maybe.fromMaybe"
-- may default m = P.maybe default id m
may :: a -> Maybe a -> a
may _ (Just a) = a
may a _ = a
-- see "Data.Maybe.maybeToList/listToMaybe"
may2ls :: Maybe a -> [a]
may2ls (Just a) = [a]
may2ls _ = []
-- see "Data.Maybe.fromJust"
unjust :: Maybe a -> a
unjust (Just a) = a
unjust _ = error "unjust Nothing"
mayTakeWhile (Just a : ls) = a : mayTakeWhile ls
mayTakeWhile _ = []



{-
class Test a => ToMaybe a where
    -- e.g. Map.alter :: ((Maybe a)->(Maybe a)) -> ...
    just :: a -> Maybe a
    just a = justif (test a) a
-}




------------------ tuple
map2 :: (a -> b) -> (a, a) -> (b, b)
map2 f (a, b) = (f a, f b)
map2_2 :: (a -> c, b -> d) -> (a, b) -> (c, d)
map2_2 (f, g) (a, b) = (f a, g b)
map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (a, b, c) = (f a, f b, f c)
------------------ list
for :: [a] -> (a -> b) -> [b]
for = flip map
filter_not :: (a -> Bool) -> [a] -> [a]
filter_not p = filter (not . p)
blocksLenN :: Integer -> [a] -> [[a]]
blocksLenN n = f where
    n' = fromInteger n
    f ls = if P.null ls then [] else
        let (h, ts) = splitAt n' ls in h : f ts


compareLen :: [a] -> [b] -> Ordering
compareLen (_:_) [] = GT
compareLen [] (_:_) = LT
compareLen [] [] = EQ
compareLen (_:a) (_:b) = compareLen a b
eqLen :: [a] -> [b] -> Bool
eqLen a b = compareLen a b == EQ
ltLen :: [a] -> [b] -> Bool
ltLen a b = compareLen a b == LT
leLen :: [a] -> [b] -> Bool
leLen a b = compareLen a b /= GT


initsWhile :: (a -> Bool) -> [a] -> [[a]]
initsWhile f = L.inits . takeWhile f
{-
initsWhile f (a:ls)
    | f a = (:) [] $ map (a:) $ initsWhile f ls
initsWhile _ _ = [[]]
-}


-- fold
safe_min :: Ord a => [a] -> Maybe a
safe_min [] = Nothing
safe_min ls = Just $ L.minimum ls

minByKey :: Ord key => (a -> key) -> [a] -> Maybe a
minByKey f ls = minByKey_ex f ls >>= return . snd
minByKey_ex :: Ord key => (a -> key) -> [a] -> Maybe (key, a)
minByKey_ex _ [] = Nothing
minByKey_ex key ls = Just $ L.minimumBy f $ map (\a -> (key a, a)) ls
  where
    -- f (lkey, _) (rkey, _) = compare lkey rkey
    f = lift2 fst compare





-- see "L.iterate/scanl/unfoldr"
-- closure; see Data.Function.fix
find_fixed_pointBy :: (a->a->Bool) -> (a->a) -> a -> a
find_fixed_pointBy eq f a = ffp a where
    ffp old = let new = f old in if new `eq` old then old else ffp new
gen_list :: (a -> Maybe a) -> a -> [a]
gen_list f a = gen_list_m f $ Just a
gen_list_m :: (a -> Maybe a) -> Maybe a -> [a]
gen_list_m f (Just a) = a : gen_list_m f (f a)
gen_list_m _ _ = []

next_ls :: (a -> [a]) -> [a] -> [a]
next_ls f = merge_lsls . map f
follow_lsls :: (a -> [a]) -> [a] -> [[a]]
follow_lsls f ls = gen_list_m (just . next_ls f) (just ls)
follow_closure :: (a -> [a]) -> [a] -> [a]
follow_closure f = merge_lsls . follow_lsls f


followButFinal :: (a -> Bool) -> ((a->[a])->b) -> ((a->[a])->b)
followButFinal is_final follow step = follow step' where
    step' a = if is_final a then [] else step a
follow_closureBut :: (a -> Bool) -> (a -> [a]) -> [a] -> [a]
follow_closureBut = flip followButFinal follow_closure
follow_lslsBut :: (a -> Bool) -> (a -> [a]) -> [a] -> [[a]]
follow_lslsBut = flip followButFinal follow_lsls
next_lsBut :: (a -> Bool) -> (a -> [a]) -> [a] -> [a]
next_lsBut = flip followButFinal next_ls




-- len_le :: Integer -> [a] -> Bool

only_head :: [a] -> a
only_head [a] = a
only_head [] = error "only_head []"
only_head _ = error "only_head (a:b:_)"


-- input: countable list of countable list of a
merge_lsls :: [[a]] -> [a]
merge_lsls lsls = merge [] lsls where
    -- merge finite_countable countable_countable
    merge finite (ls:countable) = heads ++ after where
        (heads, tails) = heads_ex finite -- $ filter (not . null) finite
        after = merge (ls:tails) countable
    merge finite [] = merge_finite_lsls finite

-- input: will be (filter (not . null) input)
-- output: ([head], [tail])
--  len heads = len tails <= len input
heads_ex :: [[a]] -> ([a], [[a]])
heads_ex ((a:ls):lsls) = (a:heads, ls:tails) where
    (heads, tails) = heads_ex lsls
heads_ex ([]:lsls) = heads_ex lsls
heads_ex [] = ([], [])



-- input: finite list of countable list of a
--      length input
merge_finite_lsls :: [[a]] -> [a] -- length input
merge_finite_lsls [] = []
merge_finite_lsls finite = heads ++ after where
    (heads, tails) = heads_ex finite
    after = merge_finite_lsls tails
{-
merge_finite_lsls [] = []
merge_finite_lsls lsls =
    let (outs, next_input) = foldr f ([], []) lsls
    in  outs ++ merge_finite_lsls next_input where
    f :: [a] -> ([a], [[a]]) -> ([a], [[a]])
    f (a:ls) (outs, next_input) = (a:outs, ls:next_input)
    -- why fail?? "Equations for `f' have different numbers of arguments"
    -- f _ = id
    f _ x = x
-}

--
swap_pair_in_ls :: [(a, b)] -> [(b, a)]
swap_pair_in_ls = fmap $ \(a, b) -> (b, a)

-- fmerge_x_ys_pairs x2key fromList union input
-- Foldable input??
fmerge_x_ys_pairs :: Ord a => (x->a) -> (bs->fb) -> (fb -> fb -> fb)
                                -> [(x, bs)] -> Map a fb
fmerge_x_ys_pairs x2key fromls union = foldr f M.empty where
    f (x, ys) = M.insertWith' union (x2key x) $ fromls ys

group_pairs :: (Ord a) => [(a, b)] -> Map a [b]
group_pairs = M.map ($ []) . fmerge_x_ys_pairs id (:) (.)
group_pairs__set :: (Ord a, Ord b) => [(a, b)] -> Map a (Set b)
group_pairs__set = M.map S.fromList . group_pairs
merge_x_ys_pairs__set :: (Ord a, Ord b) => [(a, [b])] -> Map a (Set b)
merge_x_ys_pairs__set = fmerge_x_ys_pairs id S.fromList S.union


-- impossible: direct_product :: countable[finite[a]]
-- direct_product :: finite[countable[a]]
direct_product_finite_countables :: [[a]] -> [[a]]
direct_product_finite_countables [] = [[]]
-- direct_product_finite_countables [countable] = map (:[]) countable
direct_product_finite_countables (ls:lsls) =
    direct_product2 (:) ls $ direct_product_finite_countables lsls
direct_product2 :: (a->b->c) -> [a] -> [b] -> [c]
direct_product2 op xs ys = merge_lsls xyss where
    -- countable xs ys
    xyss = map (\x->map (op x) ys) xs -- countable[countable[xy]]

-- direct_product :: finite[finite[a]]
direct_product :: [[a]] -> [[a]] -- [[1,2], [3]] -> [[1,3], [2,3]]
direct_product [] = [[]]
direct_product (ls:lsls) =
    let dp = direct_product lsls
    in  concat $ fmap (\a -> fmap (a:) dp) ls
ungroup :: [(a, [b])] -> [(a, b)]
ungroup a_bs_ls = [(a, b) | (a, bs) <- a_bs_ls, b <- bs]
ungroup_half :: [(a, [b])] -> [[(a, b)]]
ungroup_half a_bs_ls = [[(a, b) | b <- bs] | (a, bs) <- a_bs_ls]









-------------- closure


-- productive_closure all_keys -> key2product_rules
--                 -> init_productive_keys
--                 -> (final_productive_keys, non_productives)
productive_closure :: Ord a => Set a -> Map a [[a]]
                            -> Set a -> (Set a, Set a)
productive_closure all_keys k2r k2p = f k2p all_keys where
    -- k2r as implicit parameter below
    get_rules k = M.findWithDefault [] k k2r
    get_p k2p k = S.member k k2p -- M.findWithDefault False k k2p
    is_productive k2p k = get_p k2p k ||
                          (any (is_productive_rule k2p) $ get_rules k)
    is_productive_rule k2p ls = all (get_p k2p) ls
    -- :: Set a -> Set a -> Set a
    --   init_p_keys -> unknown_keys -> final_p_keys
    f k2p unknown_ks =
        let (news, unknown') = partition k2p unknown_ks
            k2p' = S.union k2p news
        in  if S.null news then (k2p, unknown_ks)
            else f k2p' unknown'
    partition k2p ks = S.partition (is_productive k2p) ks

rules2all_symbols :: (Ord a) => Map a [[a]] -> Set a
rules2all_symbols m = S.fromList $
    M.keys m ++ [a | lsls <- M.elems m, ls <- lsls, a <- ls]
easy_productive_closure :: Ord a => Map a [[a]] -> Set a -> (Set a, Set a)
easy_productive_closure rules = (productive_closure keys rules) where
    keys = rules2all_symbols rules





--------- list basic
take__ls :: Integer -> [a] -> [a]
take__ls i ls
    | i <= 0 = []
    | otherwise = take i ls where
        take i (a:ls) = if i == 0 then [] else a : take (i-1) ls
        take i [] = []


