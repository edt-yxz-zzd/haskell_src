
module RE.Refine where

{-
TODO:
    range_partition

-}
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (groupBy, sortBy)
import Numeric.Natural


mergeMap :: Ord k => Map k a -> Map k b -> Map k (a,b)
mergeMap = M.mergeWithKey
    (\k a b -> Just (a,b)) (const M.empty) (const M.empty)

eq_fst :: Eq a => (a, b) -> (a, b) -> Bool
cmp_fst :: Ord a => (a, b) -> (a, b) -> Ordering
eq_fst p q = fst p == fst q
cmp_fst p q = fst p `compare` fst q
all_the_same :: Eq a => [a] -> Bool
all_the_same [] = True
all_the_same (h:ls) = all (h ==) ls



is_refinement :: (Ord k, Ord subcolor, Eq color)
    => Map k subcolor -> Map k color -> Bool
is_refinement subpartition partition = b0 && b1 where
    -- subcolor -> color -- many-to-1
    b0 = M.keysSet subpartition == M.keysSet partition
    sub_color_pairss = groupBy eq_fst . sortBy cmp_fst . M.elems
        $ mergeMap subpartition partition
    colorss = map (map snd) sub_color_pairss
    b1 = and $ map all_the_same colorss



refine__Functor
    :: (Ord a, Functor f, Ord (f Natural))
    => (f Natural -> f Natural)
    -> f a -> Map a (f a) -> Map a Natural -> Map a Natural
refine__Functor complete_color default_info point2info partition =
    refine fMap default_info point2info partition where
    fMap partition = complete_color . recolor default_error_nat partition
    default_error_nat = fromIntegral $ M.size partition



-- info = f a; color = f Natural; f a - e.g. Map k a; Set a
refine
    :: (Ord a, Ord color)
    => (Map a Natural -> info -> color)
    -> info -- default info for (Map a info)
    -> Map a info
    -> Map a Natural -> Map a Natural
refine fMap default_info point2info partition = partition' where
    point2info' = M.union point2info $ fmap (const default_info) partition
    partition' = __refine fMap point2info' partition

__refine
    :: (Ord a, Ord color)
    => (Map a Natural -> info -> color)
    -> Map a info
    -> Map a Natural -> Map a Natural
__refine fMap point2info partition =
    if M.keysSet partition /= M.keysSet point2info'
        then error "not (keysSet partition <= keysSet point2info)"
        else f partition
  where
    -- fMap :: partition -> info -> color
    -- assert keysSet partition <= keysSet point2info
    -- assert keysSet partition == keysSet point2info'
    -- if partition[a] == nat
    --    && fMap partition point2info[a] == color
    --    && fMap partition point2info[b] == color
    --    ==>> partition[b] == nat
    --    i.e. fMap is a refinement
    point2info' = M.intersection point2info partition
    {-
    merge = M.mergeWithKey
                (\point nat color -> Just (nat, color))
                -- assert keysSet partition <= keysSet point2info
                -- partition -> point2nat_color
                undefined
                -- assert keysSet partition == keysSet point2info'
                -- point2color -> point2nat_color
                undefined
                -- partition point2color
    -}
    merge = mergeMap
    f partition = r where
        -- Map a color
        point2color = M.map (fMap partition) point2info'
        -- Map a (nat, color)
        point2nat_color = merge partition point2color
        partition' = recolor_by_Natural point2nat_color
        r = if M.size partition' == M.size partition
            then partition' else f partition'


mkOld2NewDefault :: Ord old => Map old new -> new -> (old -> new)
mkOld2NewDefault dict new old = M.findWithDefault new old dict
mkOld2New :: Ord old => Map old new -> (old -> new) -> (old -> new)
mkOld2New old2new f_old2new old
    = M.findWithDefault (f_old2new old) old old2new
recolorEx
    -- :: (Ord k, Ord old)
    -- => (old -> new) -> Map old new -> Map k old -> Map k new
    :: (Ord old, Functor f)
    => (old -> new) -> Map old new -> f old -> f new
recolorEx f_old2new old2new = fmap $ mkOld2New old2new f_old2new
-- recolor :: (Ord k, Ord old) => Map old new -> Map k old -> Map k new
recolor :: (Ord old, Functor f) => new -> Map old new -> f old -> f new
recolor = recolorEx . const



recolor_by_Natural :: (Ord k, Ord color) => Map k color -> Map k Natural
recolor_by_Natural k2color = recolor 0 color2nat k2color where
    colors = S.toList . S.fromList $ M.elems k2color
    color2nat = M.fromList $ zip colors [0..]

