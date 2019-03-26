{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module RE.RangeMap where


import Data.Map (Map)
import qualified Data.Map as M
import RE.RangePoint

newtype RangeMap p v = RangeMap (Map (RightBound p) (LeftBound p, v))
    deriving (Read, Show, Eq, Ord, Functor)
    -- rb `member` d ==>> not $ is_empty_range (fst $ get rb d, rb)
    -- rb1, rb2 `member` d && rb1 < rb2 ==>> is_empty_range (fst $ get rb2 d, rb1)
newtype AutoMergeRangeMap p v = AutoMergeRangeMap (RangeMap p v)
    deriving (Read, Show, Eq, Ord, Functor)
    -- rb1, rb2 `member` d && rb1 < rb2 ==>>
    --      let lb2 = fst $ get rb2 d
    --          p2 = unLeftBound lb2
    --          p1 = unRightBound rb1
    --      p1 < p2 || (p1 < p2 && (not $ includedRightBound rb1) && (not $ includedLeftBound lb2))



emptyRangeMap :: RangeMap p v
emptyAutoMergeRangeMap :: AutoMergeRangeMap p v
mkFullRangeMap :: RangePoint p => v -> RangeMap p v
mkFullAutoMergeRangeMap :: RangePoint p => v -> AutoMergeRangeMap p v
emptyRangeMap = RangeMap M.empty
emptyAutoMergeRangeMap = AutoMergeRangeMap emptyRangeMap
mkFullRangeMap v = insertRangeMap mkFullRange v emptyRangeMap
mkFullAutoMergeRangeMap = AutoMergeRangeMap . mkFullRangeMap

iterRangeMap :: RangeMap p v -> [(Range p, v)]
iterAutoMergeRangeMap :: AutoMergeRangeMap p v -> [(Range p, v)]
iterAutoMergeRangeMap (AutoMergeRangeMap d) = iterRangeMap d
iterRangeMap (RangeMap d) = map f $ M.toList d where
    f (rb, (lb, v)) = ((lb, rb), v)

may_getRangeMap :: RangePoint p => RawValue p -> RangeMap p v -> Maybe v
insertRangeMap :: RangePoint p => Range p -> v -> RangeMap p v -> RangeMap p v
insertsRangeMap :: RangePoint p => [(Range p, v)] -> RangeMap p v -> RangeMap p v
fromListRangeMap :: RangePoint p => [(Range p, v)] -> RangeMap p v
may_getAutoMergeRangeMap :: RangePoint p => RawValue p -> AutoMergeRangeMap p v -> Maybe v
insertAutoMergeRangeMap :: (Eq v, RangePoint p)
    => Range p -> v -> AutoMergeRangeMap p v -> AutoMergeRangeMap p v
insertsAutoMergeRangeMap :: (Eq v, RangePoint p)
    => [(Range p, v)] -> AutoMergeRangeMap p v -> AutoMergeRangeMap p v
fromListAutoMergeRangeMap :: (Eq v, RangePoint p)
    => [(Range p, v)] -> AutoMergeRangeMap p v
insertsRangeMap = flip $ foldr (uncurry insertRangeMap)
fromListRangeMap = flip insertsRangeMap emptyRangeMap
insertsAutoMergeRangeMap = flip $ foldr (uncurry insertAutoMergeRangeMap)
fromListAutoMergeRangeMap = flip insertsAutoMergeRangeMap emptyAutoMergeRangeMap

insertRangeMap rng@(lb, rb) v r''@(RangeMap d) = r where
    r = if is_empty_range rng then r'' else r'
    (small, may_midV, big) = M.splitLookup rb d
    rb_ = left2touchedRightBound lb
    (_, middle) = M.split rb_ small
    handle_big_head middle big = case may_midV of
        Just x -> (M.insert rb x middle, [])
        _ -> case M.minViewWithKey big of
            Just ((rb', x'@(lb', v')), _) ->
                if is_empty_range (lb', rb) then (middle, []) else
                    ( M.insert rb' x' middle
                    , [(rb', (right2touchedLeftBound rb, v'))]
                    )
            _ -> (middle, [])
    handle_middle_head i@(middle, to_inserts) =
        case M.minViewWithKey middle of
            Just ((rb', x'@(lb', v')), _) ->
                if is_empty_range (lb', rb_) then i else
                    (middle, (rb_, x') : to_inserts)
            _ -> i
    (middle', to_inserts') = fmap ((rb, (lb, v)):) . handle_middle_head
                                $ handle_big_head middle big
    d' = foldr M.delete d $ M.keys middle'
    d'' = foldr (uncurry M.insert) d' to_inserts'
    r' = RangeMap d''

may_getRangeMap k (RangeMap d) = r where
    p = raw_value2point k
    rb = RightExcluded p
    (_, big) = M.split rb d
    r = do
        ((rb', (lb', v)), _) <- M.maxViewWithKey big
        if p `is_in_range` (lb', rb') then Just v else Nothing
may_getAutoMergeRangeMap k (AutoMergeRangeMap d) = may_getRangeMap k d
insertAutoMergeRangeMap rng@(lb, rb) v r''@(AutoMergeRangeMap d) = r where
    r = if is_empty_range rng then r'' else r'
    r' = AutoMergeRangeMap (RangeMap d'')
    RangeMap d' = insertRangeMap rng v d
    (small, big) = M.split rb d'
    handle_small d =
        -- [lb', rb') [lb, rb) ==>> [lb', rb)
        case M.maxViewWithKey small of
            Just ((rb', (lb', v')), _) ->
                if not (touchedBound rb' lb && v == v') then (lb, d) else
                    (lb', M.insert rb (lb', v) $ M.delete rb' d)
            _ -> (lb, d)
    handle_big (lb, d) =
        -- [lb, rb) [lb', rb') ==>> [lb, rb')
        case M.minViewWithKey big of
            Just ((rb', (lb', v')), _) ->
                if not (touchedBound rb lb' && v == v') then d else
                    M.insert rb' (lb, v) $ M.delete rb d
            _ -> d
    d'' = handle_big $ handle_small d'




