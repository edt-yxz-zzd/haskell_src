{-# LANGUAGE  MultiParamTypeClasses
            , FunctionalDependencies
            , FlexibleInstances
            , FlexibleContexts #-}

module IntegralSet
    ( IntegralSet
    , insertRng
    , toRngs
    , fromRngs
    )
where
import Container
import Boxed
import qualified Data.Set as S
import SeedUtils (lift2, test, safe_head, jcheck, merge_finite_lsls, justif)
import Prelude hiding (null)
import Data.Maybe (catMaybes)


{-
why open range instead of close?
    (-oo, +oo)
-}

data MinMax a = Min | Data a | Max
    deriving (Show, Read, Eq, Ord)
bxxx = Min < Data 1 && Data 1 < Data 2 && Data 2 < Max
instance Bounded (MinMax a) where
    minBound = Min
    maxBound = Max
-- instance Enum a => Enum (MinMax a) where
get_data (Data a) = Just a
get_data _ = Nothing


---------- OpenRange
-- Ord a => (a, b) ==>> {x | a < x < b}
-- Integral a => (a-1, a+1) ==>> {a}
data OpenRange a = OpenRange (MinMax a) (MinMax a)
    deriving (Show, Read, Eq, Ord)
leftBound :: OpenRange a -> MinMax a
leftBound (OpenRange a _) = a
rightBound :: OpenRange a -> MinMax a
rightBound (OpenRange _ a) = a
mkRng :: a -> a -> OpenRange a
mkRng = lift2 Data OpenRange
inc :: Integral a => MinMax a -> MinMax a
inc (Data a) = Data (a+1)
inc inf = inf
dec :: Integral a => MinMax a -> MinMax a
dec (Data a) = Data (a-1)
dec inf = inf
instance Ord a => Container a (OpenRange a)
instance Integral a => Null a (OpenRange a) where
    null (OpenRange a b) = not $ inc a < b
instance Integral a => Empty a (OpenRange a) where
    empty = OpenRange Min Min -- lots of empties!!
instance Ord a => Member a (OpenRange a) where
    member a_ (OpenRange b c) = let a = Data a_ in b < a && a < c
instance Integral a => AnyElem a (OpenRange a) where
    any_elem s | null s = Nothing
    any_elem (OpenRange (Data a) _) = return (a+1)
    any_elem (OpenRange _ (Data a)) = return (a-1)
    any_elem (OpenRange _ _) = return 0
instance Integral a => Iterable a (OpenRange a) where
    -- iter s | null s = []
    iter (OpenRange (Data a) (Data b)) = [a+1..b-1]
    iter (OpenRange Min (Data a)) = [a-1, a-2..]
    iter (OpenRange (Data a) Max) = [a+1..]
    iter (OpenRange Min Max) = 0 : next 1 where
        next i = i : -i : next (i+1)
    iter _ = []
instance Integral a => Singleton a (OpenRange a) where
    singleton a = mkRng (a-1) (a+1) -- OpenRange (Data $ a-1) (Data $ a+1)

instance Integral a => Set a (OpenRange a) where
    subset s0 _ | null s0 = True
    subset (OpenRange a b) (OpenRange c d) = c <= a && b <= d
instance Integral a => SetIntersection a (OpenRange a) where
    intersection (OpenRange a b) (OpenRange c d)
        = OpenRange (max a c) (min b d)
    {-
    intersection s0 s1 | subset s0 s1 = s0 -- null s0 || null s1 = empty
    intersection s0 s1 | subset s1 s0 = s1
    intersection (OpenRange a b) (OpenRange c d)
        | a < c = OpenRange c b
        | otherwise = OpenRange a d -- a > c
    -}
instance Integral a => UniversalSet a (OpenRange a) where
    universal = OpenRange Min Max














-------------- IntegralSet
type OpenRangeSet a = S.Set (OpenRange a)
-- fromAscList [...(a, b), (c, d)...] ==>> a+1<b <= c<d-1
newtype IntegralSet a = IntegralSet { unIntegralSet :: OpenRangeSet a }
    deriving (Show, Read, Eq, Ord)
instance Boxed (OpenRangeSet a) (IntegralSet a) where
    box = IntegralSet
    unbox = unIntegralSet


instance Container a (IntegralSet a)
instance Null a (IntegralSet a) where
    null = S.null . unbox
instance Empty a (IntegralSet a) where
    empty = box S.empty

emptyRng a = OpenRange a Min -- mkRng a a
neighborRng :: Ord a => MinMax a -> IntegralSet a
            -> (Maybe (OpenRange a), Maybe (OpenRange a))
neighborRng a bs =
    let (s0, b, s1) = S.splitMember (emptyRng a) $ unbox bs
        f = (>>= return . fst)
    in  if b then error "not valid IntegralSet"
        else (f $ S.maxView s0, f $ S.minView s1)
    {-
        rng0 = S.findMax s0
        rng1 = S.findMin s1
        b0 = not (S.null s0) && member a rng0
        b1 = not (S.null s1) && member a rng1
    in if b0 then Just rng0 else if b1 then Just rng1 else Nothing
    -}
findRng :: Ord a => a -> IntegralSet a -> Maybe (OpenRange a)
findRng a bs = let (left, _) = neighborRng (Data a) bs in
    left >>= jcheck (member a)
    -- safe_head . filter (member a) $ catMaybes [left, right]
removeFromRng :: Integral a => a -> OpenRange a -> [OpenRange a]
removeFromRng a rng | not $ member a rng = [rng]
removeFromRng a_ (OpenRange b c) = let a = Data a_ in
    filter (not . null) [OpenRange b a, OpenRange a c]
instance Ord a => Member a (IntegralSet a) where
    member a s = test $ findRng a s
instance Integral a => Remove a (IntegralSet a) where
    remove a s = findRng a s >>= \rng -> do
        let s' = S.delete rng $ unbox s
        return ([a], box $ foldr insert s' $ removeFromRng a rng)

-- not (null set) ==>> lower = min {rng in set | a in rng or rng.leftbound >= a}
findLower_ :: Ord a => MinMax a -> IntegralSet a -> Maybe (OpenRange a)
findLower_ a bs =
    let (left, right) = neighborRng a bs
        left' = do
            a_ <- get_data a
            left >>= jcheck (member a_)
        ls = catMaybes [left', right]
    in  safe_head ls
findUpper_ :: Ord a => MinMax a -> IntegralSet a -> Maybe (OpenRange a)
findUpper_ a bs = fst $ neighborRng a bs
findLower :: Ord a => MinMax a -> IntegralSet a -> MinMax a
findLower a bs = min a $ maybe Max leftBound $ findLower_ a bs
findUpper :: Ord a => MinMax a -> IntegralSet a -> MinMax a
findUpper a bs = max a $ maybe Min rightBound $ findUpper_ a bs
coverRng :: Ord a => OpenRange a -> IntegralSet a -> OpenRange a
coverRng (OpenRange a b) bs = OpenRange (findLower a bs) (findUpper b bs)
coveredAscRngs :: Ord a => OpenRange a -> IntegralSet a
    -> ([OpenRange a], OpenRange a)
coveredAscRngs rng bs =
    let rng'@(OpenRange lower upper) = coverRng rng bs
        (_, s') = S.split (emptyRng lower) $ unbox bs
        (s'', _) = S.split (emptyRng upper) s'
    in  (S.toAscList s'', rng')
coveredRngs :: Ord a => OpenRange a -> IntegralSet a
    -> ([OpenRange a], OpenRange a)
coveredRngs  = coveredAscRngs


insertRngs :: Integral a => [OpenRange a] -> IntegralSet a -> IntegralSet a
insertRngs rngs bs = foldr insertRng bs rngs
insertRng :: Integral a => OpenRange a -> IntegralSet a -> IntegralSet a
insertRng rng bs = if null rng then bs else
    let (rngs, rng') = coveredRngs rng bs
        s = unbox bs
        s' = foldr S.delete s rngs
        s'' = S.insert rng' s'
    in  box s''


-- symDiffRngFromRng = (\\, //)
symDiffRngFromRng :: Integral a =>
    OpenRange a -> OpenRange a -> ([OpenRange a], [OpenRange a])
symDiffRngFromRng rng0 rng1 = map2 (filter not_null) $ f rng0 rng1 where
    f rng0 rng1
        | null (rng0 /\ rng1) = ([rng0], [rng1])
        | otherwise = (rng1 // rng0, rng0 // rng1)
    (//) (OpenRange a b) (OpenRange c d) =
        [OpenRange c (inc a), OpenRange (dec b) d]
    map2 f (a, b) = (f a, f b)
{-
-- diffRngFromRng = // = filp \\
diffRngFromRng :: Integral a =>
    OpenRange a -> OpenRange a -> [OpenRange a]
diffRngFromRng rng0 rng1 = snd $ symDiffRngFromRng rng0 rng1

removeRng :: Integral a => OpenRange a -> IntegralSet a
                -> Maybe ([OpenRange a], IntegralSet a)
removeRng rng bs =
    let (rngs, rng') = coveredRngs rng bs
        intersects = map (rng /\) rngs
        (rngs_off, rngs_on) = partition null intersects
        -- assert len rngs'' <= 2
        cover_rng = foldr (\off -> only_elem . (`diffRngFromRng` off))
                        rng' rngs_off
        s = unbox bs
        s' = foldr S.delete s rngs_on
        s'' = foldr S.insert s' . filter not_null
                $ diffRngFromRng cover_rng rng
        nonnull_intersects = filter not_null intersects
    in  justif (not (null rng || null rngs_on))
            (nonnull_intersects, box s'')
-}


instance Integral a => Insert a (IntegralSet a) where
    insert a s = insertRng (singleton a) s







instance Integral a => Set a (IntegralSet a) where
    subset bs0 bs1 =
        let ls0 = S.toAscList $ unbox bs0
            ls1 = S.toAscList $ unbox bs1
        in  f ls0 ls1 where
        f [] _ = True
        f _ [] = False
        f ls0@(a:ls0') ls1@(b:ls1')
            | subset a b = f ls0' ls1
            | leftBound a < rightBound b = False
            | otherwise = f ls0 ls1'

valid_asc_rngs :: Integral a => [OpenRange a] -> Bool
valid_asc_rngs ls = all ($ ls) [valid_asc_rngs1, valid_asc_rngs2]
valid_asc_rngs1 :: Integral a => [OpenRange a] -> Bool
valid_asc_rngs1 = all (not . null)
valid_asc_rngs2 :: Integral a => [OpenRange a] -> Bool
valid_asc_rngs2 ls@(_:ls') = all pred $ zip ls ls' where
    pred (a, b) = rightBound a <= leftBound b
valid_asc_rngs2 _ = True
valid_rngset :: Integral a => OpenRangeSet a -> Bool
valid_rngset s = valid s && valid_asc_rngs (S.toAscList s)
instance Integral a => Valid (IntegralSet a) where
    valid = valid_rngset . unbox

fromRngs :: Integral a => [OpenRange a] -> IntegralSet a
fromRngs = flip insertRngs empty
singletonFromRng :: Integral a => OpenRange a -> IntegralSet a
singletonFromRng rng = fromRngs [rng]
--singletonFromRng rng = if null rng then empty else box $ S.singleton rng
instance Integral a => UniversalSet a (IntegralSet a) where
    universal = singletonFromRng (OpenRange Min Max)








-----------
toRngs :: IntegralSet a -> [OpenRange a]
toRngs = S.toAscList . unbox
isInfLength (OpenRange (Data _) (Data _)) = False
isInfLength _ = True
instance Integral a => AnyElem a (IntegralSet a) where
    any_elem = safe_head . catMaybes . map any_elem . toRngs
instance Integral a => Iterable a (IntegralSet a) where
    iter = merge_finite_lsls . map iter . toRngs
instance Integral a => Pop a (IntegralSet a) where
    pops bs = f ls bs where
        ls = iter bs
        f (a:ls) c = let c' = delete a c in (a, c') : f ls c'
        f _ _ = []
instance Integral a => Singleton a (IntegralSet a) where


--------
instance Integral a => SetDifference a (IntegralSet a) where
    difference bs0 bs1 = box $ S.fromAscList $ filter not_null ls
      where
        ls = lift2 (S.toAscList . unbox) diff bs0 bs1
        diff ls0@(a:ls0') ls1@(b:ls1')
            | rng_lt_ b a = diff ls0 ls1'
            | rng_lt_ a b = a : diff ls0' ls1
            | null (a /\ b) = error "logic error"
            -- | subset a b = diff ls0' ls1
            | otherwise = let (d0, d1) = symDiffRngFromRng a b in
                            diff (d0 ++ ls0') (d1 ++ ls1')
        diff ls _ = ls

-- partial order
rng_lt (OpenRange a b) (OpenRange c _) = a <= c && inc b <= c
rng_lt_ (OpenRange _ b) (OpenRange c _) = b <= inc c
instance Integral a => SetIntersection a (IntegralSet a) where
    intersection bs0 bs1 = box $ S.fromAscList $ filter not_null ls
      where
        ls = lift2 (S.toAscList . unbox) inter bs0 bs1
        inter ls0@(a:ls0') ls1@(b:ls1')
            | rng_lt_ a b = inter ls0' ls1
            | rng_lt_ b a = inter ls0 ls1'
            | null (a /\ b) = error "logic error"
            | otherwise = let (d0, d1) = symDiffRngFromRng a b in
                            (a /\ b) : inter (d0 ++ ls0') (d1 ++ ls1')

rng_untouch (OpenRange _ b) (OpenRange c _) = b <= c
lefter (OpenRange a _) (OpenRange b _) = a < b
instance Integral a => SetUnion a (IntegralSet a) where
    union bs0 bs1 = box $ S.fromAscList $ filter not_null ls
      where
        ls = lift2 (S.toAscList . unbox) (u $ emptyRng Min) bs0 bs1
        u rng ls0@(a:ls0') ls1@(b:ls1')
            | lefter a b = if rng_untouch rng a
                            then rng : u a ls0' ls1
                            else u (merge rng a) ls0' ls1
            | otherwise = if rng_untouch rng b
                            then rng : u b ls0 ls1'
                            else u (merge rng b) ls0 ls1'
        u rng [] ls1 = u' rng ls1
        u rng ls0 [] = u' rng ls0
        u' rng (a:ls) = if rng_untouch rng a
                        then rng : ls
                        else (merge rng a) : ls
        u' rng _ = [rng]
        merge (OpenRange a b) (OpenRange _ d) = OpenRange a $ max b d

instance Integral a => SetSep a (IntegralSet a) where
instance Integral a => SetOp a (IntegralSet a) where
instance Integral a => AnyFiniteSize a (IntegralSet a) where
instance Integral a => Buffer a (IntegralSet a) where
instance Integral a => DynSet a (IntegralSet a) where
instance Integral a => DynUniversalSet a (IntegralSet a) where


newtype IntegralPartition s =
    IntegralPartition {unIntegralPartition :: Set s}




-- -}
-- -}
-- -}
-- -}
-- -}
