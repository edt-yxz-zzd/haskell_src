

{-# LANGUAGE
  MultiParamTypeClasses
, FunctionalDependencies
, FlexibleInstances
 #-}  -- NOTE: not allow re"^#-}" !!!!!!!!!!!!!!!!!
-- why not "f x :: XType _" if f :: _ -> F a and instance F (XType a) ???




import Data.Monoid -- Endo { appEndo :: a-> a }
-- real world haskell 318 | Chapter 13:?Data Structures
-- The  head function has constant cost for lists. Our  DListequivalent requires that we convert the entire DListto a regular list, so it is much more expensive than its list counterpart¡ªits cost is linear in the number of appends we have performed to construct the DList


-- head DList = O(num(mappend))
newtype DList a = DL { unDL :: [a]->[a] }
class List a ls | ls -> a where
    toList :: ls -> [a]
    fromList :: [a] -> ls
instance List a (DList a) where
    toList (DL f) =  f []
    fromList xs = DL (xs ++)
instance List a [a] where
    toList =  id
    fromList = id

instance Monoid (DList a) where
    mempty = DL id -- [] ++
    mappend (DL xs) (DL ys) = DL (xs . ys)


left2right xs n
    | n < 1 = mempty
    | otherwise = mappend xs (left2right xs $ n-1)

right2left xs n
    | n < 1 = mempty
    | otherwise = mappend (right2left xs $ n-1) xs
xs = [1..100]
ds = (fromList :: [a] -> DList a) xs -- :: DList _
n = 1000000
-- head ls  faster than  head rs
-- take 10 rs  is funny
-- ==>> ((..) ++ xs) ++ xs should go down each time!!
ls = left2right xs n
rs = right2left xs n


-- ld is fast
-- rd only slow when call head, follow take n rd is fast
-- it seems like ((..).f).f).f) []
--      ==>> eval use time num(.) ==>> f -> f-> .... -> []
ld = toList $ left2right ds n
rd = toList $ right2left ds n



-- use Dual instead
newtype UnorderList a = UL {unUL :: [a]}
instance Monoid (UnorderList a) where
    mempty = UL []
    mappend (UL xs) (UL ys) = UL (ys++xs)


