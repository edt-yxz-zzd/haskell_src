

module Lambda.OpFreshID where

import qualified Data.Set as S
import Data.Maybe
data IDEX = IDEX String Integer
    -- xxx($1111$)?  -- 'xxx' not endswith '$'
    deriving (Read, Show, Eq, Ord)
newtype ID = ID String
    deriving (Read, Show, Eq, Ord)
unID (ID x) = x
mkID = ID


get_max_elem :: S.Set a -> Maybe a
get_max_elem__list :: Ord a => [S.Set a] -> Maybe a
get_max_elem = fmap fst . S.maxView
get_max_elem__list =
    get_max_elem . S.fromList . catMaybes . map get_max_elem

_big_str :: String -> String
_big_strs :: String -> [String]
_big_strs = tail . iterate _big_str
_big_str [] = "A"
_big_str s = foldr f "" s where
    f ch [] = if ch >= 'Z' then [ch, 'A'] else
        if ch < 'A' then "A" else [succ ch]
    f ch ts = ch : ts
class Ord id => OpFreshID id where
    fresh_id :: [S.Set id] -> id
    fresh_ids :: [S.Set id] -> [id]
    default_id :: id
instance OpFreshID ID where
    default_id = ID "A"
    fresh_id =
        ID . _big_str . unID . maybe (ID "") id . get_max_elem__list
    fresh_ids =
        map ID . _big_strs . unID . maybe (ID "") id . get_max_elem__list

class OpStr a where
    str :: a -> String -- v.s. repr/show
instance OpStr ID where
    str = unID

sepIDEX = '$'
assertIDEX_num i =
        if i < 0 then error "(IDEX name i) where i < 0"
        else i
assertIDEX_name s =
        if null s then error "(IDEX name i) where name == \"\"" else
        if last s == sepIDEX
        then error $ "(IDEX name i) where last name == " ++ [sepIDEX]
        else s
assertIDEX_args s i = (assertIDEX_name s, assertIDEX_num i)
assertIDEX (IDEX s i) = case assertIDEX_args s i of
        (name, n) -> IDEX name n
mkIDEX s i = assertIDEX $ IDEX s i
succIDEX (IDEX s i) = IDEX s $ succ i
succsIDEX (IDEX s i) = map (IDEX s) [succ i..]
instance OpFreshID IDEX where
    default_id = IDEX "x" 0
    fresh_id  = succIDEX  . maybe default_id id . get_max_elem__list
    fresh_ids = succsIDEX . maybe default_id id . get_max_elem__list
instance OpStr IDEX where
    --- "xxx" or "xxx$124$"
    str id = case assertIDEX id of
        IDEX s i -> if i == 0 then s else s ++ ends i
        where
            ends i = wrap $ show i
            wrap_ c s = c : s ++ [c]
            wrap = wrap_ sepIDEX


