
module Seed.MapOps
where
import qualified Data.Map as M
import Data.Map (Map)


group_by_Map :: Ord k => (a->k) -> (a->v) -> [a] -> Map k [v] -> Map k [v]
group_by_Map fk fv ls d = foldr f d ls where
    f a d = d' where
        k = fk a
        v = fv a
        d' = M.alter (Just . maybe [v] (v:)) k d

