{-# LANGUAGE Rank2Types #-}

module TH.Query
where
import Data.Data

num_args :: (Integral i, Data a) => a -> i
num_args = sum . gmapQ (const 1)
query_list_like :: (forall a. Data a => a -> r) -> (forall a. Data a => a -> [r])
query_list_like f a = case num_args a of
    2 -> gmapQi 0 f a : gmapQi 1 (query_list_like f) a
    0 -> []
    _ -> error "value error"


