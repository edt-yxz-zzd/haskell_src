
module EngelExpansion
    (EngelExpansion(..)
    ,unEngelExpansion
    ,unsafe_mkEngelExpansion
    ,maybe_make_finite_EngelExpansion
    )
where

import GeTwoGrowingList

data EngelExpansion = EngelExpansion Integer GeTwoGrowingList
    -- floor_part float_part
    -- float_part + 1/d[0] * (1 + 1/d[1]*(...))
    -- 2 <= d[0] <= d[1] ...
    deriving (Show, Eq)

unEngelExpansion :: EngelExpansion -> (Integer, [Integer])
unsafe_mkEngelExpansion :: Integer -> [Integer] -> EngelExpansion
maybe_make_finite_EngelExpansion :: Integer -> [Integer] -> Maybe EngelExpansion

unEngelExpansion (EngelExpansion floor_part float_part)
    = (floor_part, unGeTwoGrowingList float_part)
unsafe_mkEngelExpansion floor_part
    = EngelExpansion floor_part . unsafe_mkGeTwoGrowingList
maybe_make_finite_EngelExpansion floor_part ls = do
    float_part <- maybe_make_finite_GeTwoGrowingList ls
    return $ EngelExpansion floor_part float_part


