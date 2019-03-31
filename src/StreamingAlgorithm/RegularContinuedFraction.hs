
module RegularContinuedFraction
    (RegularContinuedFraction(..)
    ,unRegularContinuedFraction
    ,unsafe_mkRegularContinuedFraction
    ,maybe_make_finite_RegularContinuedFraction
    )
where

import PIntList
    (PIntList
    ,unPIntList
    ,unsafe_mkPIntList
    ,maybe_mk_finite_PIntList
    )

data RegularContinuedFraction
    = RegularContinuedFraction Integer PIntList
    deriving (Show, Eq)

unRegularContinuedFraction :: RegularContinuedFraction -> (Integer, [Integer])
unRegularContinuedFraction (RegularContinuedFraction floor_part float_part)
    = (floor_part, unPIntList float_part)

unsafe_mkRegularContinuedFraction :: Integer -> [Integer] -> RegularContinuedFraction
unsafe_mkRegularContinuedFraction floor_part
    = RegularContinuedFraction floor_part . unsafe_mkPIntList

maybe_make_finite_RegularContinuedFraction :: Integer -> [Integer] -> Maybe RegularContinuedFraction
maybe_make_finite_RegularContinuedFraction floor_part cf_digits = do
    ls <- maybe_mk_finite_PIntList cf_digits
    return $ RegularContinuedFraction floor_part ls


