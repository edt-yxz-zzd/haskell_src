
module SimpleContinuedFraction
    (SimpleContinuedFraction(..)
    ,unSimpleContinuedFraction
    ,unsafe_mkSimpleContinuedFraction
    ,maybe_make_finite_SimpleContinuedFraction
    )
where

import PIntList
    (PIntList
    ,unPIntList
    ,unsafe_mkPIntList
    ,maybe_mk_finite_PIntList
    )

data SimpleContinuedFraction
    = SimpleContinuedFraction Integer PIntList
    deriving (Show, Eq)

unSimpleContinuedFraction :: SimpleContinuedFraction -> (Integer, [Integer])
unSimpleContinuedFraction (SimpleContinuedFraction floor_part float_part)
    = (floor_part, unPIntList float_part)

unsafe_mkSimpleContinuedFraction :: Integer -> [Integer] -> SimpleContinuedFraction
unsafe_mkSimpleContinuedFraction floor_part
    = SimpleContinuedFraction floor_part . unsafe_mkPIntList

maybe_make_finite_SimpleContinuedFraction :: Integer -> [Integer] -> Maybe SimpleContinuedFraction
maybe_make_finite_SimpleContinuedFraction floor_part cf_digits = do
    ls <- maybe_mk_finite_PIntList cf_digits
    return $ SimpleContinuedFraction floor_part ls


