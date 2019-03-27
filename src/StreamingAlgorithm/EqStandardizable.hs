
module EqStandardizable
    (EqStandardizable(..)
    )
where

import Standardizable
class Standardizable a => EqStandardizable a where
    -- nonstd_eq a a == True
    -- nonstd_eq a b == nonstd_eq b a
    -- [nonstd_eq a b][nonstd_eq b c] ==>> [nonstd_eq a c]
    -- [nonstd_eq a b] ==>> [std_eq a b]
    -- [std_eq a b] <==> [nonstd_eq (standardize a) (standardize b)]
    nonstd_eq :: a -> a -> Bool
    std_eq :: a -> a -> Bool
    std_eq lhs rhs = nonstd_eq (standardize lhs) (standardize rhs)



