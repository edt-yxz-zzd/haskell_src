
module ZipExceptLast
    (zip_except_last
    )
where


-- lhs is non-empty
-- rhs is infinite or len(rhs) >= len(lhs)-1
zip_except_last :: [a] -> [b] -> b -> [(a,b)]
zip_except_last lhs rhs last = f lhs rhs where
    f (a:lhs@(_:_)) (b:rhs) = (a,b): f lhs rhs
    f [a] _ = [(a,last)]
    f [] _ = undefined
    f _ [] = undefined
