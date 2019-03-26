
app_zip :: [a->b] -> [a] -> [b]
app_zip (f:fs) (x:xs) = f x : app_zip fs xs
app_zip _ _ = []

appN :: Integer -> (a->b) -> ??
appN n f = appN (n-1) ?????????????
zipN_with :: Integer
zipN_with n f = repeat f
zipN_with_ :: Integer -> (a->b) -> [a] -> Either [b] (Integer)
zipN_with_ n f xs
    | n <= 0 = error "zipN_with_ : n <= 0"

