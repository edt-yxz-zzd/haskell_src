import Control.Arrow


s = second (+1)
tl2 = s (1, 2)


-- all fails:

tl3 = (1, 2, 3)
tl3' = s tl3


data T3 = T3 Int Int Int
t3 = s $ T3 1 2 3

data T2 = T2 Int Int Int
t2 = s $ T2 1 2



