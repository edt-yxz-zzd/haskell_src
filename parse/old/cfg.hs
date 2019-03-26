
{-
    attribute grammar
    N :: () -> Int
    N = Bs
        0.i = 1.i
    Bs :: () -> Int
    Bs = B
        0.i = 1.i
    Bs = Bs B
        0.i = 1.i * 2 + 2.i
    B :: () -> Int
    B = "0"
        0.i = 0
    B = "1"
        0.i = 1
-}


data N = N_0 Bs
data Bs = Bs_0 B | Bs_1 Bs B
data B = B_0 | B_1

data OutGram = OutGram_N 

data OutGram_N = OutGram_N_0 (Set OutGram_Bs)
data OutGram_Bs = OutGram_Bs_0 OutGram_B | OutGram_Bs_1 OutGram_Bs OutGram_B
data OutGram_B = OutGram_B_0 | OutGram_B_1






