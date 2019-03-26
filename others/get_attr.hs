{-# LANGUAGE FunctionalDependencies #-}


infixl 9 #

(#) :: a -> (a->b) -> b
x # attr = attr x


{-
    -- it seems '..' can not be a operator
    -- [a .. b]
    infixl 9 ..
    
    (..) :: a -> (a->b) -> b
    x .. attr = attr x
-}

{-
    -- data T1 = T1 { val :: Int }
    -- data T2 = T2 { val :: Char } -- Multiple declarations of `val'
    
    val :: T2 -> Char
    val (T2 ch) = ch
    val :: T1 -> Int -- Multiple declarations of `val'
    val (T1 i) = i
-}

data T1 = T1 Int
data T2 = T2 Char

class HasAttr_val obj attr | obj -> attr where
    val :: obj -> attr
instance HasAttr_val T1 Int where
    val (T1 x) = x
instance HasAttr_val T2 Char where
    val (T2 x) = x

t1 = T1 1
t2 = T2 '3'

v1 = val t1
v2 = t2 # val


{-
    -- S -> Space* (Word (Space+ Word)* Space*)?
    S :: () -> {rngs::[(Int, Int)]}
    S:0 -> Spaces
        0.rngs := []
    S:0 -> Spaces:1 Word:2 SepWords:3 Spaces
        1.begin := 0
        2.begin := 1.end
        3.begin := 2.end
        0.rngs := (1.begin, 1.end):3.rngs
    
    -- SepWord*
    SepWords :: {begin::Int} -> {end::Int, rngs::[(Int, Int)]}
    SepWords:0 ->
        0.end := 0.begin
        0.rngs := []
    SepWords:0 -> SepWord:1 SepWords:2
        1.begin := 0.begin
        2.begin := 1.end
        0.end := 2.end
        0.rngs := (1.begin, 1.end):2.rngs
    
    SepWords :: {begin::Int} -> {end::Int}
    SepWord:0 -> Spaces1:1 Word:2
        1.begin := 0.begin
        2.begin := 1.end
        0.end := 2.end
    
    -- Spaces -> "space"*
    Spaces :: {begin::Int} -> {end::Int}
    Spaces:0 -> 
        0.end = 0.begin
    Spaces:0 -> "space" Spaces:1
        1.begin = 0.begin+1
        0.end = 1.end
    
    
    -- Word -> "nonspace"+
    Word :: {begin::Int} -> {end::Int}
    Word:0 -> "nonspace"
        0.end+1 = 0.begin
    Word:0 -> "nonspace" Word:1
        1.begin = 0.begin+1
        0.end = 1.end
    
-}





