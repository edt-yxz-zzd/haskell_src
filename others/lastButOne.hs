


lastButOne :: [a] -> a
lastButOne (x:_:[]) = x -- NOTE: "(" ")"
lastButOne ls = lastButOne . tail $ ls
-- lastButOne xs = xs !! (length xs - 2)
{-
lastButOne xs = case xs of 
    x:_:[] -> x
    xs -> lastButOne . tail $ xs
-}


