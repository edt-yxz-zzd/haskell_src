
import Data.List (intersperse, intercalate)

-- fromIntegral = fromInteger . toInteger



-- generic null
-- null = len_lt 1
len_lt :: Integral n => n -> [a] -> Bool
len_lt n xs = n /= len first_n
    where take_n = take $ fromIntegral n
          first_n = take_n xs


{-
len_lt n _
    | n <= 0 = False
len_lt n (_:xs) = len_lt (n-1) xs
len_lt _ _ = True
-}



-- infixl version of $; like "->" ??
-- f (g a) (h b) ==>> f $> g a $> h b
($>) :: (a->b) -> a -> b
-- f $> x = f x
($>) = ($)
infixl 0 $>

len :: Integral b => [a] -> b
len = fromIntegral . length
-- len :: [a] -> Integer
-- len = toInteger . length


-- args: x --> y ...
-- data Args = Void | Args --> a
-- data Args (Args x, a) = Void | Args x >-> a

-- call :: (a->b) -> Args -> c
-- call f Void = f
-- call f (args --> x) = call args x
-- call :: (a->f) -> Maybe (a, c) -> (f-)
-- call f (a, b) = call (f a) b
-- call f () = f
-- call f (Just (a, b)) = call (f a) b
-- call f Nothing = f




join = intercalate -- join xs = concat . intersperse xs







