
{-
https://stackoverflow.com/questions/10863132/composing-floor-and-sqrt-in-haskell
-}

module FloorSqrt
    (floor_sqrt
    )
where

--import Control.Exception (assert)

floor_sqrt :: Integer -> Integer
floor_sqrt n = if (r^2 <= n && n < (r+1)^2)
                then r
                else error "floor_sqrt impl error"
    where
        r = if (0<=n) then flrt n else error "floor_sqrt (<0)"


flrt :: Integer -> Integer  -- flrt x ≈ √x,  with  flrt x^2 ≤ x < flrt(x+1)^2
flrt x = approx (round . (sqrt::Double->Double) . fromInteger $ x)
   where approx r
            | ctrl <= x, (r+1)^2 > x  = r
            | otherwise               = approx $ r - diff
          where ctrl = r^2
                diff = (ctrl - x) // (2*r)    -- ∂/∂x x² = 2x

         a//b = a`div`b + if (a>0)==(b>0) then 1 else 0   -- always away from 0


