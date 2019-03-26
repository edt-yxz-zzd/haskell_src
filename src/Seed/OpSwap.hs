
module Seed.OpSwap where


import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

class OpSwap p where
    swap :: p a b -> p b a
class OpRotate p where
    rotateR :: p (p a b) c -> p a (p b c)
    rotateL :: p a (p b c) -> p (p a b) c
instance OpSwap (,) where
    swap (a,b) = (b,a)
instance OpSwap Either where
    swap (Left a) = Right a
    swap (Right a) = Left a
instance OpRotate (,) where
    rotateR ((a,b),c) = (a,(b,c))
    rotateL (a,(b,c)) = ((a,b),c)
instance OpRotate Either where
    rotateR (Left (Left a)) = Left a
    rotateR (Left (Right b)) = Right (Left b)
    rotateR (Right c) = Right (Right c)
    rotateL (Left a) = Left (Left a)
    rotateL (Right (Left b)) = Left (Right b)
    rotateL (Right (Right c)) = Right c
swapA :: (Arrow arr, OpSwap p) => arr (p a b) (p b a)
swapA = arr swap
rotateRA :: (Arrow arr, OpRotate p) => arr (p (p a b) c) (p a (p b c))
rotateRA = arr rotateR
rotateLA :: (Arrow arr, OpRotate p) => arr (p a (p b c)) (p (p a b) c)
rotateLA = arr rotateL



