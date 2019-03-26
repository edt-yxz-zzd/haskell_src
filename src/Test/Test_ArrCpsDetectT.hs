{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Test_ArrCpsDetectT
where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)


import Arrows.ArrCpsDetectT
import qualified ADT.IArrowCatch as C
import ADT.IArrowCC

type Arr = ArrCpsDetectT String Char (->)

throwA_ = C.throwA_ . Just
catchA = C.catchA
unArrCpsT i2o i = case unArrCpsDetectT i2o i of
    Left (Just e) -> Left e
    Right o -> Right o


i2o :: Arr Int Integer
i2o = arr toInteger
i2o2 = catchA i2o (throwA_ "i2o2")
i2o3 = catchA i2o2 (throwA_ "i2o3")

o2r, o2r2, o2r3 :: Arr Integer Char
o2r = throwA_ "o2r"
o2r2 = catchA o2r (throwA_ "o2r2")
o2r3 = catchA o2r2 (throwA_ "o2r3")


i2r1 = catchA i2o (exitA_ '1') >>> o2r -- Left "o2r"
i2r2 = catchA i2r1 (exitA_ '2') -- Right '2'
i2r3 = catchA i2r2 (exitA_ '3') -- Right '2'
i2r4 = catchA i2o (exitA_ '4') >>> o2r2 -- Left "o2r2"
i2r5 = catchA i2o (exitA_ '5') >>> o2r3 -- Left "o2r3"
i2r6 = catchA i2o2 (exitA_ '6') >>> o2r2 -- Left "o2r2"
i2r7 = catchA i2o2 (exitA_ '7') >>> o2r3 -- Left "o2r3"
i2r8 = catchA i2o3 (exitA_ '8') >>> o2r2 -- Left "o2r2"
i2r9 = catchA i2o3 (exitA_ '9') >>> o2r3 -- Left "o2r3"

pr = do
    print $ unArrCpsT i2r1 454 == Left "o2r"
    print $ unArrCpsT i2r2 454 == Right '2'
    print $ unArrCpsT i2r3 454 == Right '2'
    print $ unArrCpsT i2r4 454 == Left "o2r2"
    print $ unArrCpsT i2r5 454 == Left "o2r3"
    print $ unArrCpsT i2r6 454 == Left "o2r2"
    print $ unArrCpsT i2r7 454 == Left "o2r3"
    print $ unArrCpsT i2r8 454 == Left "o2r2"
    print $ unArrCpsT i2r9 454 == Left "o2r3"
    {-
    -- these two fail: catchCPS ver1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    print $ unArrCpsT i2r6 454
    print $ unArrCpsT i2r7 454
    -}



