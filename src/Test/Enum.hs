

module Test.Enum
    ( module Test.Enum
    , module Data.List
    )
where
--import IntDefs.IntDefs
import Numeric.Natural (Natural)
import Data.List

range :: Natural -> [Natural]
range 0 = []
range u = [0..u-1]

enumUInt :: [Natural]
enumUInt = [0..]

mulList1 :: a -> [[a]] -> [[a]]
mulList1 h = map (h:)
mulList :: [a] -> [[a]] -> [[a]]
mulList hs tss = concat $ map (flip mulList1 tss) hs
-- allWordsPerLen [0..2] = [[[]], [[0]..[2]], [[0,0]..[2,2]]..]
allWordsPerLen :: [a] -> [[[a]]]
allWordsPerLen ls = iterate (mulList ls) [[]]
-- allWords [0..2] = [[], [0]..[2], [0,0]..[2,2]..]
allWords :: [a] -> [[a]]
allWords = concat . allWordsPerLen

{-
enumUIntsMod_incLen :: [Natural] -> [[Natural]] -> [[Natural]]
enumUIntsMod_incLen modulo uss
enumUIntsMod :: Natural -> [[Natural]]
enumUIntsMod modulo = 

--}
--}
--}
--}
--}
--}
