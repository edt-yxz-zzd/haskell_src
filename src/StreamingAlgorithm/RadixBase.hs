
module RadixBase
    (RadixBase() --()
    ,unRadixBase
    ,unsafe_mkRadixBase
    ,maybe_mkRadixBase
    )
where

import Control.Exception(assert)

newtype RadixBase = Private_RadixBase Integer -- radix_base
        -- (radix_base, [floor; digits...])
    deriving (Show, Read, Eq, Ord)
unRadixBase :: RadixBase -> Integer
unRadixBase (Private_RadixBase radix_base) = radix_base
unsafe_mkRadixBase :: Integer -> RadixBase
maybe_mkRadixBase :: Integer -> Maybe RadixBase
unsafe_mkRadixBase radix_base
    = assert (radix_base >= 2) (Private_RadixBase radix_base)
maybe_mkRadixBase radix_base
    = if radix_base >= 2
      then Just $ Private_RadixBase radix_base
      else Nothing


