{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TemplateHaskell #-} !!!!

import Tpl hiding (main)

-- d2 ''Cls ''t  -- Name Name
-- d2 [t|Cls|] [t|t|] -- TypeQ TypeQ
class Cls a
-- d2 ''Cls ''Int
d2 [t|Cls|] [t|Int|]

$derive


{- :i Cls
class Cls a
        -- Defined at E:\my_data\program_source\haskell\try_haskell\template\User.hs:8:7
instance Cls Int
  -- Defined at E:\my_data\program_source\haskell\try_haskell\template\User.hs:10:1
--}

----- why need main??
main = do
    print ""
