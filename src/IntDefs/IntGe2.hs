{-# LANGUAGE TemplateHaskell #-}

module IntDefs.IntGe2 (IntGe2())
where


import IntDefs.IntGeX_tpl (defs)


$(defs 2)



