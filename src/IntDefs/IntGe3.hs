{-# LANGUAGE TemplateHaskell #-}

module IntDefs.IntGe3 (IntGe3())
where


import IntDefs.IntGeX_tpl (defs)


$(defs 3)



