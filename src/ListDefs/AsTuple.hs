{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}


module ListDefs.AsTuple
    ( module ListDefs.AsTuple
    , module ListDefs.AsTupleBase
    )
where

import ListDefs.AsTupleBase (defs__instExplain_Tuple_AsTuple, max_tuple_length)
import ListDefs.AsTupleBase

$(defs__instExplain_Tuple_AsTuple [2..4])

