{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
module Explain.Subtype
    ( module Explain.Subtype
    , module Explain.ExplainBase
    )
where

import Explain.ExplainBase

type spec <: u = Explain u spec
type Subtype spec u = Explain u spec
infix 0 <: -- like ->, but what is ~



