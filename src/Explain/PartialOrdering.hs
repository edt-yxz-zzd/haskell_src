{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Explain.PartialOrdering
    ( PartialOrdering(..), TotalOrdering()
    , ord2pord, pord2may_ord
    , PartialOrdCmpResult, TotalOrdCmpResult
    )
where
import Prelude hiding (Ordering(..))
import Explain.ExplainBase
import qualified Prelude as P

type TotalOrdering = P_Ord
data PartialOrdering = NO | LT | EQ | GT
    deriving (Show, Read, Eq, Ord)
type POrd = PartialOrdering
type P_Ord = P.Ordering
ord2pord :: P_Ord -> POrd
ord2pord P.LT = LT
ord2pord P.EQ = EQ
ord2pord P.GT = GT
pord2may_ord :: POrd -> Maybe P_Ord
pord2may_ord NO = Nothing
pord2may_ord LT = Just P.LT
pord2may_ord EQ = Just P.EQ
pord2may_ord GT = Just P.GT
-- instExplain [t| POrd |] [t| P_Ord |] [e| ord2pord |]
instance Explain POrd P_Ord where
    explain = ord2pord


class Explain POrd a => PartialOrdCmpResult a
class (PartialOrdCmpResult a, Explain P_Ord a) => TotalOrdCmpResult a
instance Explain POrd a => PartialOrdCmpResult a
instance (PartialOrdCmpResult a, Explain P_Ord a) => TotalOrdCmpResult a


